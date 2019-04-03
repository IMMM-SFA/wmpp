# support functions

# read_grid_ids
#
# This function gets the grid ids for a named HUC2-WM region (e.g., "10", "11", "COLO", "PNW")
read_grid_ids <- function(region){

  stopifnot(is.character(region))
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read.table(paste0(extdata_dir, "grid_id/", region, ".ll")) %>%
    .$V1

}

# read_hydro_plant_grids
#
# Gets hydroplants grid ids (and lat/lon)
read_hydro_plant_grids <- function(region){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "plexos_hydro_BA_hucwm.csv"),
           col_types = cols()) %>%
    dplyr::filter(reg_hucwm == region) %>%
    .$grid_id %>% as.character()
}

# read_hydro_plant_data
#
# Gets hydroplant data for wsgif computation
read_hydro_plant_data <- function(region){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "plexos_hydro_BA_hucwm.csv"),
           col_types = cols()) -> hydro_plants

  read_csv(paste0(extdata_dir, "BA_2_region_mapping.csv"),
           col_types = cols()) -> BA_region_mapping

  hydro_plants %>% left_join(BA_region_mapping, by = "BA") %>%
    rename(tepcc_region = `TEPCC region`, region = Regions)

}

# read_thermal_plant_data
#
#
read_thermal_plant_data <- function(){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "plantFlow_thermal.csv"),
           col_types = cols()) %>% rename(hucwm = region)
}


# get_huc4_data
#
# gets huc4 lat/lon and connects to grid ids
# this is performed offline to reduce run time in prep_flow
get_huc4_data <- function(){

  # read the huc4 data from the excel file
  extdata_dir <- system.file("extdata/", package = "wmpp")
  huc4_loc <- read_csv(paste0(extdata_dir, "HUC4list.csv"),
                       col_types = cols())

  #
  huc4_loc %>%
    mutate(huc2 = substr(HUC4, 1, 2)) %>%
    left_join(huc2_hucwm, by = "huc2") %>%
    split(.$hucwm) %>%
    purrr::map(function(x){
      # get mask file for each hucwm region
      readr::read_delim(paste0(extdata_dir, "mask_files/", x$hucwm %>% unique(), "_mask_plot.ll"),
                        delim = " ",
                        col_names = c("grid_id", "lat", "lon", "X1", "X2"),
                        col_types = cols()) %>%
        select(grid_id, lat, lon) %>%
        # convert to negative lon format
        mutate(lon = lon - 360) -> grid_id_lat_lon

      x %>%
        left_join(grid_id_lat_lon,
                  by = c("Lat" = "lat", "Long" = "lon")) %>%
        select(huc4 = HUC4, huc4_name = NAME, states = STATES,
               lat = Lat, lon = Long, huc2, huc2_name, hucwm, grid_id)


    }) %>%
    bind_rows()
}


# get_wsgif_all_dams
#
# calculates the hydro wsgif for each dam
get_wsgif_all_dams <- function(hydro_flow_his,
                               hydro_flow_fut){

  # get the historical period average flow
  hydro_flow_his %>%
    summarise_if(is.numeric, mean) %>%
    gather(dam, mean_flow_hist) -> hydro_flow_mean_hist

  # get the future period annual flows
  hydro_flow_fut %>%
    group_by(year = year(date)) %>%
    summarise_if(is.numeric, mean) %>%
    gather(dam, mean_flow, -year) -> hydro_flow_mean

  # compute wsgif at dam level
  hydro_flow_mean %>%
    left_join(hydro_flow_mean_hist, by = "dam") %>%
    mutate(wsgif = mean_flow / mean_flow_hist) %>%
    select(year, dam, wsgif)
}

# get_wsgif_all_huc4s
#
# calculates the thermal wsgif for each huc4
get_wsgif_all_huc4s <- function(thermal_flow_his,
                                thermal_flow_fut){

  # get the historical period average flow
  thermal_flow_his %>%
    summarise_if(is.numeric, mean) %>%
    gather(huc4_outlet, mean_flow_hist) -> thermal_flow_mean_hist

  # get the future period annual flows
  thermal_flow_fut %>%
    group_by(year = year(date)) %>%
    summarise_if(is.numeric, mean) %>%
    gather(huc4_outlet, mean_flow, -year) -> thermal_flow_mean

  # compute wsgif at huc4 level
  thermal_flow_mean %>%
    left_join(thermal_flow_mean_hist, by = "huc4_outlet") %>%
    rowwise() %>%
    mutate(wsgif = min(1, mean_flow / mean_flow_hist)) %>%
    ungroup() %>%
    select(year, huc4_outlet, wsgif)
}




# get_regions_by_huc4
#
# returns regions/huc4 table
get_regions_by_huc4 <- function(){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_tsv(paste0(extdata_dir, "HUC42region.txt"),
                  col_names = c("huc4", "region_number"),
                  col_types = readr::cols()) %>%
    mutate(region = case_when(
      region_number == 1 ~ "Northwest",
      region_number == 2 ~ "NCalifornia",
      region_number == 3 ~ "SCalifornia",
      region_number == 4 ~ "Basin",
      region_number == 5 ~ "Southwest",
      region_number == 6 ~ "Rockies"
    )) %>% select(-region_number)
}




