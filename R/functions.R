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
           col_types = cols())
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

