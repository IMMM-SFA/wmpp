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

  # read_csv(paste0(extdata_dir, "BA_2_region_mapping.csv"),
  #          col_types = cols()) -> BA_region_mapping

  read_csv(paste0(extdata_dir, "plexos_hydro_power_areas.csv"),
           col_types = cols()) -> power_areas

  hydro_plants %>% left_join(power_areas, by = "plant")
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


# get_wsgif_all_grids
#
# calculates the thermal wsgif for each huc4
get_wsgif_all_grids <- function(flow_his,
                                flow_fut){

  # get the historical period average flow
  flow_his %>%
    summarise_if(is.numeric, mean) %>%
    gather(grid_id, mean_flow_hist) -> flow_mean_hist

  # get the future period annual flows
  flow_fut %>%
    group_by(year = year(date)) %>%
    summarise_if(is.numeric, mean) %>%
    gather(grid_id, mean_flow, -year) -> flow_mean

  # compute wsgif at grid level
  flow_mean %>%
    left_join(flow_mean_hist, by = "grid_id") %>%
    mutate(wsgif = mean_flow / mean_flow_hist) %>%
    ungroup() %>%
    select(year, grid_id, wsgif)
}

# get_power_areas_by_huc4
#
# returns regions/huc4 table
get_power_areas_by_huc4 <- function(){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_tsv(paste0(extdata_dir, "HUC42region.txt"),
                  col_names = c("huc4", "region_number"),
                  col_types = readr::cols()) %>%
    mutate(power_area= case_when(
      region_number == 1 ~ "Northwest",
      region_number == 2 ~ "Northern_California",
      region_number == 3 ~ "Southern_California",
      region_number == 4 ~ "Basin",
      region_number == 5 ~ "Southwest",
      region_number == 6 ~ "Rockies"
    )) %>% select(-region_number)
}

# read_WECC_plant_data
#
# reads WECC resources and selects required columns
read_WECC_plant_data <- function(){
  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "WECC_resources_2024CC-V1.5.csv"),
           comment = "##",
           col_types = cols()) %>%
    select(teppc_id = `Ref ID`,
           teppc_shortname = `Plant/Unit (Short)`,
           #teppc_longname = `Plant/Unit (Long)`,
           en_annual_MWh = `Annual Energy (MWh)`,
           np_MWh = `Nameplate (MW)`,
           type = `Generator Type`,
           #teppc_plantcode = PLANT_CODE,
           lat = `Latitude(Red=Confidential)`,
           lon = `Longitude(Red=Confidential)`) %>%
    mutate(lat = as.numeric(lat), lon = as.numeric(lon))
}

# get_plexos_unit_coords
#
# reads plexos plant names and matches with WECC plant data to get locations
get_plexos_unit_coords <- function(){

  read_WECC_plant_data() -> WECC_plant_data
  extdata_dir <- system.file("extdata/", package = "wmpp")

  c("hydro", "fixedhydro", "thermal") %>%
    map(function(x){

      read_csv(paste0(extdata_dir, "plexos_", x, "_units.csv"), comment = "#",
               col_types = cols(name = "c")) -> plexos_units

      plexos_units %>%
        left_join(WECC_plant_data,
                  by = c("name" = "teppc_shortname")) ->
        plexos_units_latlon

      # manual fix for name mismatches in the case of thermal
      if(x == "thermal"){
        plexos_units_latlon %>% filter(is.na(teppc_id))

        get_loc <- function(missing_name){
          WECC_plant_data %>%
          filter(grepl(missing_name, teppc_shortname)) %>%
            select(lat, lon) %>% unique()
        }

        missed <- function(plxname){
          which(grepl(plxname, plexos_units_latlon$name) &
                  is.na(plexos_units_latlon$teppc_id))
        }

        plexos_units_latlon[missed("Ormesa"), "lat"] <- get_loc("Ormesa")$lat
        plexos_units_latlon[missed("Ormesa"), "lon"] <- get_loc("Ormesa")$lon
        plexos_units_latlon[missed("FourCorners"), "lat"] <- get_loc("FourCorners")$lat
        plexos_units_latlon[missed("FourCorners"), "lon"] <- get_loc("FourCorners")$lon
        plexos_units_latlon[missed("Stillwater"), "lat"] <- get_loc("Stillwater")$lat
        plexos_units_latlon[missed("Stillwater"), "lon"] <- get_loc("Stillwater")$lon
        plexos_units_latlon[missed("AESO"), "lat"] <- get_loc("AESO")$lat
        plexos_units_latlon[missed("AESO"), "lon"] <- get_loc("AESO")$lon

        }

      subset(plexos_units_latlon, is.na(lat) | is.na(lon)) -> exc_loc
      message(nrow(exc_loc), " of ", nrow(plexos_units_latlon), " ", x,
              " plants missing lat/lon... (",
              round(100 * sum(exc_loc$np_MWh, na.rm = T) / sum(plexos_units_latlon$np_MWh, na.rm = T), 2),
              "% of nameplate missing)")

      plexos_units_latlon %>% mutate(plexos_type = x)

    }) %>% bind_rows()

}


# get_huc4_for_plexos_units
#
# gets huc4 code for plexos units with lat/lon
get_huc4_for_plexos_units <- function(huc4_shape_dir){

  # check if huc4 exists--download if not
  #if (!"nhd_huc4.gpkg.zip" %in% list.files(huc4_shape_dir)) get_huc4(template = NULL, huc4_shape_dir)

  # unzip and load huc4 spatial data
  tempdir <- tempfile()
  utils::unzip(paste0(huc4_shape_dir, "/nhd_huc4.gpkg.zip"), exdir = tempdir)
  st_read(stringr::str_c(tempdir, "/data-raw/nhd_huc4.gpkg"),
              quiet = TRUE) %>% as("Spatial") -> huc4

  get_plexos_unit_coords() -> plexos_units

  # transform plexos units to spatial object
  plexos_units %>%
    filter(!is.na(lat) & !is.na(lon)) %>%
    select(name, lat, lon) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = "+proj=longlat +datum=WGS84") -> plexos_units_points


  # transform to US coordinate system
  st_transform(plexos_units_points, 2163) -> plexos_units_points_
  st_as_sf(huc4) %>% st_transform(2163) -> huc4_

  # get huc4 for each unit
  plexos_units_points_$huc4 <- apply(st_intersects(huc4_, plexos_units_points_, sparse = FALSE), 2,
                                    function(col) {
                                      which(col) -> x
                                      if(!length(x) == 1) return(NA_character_)
                                      as.character(huc4_[x, ]$HUC4)
                                    })

  rm(huc4)

  # join huc4 back into main plexos unit tibble
  plexos_units %>%
    left_join(
      tibble(name = plexos_units_points_$name,
             huc4 = plexos_units_points_$huc4),
      by = "name"
    )
}



# read_plexos_hydro_units
#
# Reads the static plexos hydro input
read_plexos_hydro_units <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "NREL/monthlyMaxEnergy_hydro.csv"),
           col_types = cols()) %>%
    select(one_of(c("name", paste0("M", 1:12))))
}

# read_plexos_hydro_units_max
#
# max constraint for hydro
read_plexos_hydro_units_max <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "NREL/monthly_MaxEnergy_hydro_fromMaxCap.csv"),
           col_types = cols()) %>%
    select(one_of(c("name", paste0("M", 1:12))))
}

# read_plexos_hydro_units_min
#
# min constraint for hydro
read_plexos_hydro_units_min <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "NREL/monthly_MinEnergy_hydro_fromMinStable.csv"),
           col_types = cols()) %>%
    select(one_of(c("name", paste0("M", 1:12))))
}

# get_hydro_fixed_dispatch
#
# get baseline hourly Mwh for fixed dispatch
get_hydro_fixed_dispatch <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")

  hd_dir <- paste0(extdata_dir, "NREL/fixed_dispatch_hydro_files/")

  list.files(hd_dir, recursive = T) %>%
    .[grepl("HY_", .)] %>%
    map(function(x){
      read_csv(paste0(hd_dir, "/", x), col_types = cols()) %>%
        mutate(file = x)
    }) %>%
    bind_rows() %>% mutate(DATETIME = gsub("/", "-", DATETIME)) %>%
    mutate(DATETIME = mdy_hm(DATETIME)) %>%
    arrange(file, DATETIME) -> hydro_files


  # get mapping file
  read_csv(paste0(hd_dir, "mapping.csv"), col_types = cols()) %>%
    rename(file = `Data File`) %>%
    mutate(file = paste0("FixedDispatchHydroFiles/", file),
           Multiplier = as.numeric(substr(Multiplier, 11, nchar(Multiplier)))) ->
    mapping

  # merge data
  mapping %>% full_join(hydro_files, by = "file") %>%
    select(-file) %>%
    mutate(MWh = Multiplier * Value) %>%
    select(-Multiplier, -Value)
}


# read_plexos_fixed_hydro_units_max
#
# max constraint for hydro
read_plexos_fixed_hydro_units_max <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "NREL/monthlyCap_hydro.csv"),
           col_types = cols()) %>%
    select(one_of(c("name", paste0("M", 1:12)))) %>%
    gather(Month, max, -name) %>%
    mutate(Month = as.integer(substr(Month, 2, nchar(Month))))
}

# read_plexos_fixed_hydro_units_min
#
# min constraint for hydro
read_plexos_fixed_hydro_units_min <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  read_csv(paste0(extdata_dir, "NREL/monthlyMinStable_hydro.csv"),
           col_types = cols()) %>%
    select(one_of(c("name", paste0("M", 1:12)))) %>%
    gather(Month, min, -name) %>%
    mutate(Month = as.integer(substr(Month, 2, nchar(Month))))
}


# read_plexos_thermal_max_min
#
# read plexos thermal
read_plexos_thermal_max_min <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")

  read_csv(paste0(extdata_dir, "NREL/thermalGen_maxCap_minGen.csv"),
           col_types = cols()) %>%
    select(name, max = `Max Capacity`, min = MinStable) ->
    thermal_constraints

  read_csv(paste0(extdata_dir, "plantFlow_thermal.csv"),
           col_types = cols()) %>%
    filter(!coolingTechnology == "Dry Cooled") %>%
    filter(coolingType == "Surface Water") %>%
    select(name, huc4) %>%
    full_join(thermal_constraints, by = "name") %>%
    mutate(huc4 = as.character(huc4))
}

# read_plexos_hydro_grid_ids
#
# read grid ids for plexos thermal plants
read_plexos_hydro_grid_ids <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")

  read_csv(paste0(extdata_dir, "plexos_hydro_grid_ids.csv"),
           col_types = cols())
}

