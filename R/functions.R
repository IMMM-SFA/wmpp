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
  read_csv(paste0(extdata_dir, "plexos_hydro_BA_hucwm.csv")) %>%
    dplyr::filter(reg_hucwm == region) %>%
    .$grid_id %>% as.character()
}



