#' get_wsgif
#'
#' @param flow_fn_1 file naming format for the hydro_flow_file, excluding the region (e.g., 10_HIST_MOSART.csv, would be "HIST_MOSART.csv")
#' @param flow_fn_2 optional second flow file naming format in case hist and future parts are split.
#' @param flow_file_dir directory where hydro flow files are stored
#' @param output_fn_tag naming convention for output files
#' @param output_file_dir directory for output. If blank, new directory is made inside hydro_flow_file_dir.
#' @param by spatial unit ("all", "bal_auth", "region", "HUCWM", "WECC")
#' @param historical_period start and end dates of historical period in format yyyy-mm-dd
#' @param future_period start and end dates of future period in format yyyy-mm-dd
#' @param dam_cutoff_percentile cutoff for which dams to include in the wsgif computation. The percentile is based on nameplate capacity (i.e., percentile of 0.5 will include the largest dams that contribute 50 percent total nameplate).
#' @details computes WSGIF for hydropower by balancing authority, region, HUC2, or entire WECC
#' @return time series of WSGIF
#' @importFrom readr read_csv read_tsv cols
#' @importFrom lubridate ymd year
#' @importFrom tidyr gather spread separate
#' @importFrom stringr str_split
#' @importFrom dplyr select arrange left_join bind_rows mutate summarise group_by ungroup filter pull if_else summarise_if case_when rowwise
#' @importFrom tibble tibble
#' @importFrom purrr map map2
#' @export
#'
get_wsgif <- function(flow_fn_1,
                      flow_fn_2,
                      flow_file_dir,
                      output_fn_tag = "",
                      output_file_dir,
                      historical_period = c("1955-01-01", "2004-12-31"),
                      future_period = c("2005-01-01", "2095-12-31"),
                      by = "all",
                      dam_cutoff_percentile = 1.0){


  # stop routine for incorrect function arguments
  stopifnot(dam_cutoff_percentile > 0,
            dam_cutoff_percentile <= 1,
            length(historical_period) == 2,
            length(future_period) == 2)

  # read in dam file and filter for largest (according to chosen dam_cutoff_percentile)
  read_hydro_plant_data() %>%
    select(plant, MWh = annualProduction_MWh_, BA, reg_hucwm, grid_id, power_area) %>%
    filter(reg_hucwm %in% huc2_hucwm$hucwm) %>%
    arrange(-MWh) %>%
    mutate(percentile = cumsum(-MWh) / sum(-MWh),
           grid_id = as.character(grid_id)) %>%
    filter(percentile <= dam_cutoff_percentile) %>% select(-percentile) ->
    hydro_plants

  # read in thermal plant data
  read_thermal_plant_data() %>%
    filter(coolingType == "Surface Water") %>%
    left_join(get_power_areas_by_huc4(), by = "huc4") %>%
    left_join(get_huc4_data() %>% select(huc4, grid_id), by = "huc4") %>%
    select(MWh = annualMWh, huc4, grid_id, power_area) ->
    thermal_nameplate

  # get date sequences
  date_seq_his <- seq(ymd(historical_period[1]),
                      ymd(historical_period[2]),
                      by = "days")

  date_seq_fut <- seq(ymd(future_period[1]),
                      ymd(future_period[2]),
                      by = "days")

  if(missing(flow_fn_2)){ # for when flow input is included in a single simulation file

    list.files(flow_file_dir) %>%
      .[grepl(flow_fn_1, .)] -> flow_files

    flow_files %>% .[grepl("hydro", .)] -> hydro_flow_files
    flow_files %>% .[grepl("huc4", .)] -> thermal_flow_files


    str_split(hydro_flow_files, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_hydro

    str_split(hydro_flow_files, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_thermal

    stopifnot(all(regions_detected_hydro == regions_detected_thermal))

    message(paste0(c("Regions detected:", regions_detected_hydro), collapse = " "))

    # set up function for pulling flow file, checking the data, and performing wgif computation
    flow_to_wsgif <- function(flow_file){

      # read flows
      flow <- read_csv(paste0(flow_file_dir, "/", flow_file),
                       col_types = cols())

      # return shell for files without hydro flow
      if(ncol(flow) == 1) return(tibble(year = as.double(),
                                        grid_id = as.character(),
                                        wsgif = as.double()))

      # ensure flow file contains representative dates for historical and future periods:
      if(!all(date_seq_his %in% flow$date)) stop(paste0(flow_file, " lacks some dates of the historical period"))
      if(!all(date_seq_fut %in% flow$date)) stop(paste0(flow_file, " lacks some dates of the future period"))

      # get historical hydro flows
      flow %>%
        filter(date %in% date_seq_his) ->
        flow_his

      # get future hydro flows
      flow %>%
        filter(date %in% date_seq_fut) ->
        flow_fut

      # get the wsgif
      get_wsgif_all_grids(flow_his, flow_fut)

    }

    # get hydro wsgif
    hydro_flow_files %>% map(flow_to_wsgif) %>% bind_rows() -> wsgif_all_dams

    # get thermal wsgif
    thermal_flow_files %>% map(flow_to_wsgif) %>% bind_rows() %>%
      mutate(wsgif = if_else(wsgif > 1.0, 1.0, wsgif)) ->
      wsgif_all_huc4s

  }else{ # for when inputs are split across two separate files...

    # get filenames for historical and future period results
    list.files(flow_file_dir) %>%
      .[grepl(flow_fn_1, .)] -> flow_files_his

    list.files(flow_file_dir) %>%
      .[grepl(flow_fn_2, .)] -> flow_files_fut

    flow_files_his %>% .[grepl("hydro", .)] -> hydro_flow_files_his
    flow_files_fut %>% .[grepl("hydro", .)] -> hydro_flow_files_fut
    flow_files_his %>% .[grepl("huc4", .)] -> thermal_flow_files_his
    flow_files_fut %>% .[grepl("huc4", .)] -> thermal_flow_files_fut

    str_split(hydro_flow_files_his, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_his_hyd

    str_split(hydro_flow_files_fut, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_fut_hyd

    str_split(thermal_flow_files_his, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_his_thr

    str_split(thermal_flow_files_fut, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected_fut_thr

    if(!all(regions_detected_his_hyd == regions_detected_fut_hyd)){
      stop(paste0(c("Region inconsistency for hydro... Historical regions:", regions_detected_his_hyd,
                    "  Future regions:", regions_detected_fut_hyd), collapse = " "))
    }

    if(!all(regions_detected_his_thr == regions_detected_fut_thr)){
      stop(paste0(c("Region inconsistency for thermal... Historical regions:", regions_detected_his_thr,
                    "  Future regions:", regions_detected_fut_thr), collapse = " "))
    }

    if(anyDuplicated(regions_detected_his_hyd) > 0){
      stop(paste0(c("Some regions (hydro) have more than one associated file... ",
                    anyDuplicated(regions_detected_his_hyd)), collapse = " "))
    }

    if(anyDuplicated(regions_detected_his_thr) > 0){
      stop(paste0(c("Some regions (thermal) have more than one associated file... ",
                    anyDuplicated(regions_detected_his_thr)), collapse = " "))
    }

    # ensure consistency in hucwm regions
    stopifnot(all(regions_detected_his_hyd == regions_detected_his_thr))
    message(paste0(c("hucwm regions detected in files:", regions_detected_his_hyd), collapse = " "))


    # set up function for pulling flow file, checking the data, and performing wgif computation
    flow_to_wsgif <- function(hucwm, hyd_huc){
      # get historical hydro flows
      read_csv(paste0(flow_file_dir, "/",
                      hyd_huc ,"_", hucwm, "_",
                      flow_fn_1),
               col_types = cols()) %>%
        filter(date %in% date_seq_his) ->
        flow_his

      # get future hydro flows
      read_csv(paste0(flow_file_dir, "/",
                      hyd_huc, "_", hucwm, "_",
                      flow_fn_2),
               col_types = cols()) %>%
        filter(date %in% date_seq_fut) ->
        flow_fut

      # return shell for files without hydro flow
      if(ncol(flow_fut) == 1 | ncol(flow_fut) == 1){
        return(tibble(year = as.double(),
                      grid_id = as.character(),
                      wsgif = as.double()))
      }

      # ensure flow file contains representative dates for historical and future periods:
      if(!all(date_seq_his %in% flow_his$date)) stop(paste0(hucwm, " ", hyd_huc, " file lacks some dates of the historical period"))
      if(!all(date_seq_fut %in% flow_fut$date)) stop(paste0(hucwm, " ", hyd_huc, " lacks some dates of the future period"))

      # return the (non-weighted) wsgif for all hydro dams
      get_wsgif_all_grids(flow_his, flow_fut)
    }

    regions_detected_his_hyd %>%
      map2("hydro", flow_to_wsgif) %>%
      bind_rows() -> wsgif_all_dams

    regions_detected_his_thr %>%
      map2("huc4", flow_to_wsgif) %>%
      bind_rows() %>%
      mutate(wsgif = if_else(wsgif > 1.0, 1.0, wsgif)) ->
      wsgif_all_huc4s

  }

  # bring in plant data for balancing authority, region.

  pull(wsgif_all_dams, grid_id) %>% unique() -> wsgif_dam_grids
  pull(hydro_plants, grid_id) -> desired_dam_grids

  if(all(desired_dam_grids %in% wsgif_dam_grids)){
    message("WSGIF is available for all desired hydropower plants!")
    }else{
      warning(paste0(c("Plants missing from WSGIF data:",
                       desired_dam_grids[!desired_dam_grids %in% wsgif_dam_grids]),
                     collapse = " "))
    }

  pull(wsgif_all_huc4s, grid_id) %>% unique() -> wsgif_huc4_grids
  pull(thermal_nameplate, grid_id) %>% unique() -> desired_huc4_grids

  if(all(desired_huc4_grids %in% wsgif_huc4_grids)){
    message("WSGIF is available for all desired HUC4 catchments!")
  }else{
    warning(paste0(c("HUC4 catchments missing from WSGIF data:",
                     desired_huc4_grids[!desired_huc4_grids %in% wsgif_huc4_grids]),
                   collapse = " "))
  }

  wsgif_all_dams %>%
    filter(is.na(wsgif)) %>% pull(grid_id) %>% unique() ->
    dam_grids_with_bad_flow_data

  wsgif_all_huc4s %>%
    filter(is.na(wsgif)) %>% pull(grid_id) %>% unique() ->
    huc4_grids_with_bad_flow_data

  # add balancing authority and region weights for hydro
  hydro_plants %>%
    filter(!grid_id %in% dam_grids_with_bad_flow_data) %>%
    group_by(BA) %>%
    mutate(BA_weighting = MWh / sum(MWh)) %>%
    group_by(power_area) %>%
    mutate(power_area_weighting = MWh / sum(MWh)) %>%
    ungroup() %>%
    mutate(wecc_weighting = MWh / sum(MWh)) ->
    hydro_plants_weighted

  # add region weights for thermal
  thermal_nameplate %>%
    filter(!grid_id %in% huc4_grids_with_bad_flow_data) %>%
    group_by(power_area) %>%
    mutate(power_area_weighting = MWh / sum(MWh)) %>%
    ungroup() %>%
    mutate(wecc_weighting = MWh / sum(MWh)) %>%
    mutate(grid_id = as.character(grid_id)) ->
    thermal_plants_weighted

  wsgif_all_dams %>%
    left_join(hydro_plants_weighted, by = c("grid_id")) %>%
    filter(!is.na(MWh)) ->
    wsgif_all_dams_BA_region

  wsgif_all_huc4s %>%
    left_join(thermal_plants_weighted, by = c("grid_id")) %>%
    filter(!is.na(MWh)) ->
    wsgif_all_huc4_region

  # get hydro wsgif by balancing authority
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * BA_weighting) %>%
    group_by(year, BA) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(BA, wsgif) -> wsgif_hydro_BA

  # get hydro wsgif by power area
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * power_area_weighting) %>%
    group_by(year, power_area) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(power_area, wsgif) -> wsgif_hydro_power_area

  # get hydro wsgif by wecc region
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * wecc_weighting) %>%
    group_by(year) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    rename(wecc = wsgif) -> wsgif_hydro_wecc

  # get thermal wsgif by power area
  wsgif_all_huc4_region %>%
    mutate(wsgif_weighted = wsgif * power_area_weighting) %>%
    group_by(year, power_area) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(power_area, wsgif) -> wsgif_thermal_power_area

  # get thermal wsgif by wecc
  wsgif_all_huc4_region %>%
    mutate(wsgif_weighted = wsgif * wecc_weighting) %>%
    group_by(year) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    rename(wecc = wsgif) -> wsgif_thermal_wecc

  # write to file
  if(missing(output_file_dir)){
    output_file_dir <- paste0(flow_file_dir, "/wsgif")
    dir.create(output_file_dir)
  }

  write_csv(wsgif_hydro_BA, paste0(output_file_dir, "/wsgif_hydro_BA_", output_fn_tag, ".csv"))
  write_csv(wsgif_hydro_power_area, paste0(output_file_dir, "/wsgif_hydro_power_area_", output_fn_tag, ".csv"))
  write_csv(wsgif_hydro_wecc, paste0(output_file_dir, "/wsgif_hydro_wecc_", output_fn_tag, ".csv"))

  write_csv(wsgif_thermal_power_area, paste0(output_file_dir, "/wsgif_thermal_power_area_", output_fn_tag, ".csv"))
  write_csv(wsgif_thermal_wecc, paste0(output_file_dir, "/wsgif_thermal_wecc_", output_fn_tag, ".csv"))

}
