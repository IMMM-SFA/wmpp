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
                      dam_cutoff_percentile = 0.92){


  # stop routine for incorrect function arguments
  stopifnot(dam_cutoff_percentile > 0,
            dam_cutoff_percentile <= 1,
            length(historical_period) == 2,
            length(future_period) == 2)

  # read in dam file and filter for largest (according to chosen dam_cutoff_percentile)
  read_hydro_plant_data() %>%
    select(plant, nameplate_MW_, BA, reg_hucwm, grid_id, region) %>%
    mutate(nameplate_MW_ = if_else(nameplate_MW_ < 0, 0, nameplate_MW_)) %>%
    filter(reg_hucwm %in% huc2_hucwm$hucwm) %>%
    arrange(-nameplate_MW_) %>%
    mutate(percentile = cumsum(nameplate_MW_) / sum(nameplate_MW_),
           grid_id = as.character(grid_id)) %>%
    filter(percentile <= dam_cutoff_percentile) %>% select(-percentile) ->
    hydro_plants

  # read in thermal plant data
  read_thermal_plant_data() %>%
    filter(coolingType == "Surface Water") %>%
    left_join(get_regions_by_huc4(), by = "huc4") %>%
    left_join(get_huc4_data() %>% select(huc4, grid_id), by = "huc4") %>%
    select(nameplate = namePlate, huc4, grid_id, region) ->
    thermal_nameplate

  # get date sequences
  date_seq_his <- seq(ymd(historical_period[1]),
                      ymd(historical_period[2]),
                      by = "days")

  date_seq_fut <- seq(ymd(future_period[1]),
                      ymd(future_period[2]),
                      by = "days")

  if(missing(flow_fn_2)){

    list.files(flow_file_dir) %>%
      .[grepl(flow_fn_1, .)] -> flow_files

    flow_files %>% .[grepl("hydro", .)] -> hydro_flow_files
    flow_files %>% .[grepl("huc4", .)] -> thermal_flow_files


    str_split(hydro_flow_files, "_") %>%
      map(~.[[2]]) %>% unlist() -> regions_detected
    message(paste0(c("Regions detected in hydro files:", regions_detected), collapse = " "))

    hydro_flow_files %>%
      # map through all regions and calculate dam-level wsgif
      map(function(hff){

        # read hydro flows
        hydro_flow <- read_csv(paste0(flow_file_dir, "/", hff),
                               col_types = cols())

        # return shell for files without hydro flow
        if(ncol(hydro_flow) == 1) return(tibble(year = as.double(),
                                                dam = as.character(),
                                                wsgif = as.double()))

        # ensure flow file contains representative dates for historical and future periods:
        if(!all(date_seq_his %in% hydro_flow$date)) stop("hydro_flow_file lacks some dates of the historical period")
        if(!all(date_seq_fut %in% hydro_flow$date)) stop("hydro_flow_file lacks some dates of the future period")

        # get historical hydro flows
        hydro_flow %>%
          filter(date %in% date_seq_his) ->
          hydro_flow_his

        # get future hydro flows
        hydro_flow %>%
          filter(date %in% date_seq_fut) ->
          hydro_flow_fut

        get_wsgif_all_dams(hydro_flow_his, hydro_flow_fut)

      }) -> wsgif_all_dams_and_regions

  }else{

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

    regions_detected_his_hyd %>%
      map(function(hucwm){

        # get historical hydro flows
        read_csv(paste0(flow_file_dir,
                        "/hydro_", hucwm, "_",
                        flow_fn_1),
                 col_types = cols()) %>%
          filter(date %in% date_seq_his) ->
          hydro_flow_his

        # get future hydro flows
        read_csv(paste0(flow_file_dir,
                        "/hydro_", hucwm, "_",
                        flow_fn_2),
                 col_types = cols()) %>%
          filter(date %in% date_seq_fut) ->
          hydro_flow_fut

        # return shell for files without hydro flow
        if(ncol(hydro_flow_fut) == 1 | ncol(hydro_flow_fut) == 1){
          return(tibble(year = as.double(),
                        dam = as.character(),
                        wsgif = as.double()))
        }

        # ensure flow file contains representative dates for historical and future periods:
        if(!all(date_seq_his %in% hydro_flow_his$date)) stop("hydro_flow_file_1 lacks some dates of the historical period")
        if(!all(date_seq_fut %in% hydro_flow_fut$date)) stop("hydro_flow_file_2 lacks some dates of the future period")

        # return the (non-weighted) wsgif for all hydro dams
        get_wsgif_all_dams(hydro_flow_his, hydro_flow_fut)

      }) %>% bind_rows() -> wsgif_all_dams

    regions_detected_his_thr %>%
      map(function(hucwm){

        # get historical hydro flows
        read_csv(paste0(flow_file_dir,
                        "/huc4_", hucwm, "_",
                        flow_fn_1),
                 col_types = cols()) %>%
          filter(date %in% date_seq_his) ->
          thermal_flow_his

        # get future hydro flows
        read_csv(paste0(flow_file_dir,
                        "/huc4_", hucwm, "_",
                        flow_fn_2),
                 col_types = cols()) %>%
          filter(date %in% date_seq_fut) ->
          thermal_flow_fut

        # return shell for files without hydro flow
        if(ncol(thermal_flow_fut) == 1 | ncol(thermal_flow_fut) == 1){
          return(tibble(year = as.double(),
                        dam = as.character(),
                        wsgif = as.double()))
        }

        # ensure flow file contains representative dates for historical and future periods:
        if(!all(date_seq_his %in% thermal_flow_his$date)) stop("flow_file_1 lacks some dates of the historical period")
        if(!all(date_seq_fut %in% thermal_flow_fut$date)) stop("flow_file_2 lacks some dates of the future period")

        # return the (non-weighted) wsgif for all huc4
        get_wsgif_all_huc4s(thermal_flow_his, thermal_flow_fut)

      }) %>% bind_rows() -> wsgif_all_huc4s
  }

  # bring in plant data for balancing authority, region.

  pull(wsgif_all_dams, dam) %>% unique() -> wsgif_dams
  pull(hydro_plants, grid_id) -> desired_dams

  if(all(desired_dams %in% wsgif_dams)){
    message("WSGIF is available for all desired hydropower plants!")
    }else{
      warning(paste0(c("Plants missing from WSGIF data:",
                       desired_dams[!desired_dams %in% wsgif_dams]),
                     collapse = " "))
    }

  pull(wsgif_all_huc4s, huc4_outlet) %>% unique() -> wsgif_huc4s
  pull(thermal_nameplate, grid_id) %>% unique() -> desired_huc4s

  if(all(desired_huc4s %in% wsgif_huc4s)){
    message("WSGIF is available for all desired HUC4 catchments!")
  }else{
    warning(paste0(c("HUC4 catchments missing from WSGIF data:",
                     desired_huc4s[!desired_huc4s %in% wsgif_huc4s]),
                   collapse = " "))
  }

  wsgif_all_dams %>%
    filter(is.na(wsgif)) %>% pull(dam) %>% unique() ->
    dams_with_bad_flow_data

  wsgif_all_huc4s %>%
    filter(is.na(wsgif)) %>% pull(huc4_outlet) %>% unique() ->
    huc4s_with_bad_flow_data

  # add balancing authority and region weights for hydro
  hydro_plants %>%
    filter(!grid_id %in% dams_with_bad_flow_data) %>%
    group_by(BA) %>%
    mutate(BA_weighting = nameplate_MW_ / sum(nameplate_MW_)) %>%
    group_by(region) %>%
    mutate(region_weighting = nameplate_MW_ / sum(nameplate_MW_)) %>%
    ungroup() %>%
    mutate(wecc_weighting = nameplate_MW_ / sum(nameplate_MW_)) ->
    hydro_plants_weighted

  # add region weights for thermal
  thermal_nameplate %>%
    filter(!grid_id %in% huc4s_with_bad_flow_data) %>%
    group_by(region) %>%
    mutate(region_weighting = nameplate / sum(nameplate)) %>%
    ungroup() %>%
    mutate(wecc_weighting = nameplate / sum(nameplate)) %>%
    mutate(grid_id = as.character(grid_id)) ->
    thermal_plants_weighted

  wsgif_all_dams %>%
    left_join(hydro_plants_weighted, by = c("dam" = "grid_id")) %>%
    filter(!is.na(nameplate_MW_)) ->
    wsgif_all_dams_BA_region

  wsgif_all_huc4s %>%
    left_join(thermal_plants_weighted, by = c("huc4_outlet" = "grid_id")) %>%
    filter(!is.na(nameplate)) ->
    wsgif_all_huc4_region

  # get hydro wsgif by balancing authority
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * BA_weighting) %>%
    group_by(year, BA) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(BA, wsgif) -> wsgif_hydro_BA

  # get hydro wsgif by region
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * region_weighting) %>%
    group_by(year, region) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(region, wsgif) -> wsgif_hydro_region

  # get hydro wsgif by wecc region
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * wecc_weighting) %>%
    group_by(year) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    rename(wecc = wsgif) -> wsgif_hydro_wecc

  # get thermal wsgif by region
  wsgif_all_huc4_region %>%
    mutate(wsgif_weighted = wsgif * region_weighting) %>%
    group_by(year, region) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 3)) %>% ungroup() %>%
    spread(region, wsgif) -> wsgif_thermal_region

  # get thermal wsgif by wecc region
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
  write_csv(wsgif_hydro_region, paste0(output_file_dir, "/wsgif_hydro_region_", output_fn_tag, ".csv"))
  write_csv(wsgif_hydro_wecc, paste0(output_file_dir, "/wsgif_hydro_wecc_", output_fn_tag, ".csv"))

  write_csv(wsgif_thermal_region, paste0(output_file_dir, "/wsgif_thermal_region_", output_fn_tag, ".csv"))
  write_csv(wsgif_thermal_wecc, paste0(output_file_dir, "/wsgif_thermal_wecc_", output_fn_tag, ".csv"))

}
