#' get_wsgif_hydro
#'
#' @param hydro_flow_fn_1 file naming format for the hydro_flow_file, excluding the region (e.g., 10_HIST_MOSART.csv, would be "HIST_MOSART.csv")
#' @param hydro_flow_fn_2 optional second flow file naming format in case hist and future parts are split.
#' @param hydro_flow_file_dir directory where hydro flow files are stored
#' @param output_fn_tag naming convention for output files
#' @param output_file_dir directory for output. If blank, new directory is made inside hydro_flow_file_dir.
#' @param by spatial unit ("all", "bal_auth", "region", "HUC2", "WECC")
#' @param historical_period start and end dates of historical period in format yyyy-mm-dd
#' @param future_period start and end dates of future period in format yyyy-mm-dd
#' @param dam_cutoff_percentile cutoff for which dams to include in the wsgif computation. The percentile is based on nameplate capacity (i.e., percentile of 0.5 will include the largest dams that contribute 50 percent total nameplate).
#' @details computes WSGIF for hydropower by balancing authority, region, HUC2, or entire WECC
#' @return time series of WSGIF
#' @importFrom readr read_csv cols
#' @importFrom lubridate ymd year
#' @importFrom tidyr gather spread separate
#' @importFrom stringr str_split
#' @importFrom dplyr select arrange left_join bind_rows mutate summarise group_by ungroup filter pull if_else summarise_if
#' @export
#'
get_wsgif_hydro <- function(hydro_flow_fn_1,
                            hydro_flow_fn_2,
                            hydro_flow_file_dir,
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
    select(plant, nameplate_MW_, BA, reg_hucwm, grid_id) %>%
    mutate(nameplate_MW_ = if_else(nameplate_MW_ < 0, 0, nameplate_MW_)) %>%
    filter(reg_hucwm %in% hucwm_regions) %>%
    arrange(-nameplate_MW_) %>%
    mutate(percentile = cumsum(nameplate_MW_) / sum(nameplate_MW_),
           grid_id = as.character(grid_id)) %>%
    filter(percentile <= dam_cutoff_percentile) %>% select(-percentile) ->
    hydro_plants

  # get date sequences
  date_seq_his <- seq(ymd(historical_period[1]),
                       ymd(historical_period[2]),
                       by = "days")

  date_seq_fut <- seq(ymd(future_period[1]),
                       ymd(future_period[2]),
                       by = "days")

  if(missing(hydro_flow_fn_2)){

    list.files(hydro_flow_file_dir) %>%
      .[grepl(hydro_flow_fn_1, .)] -> hydro_flow_files

    str_split(hydro_flow_files, "_") %>%
      map(~.[[1]]) %>% unlist() -> regions_detected
    message(paste0(c("Regions detected:", regions_detected), collapse = " "))

    hydro_flow_files %>%
      # map through all regions and calculate dam-level wsgif
      map(function(hff){

        # read hydro flows
        hydro_flow <- read_csv(paste0(hydro_flow_file_dir, "/", hff),
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
    list.files(hydro_flow_file_dir) %>%
      .[grepl(hydro_flow_fn_1, .)] -> hydro_flow_files_his

    list.files(hydro_flow_file_dir) %>%
      .[grepl(hydro_flow_fn_2, .)] -> hydro_flow_files_fut

    str_split(hydro_flow_files_his, "_") %>%
      map(~.[[1]]) %>% unlist() -> regions_detected_his

    str_split(hydro_flow_files_fut, "_") %>%
      map(~.[[1]]) %>% unlist() -> regions_detected_fut

    if(!all(regions_detected_his == regions_detected_fut)){
      stop(paste0(c("Region inconsistency... Historical regions:", regions_detected_his,
                    "  Future regions:", regions_detected_fut), collapse = " "))
    }

    if(anyDuplicated(regions_detected_his) > 0){
      stop(paste0(c("Some regions have more than one associated file... ",
                    anyDuplicated(regions_detected_his)), collapse = " "))
    }

    message(paste0(c("Regions detected:", regions_detected_his), collapse = " "))

    regions_detected_his %>%
      map(function(hucwm){

        # get historical hydro flows
        read_csv(paste0(hydro_flow_file_dir,
                        "/", hucwm, "_",
                        hydro_flow_fn_1),
                 col_types = cols()) %>%
          filter(date %in% date_seq_his) ->
          hydro_flow_his

        # get future hydro flows
        read_csv(paste0(hydro_flow_file_dir,
                        "/", hucwm, "_",
                        hydro_flow_fn_2),
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

        # return the wsgif
        get_wsgif_all_dams(hydro_flow_his, hydro_flow_fut)

      }) %>% bind_rows() -> wsgif_all_dams_and_regions
  }
  # bring in plant data for balancing authority, region.

  pull(wsgif_all_dams_and_regions, dam) %>% unique() -> wsgif_dams
  pull(hydro_plants, grid_id) -> desired_dams

  if(all(desired_dams %in% wsgif_dams)){
    message("WSGIF is available for all desired hydropower plants!")
    }else{
      warning(paste0(c("Plants missing from WSGIF data:",
                       desired_dams[!desired_dams %in% wsgif_dams]),
                     collapse = " "))
    }


  wsgif_all_dams_and_regions %>%
    filter(is.na(wsgif)) %>% pull(dam) %>% unique() ->
    dams_with_bad_flow_data

  # add balancing authority and hucwm region weights
  hydro_plants %>%
    filter(!grid_id %in% dams_with_bad_flow_data) %>%
    group_by(BA) %>%
    mutate(BA_weighting = nameplate_MW_ / sum(nameplate_MW_)) %>%
    group_by(reg_hucwm) %>%
    mutate(hucwm_weighting = nameplate_MW_ / sum(nameplate_MW_)) %>%
    ungroup() %>%
    mutate(wecc_weighting = nameplate_MW_ / sum(nameplate_MW_)) ->
    hydro_plants_weighted

  wsgif_all_dams_and_regions %>% #filter(is.na(wsgif)) %>% pull(dam) %>% unique()
    left_join(hydro_plants_weighted, by = c("dam" = "grid_id")) %>%
    filter(!is.na(nameplate_MW_)) ->
    wsgif_all_dams_BA_region

  # get wsgif by balancing authority
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * BA_weighting) %>%
    group_by(year, BA) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 2)) %>% ungroup() %>%
    spread(BA, wsgif) -> wsgif_hydro_BA

  # get wsgif by hucwm region
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * hucwm_weighting) %>%
    group_by(year, reg_hucwm) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 2)) %>% ungroup() %>%
    spread(reg_hucwm, wsgif) -> wsgif_hydro_hucwm

  # get wsgif by wecc region
  wsgif_all_dams_BA_region %>%
    mutate(wsgif_weighted = wsgif * wecc_weighting) %>%
    group_by(year, reg_hucwm) %>%
    summarise(wsgif = round(sum(wsgif_weighted), 2)) %>% ungroup() %>%
    spread(reg_hucwm, wsgif) -> wsgif_hydro_wecc

  # write to file
  if(missing(output_file_dir)){
    output_file_dir <- paste0(hydro_flow_file_dir, "/wsgif_hydro")
    dir.create(output_file_dir)
  }

  write_csv(wsgif_hydro_BA, paste0(output_file_dir, "/wsgif_hydro_BA_", output_fn_tag, ".csv"))
  write_csv(wsgif_hydro_hucwm, paste0(output_file_dir, "/wsgif_hydro_hucwm_", output_fn_tag, ".csv"))
  write_csv(wsgif_hydro_wecc, paste0(output_file_dir, "/wsgif_hydro_wecc_", output_fn_tag, ".csv"))

}



