# data preparation

#' prep_flow
#'
#' @param wm_output_dir directory of results folder
#' @param hyd_output_dir output directory
#' @param in_parallel run data extraction in parallel (where multiple regions are being assessed)
#' @details extracts flow data for hydropower grids; combines files if runs are temporally split
#' @return daily time series of regulated flows at hydropower grids
#' @importFrom readr read_table2 read_csv write_csv
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate mutate_if rename bind_rows select one_of
#' @importFrom magrittr set_colnames
#' @importFrom stringr str_split_fixed
#' @importFrom tibble as_tibble
#' @importFrom purrr map pmap
#' @importFrom future plan multiprocess
#' @importFrom furrr future_map
#' @export
#'
prep_flow <- function(wm_output_dir,
                      hyd_output_dir = "",
                      in_parallel = FALSE){

  if(in_parallel == TRUE){
    plan(multiprocess)
    map_wm_regions <- furrr::future_map
  }else{
    map_wm_regions <- purrr::map
  }

  # get names of all flow files and folders in results directory
  list.files(wm_output_dir, recursive = T) %>%
    .[grepl(".KW.flow", .)] %>%
    .[!grepl(".KW.flow.month", .)] -> flow_files

  # infer regions, simulation names, and temporal splits from sub-folders
  flow_files %>%
    str_split_fixed("/", 4) %>% .[,1:3] %>%
    as_tibble() %>%
    rename(reg_hucwm = V1, sim = V2, section = V3) ->
    flow_by_region_sim_sec

  # read flows, combine temporal splits, and extract hydro grids
  flow_by_region_sim_sec %>%
    split(.$reg_hucwm) %>%
    # map on all regions (accross cores if available)
    map_wm_regions(function(x_reg){
      x_reg$reg_hucwm %>% unique() -> reg
      reg %>% read_grid_ids() %>% as.character() -> grid_ids
      reg %>% read_hydro_plant_grids() -> hydro_grid_ids
      x_reg %>%
        split(.$sim) %>%
        # map on all simulations within each region
        map(function(x_reg_sim){
          x_reg_sim %>%
            unique() %>%
            # ^^ deals with cases without section splits
            # map on all temporal sections of simalution
            pmap(~ read_table2(paste0(wm_output_dir, "/",
                                      ..1, "/", ..2, "/", ..3, "/",
                                      ..1, ".KW.flow"),
                                 col_names = F) %>%
                   as_tibble()
                 ) %>% bind_rows() %>%
            mutate(X1 = ymd(X1)) %>%
            set_colnames(c("date", grid_ids)) %>%
            select(one_of(c("date", hydro_grid_ids))) %>%
            mutate_if(is.numeric, round, 3) %>%
            write_csv(paste0(hyd_output_dir, "/", reg, "_", x_reg_sim$sim %>% unique(), ".csv"))
        })
    })
}
