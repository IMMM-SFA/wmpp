#' get_plexos_inputs
#'
#' @param huc4_fn_1 file naming format for the hydro_flow_file, excluding the region (e.g., 10_HIST_MOSART.csv, would be "HIST_MOSART.csv")
#' @param huc4_fn_2 optional second flow file naming format in case hist and future parts are split.
#' @param flow_dir directory where huc4 flow results reside
#' @param huc4_shape_dir
#' @details derives plexos inputs for given scenario
#' @importFrom FedData get_huc4
#' @importFrom purrr map reduce
#' @importFrom readr read_csv cols
#' @importFrom dplyr left_join right_join filter select bind_rows vars contains
#' @importFrom tidyr replace_na
#' @importFrom sf st_as_sf st_transform st_intersects st_read
#' @importFrom lubridate year month
#' @return ...
#' @export
#'
get_plexos_inputs <- function(huc4_fn_1,
                              huc4_fn_2,
                              flow_dir,
                              huc4_shape_dir,
                              base_yr,
                              output_dir){

  get_huc4_for_plexos_units(huc4_shape_dir) ->
    plexos_units_all

  plexos_units_all %>% filter(!is.na(huc4)) ->
    plexos_units_w_loc

  get_huc4_data() %>%
    filter(huc4 %in% plexos_units_w_loc$huc4) %>%
    mutate(huc4 = as.character(huc4)) ->
    huc4_outlet_grid_id

  list.files(flow_dir) %>%
    .[grepl("huc4", .)] %>%
    .[grepl(huc4_fn_1, .)] -> flow_files_his

  list.files(flow_dir) %>%
    .[grepl("huc4", .)] %>%
    .[grepl(huc4_fn_2, .)] -> flow_files_fut

  flow_files_his %>%
    map(~read_csv(paste0(flow_dir, "/", .),
                  col_types = cols()) %>%
          mutate(year = year(date)) %>%
          group_by(year) %>%
          summarise_if(is.numeric, mean) %>% ungroup()
    ) %>%
    purrr::reduce(left_join, by = c("year")) %>%
    select(year, one_of(as.character(huc4_outlet_grid_id$grid_id))) %>%
    filter(year == base_yr) %>%
    gather(grid_id, base_yr_flow, -year) %>% select(-year) ->
    flow_base_yr

  flow_files_fut %>%
    map(~read_csv(paste0(flow_dir, "/", .),
                  col_types = cols()) %>%
          mutate(year = year(date)) %>%
          group_by(year) %>%
          summarise_if(is.numeric, mean) %>% ungroup()
        ) %>%
    purrr::reduce(left_join, by = c("year")) %>%
    select(year, one_of(as.character(huc4_outlet_grid_id$grid_id))) %>%
    gather(grid_id, flow, -year) ->
    flow_future_yrs

  read_plexos_hydro_units() %>%
    left_join(select(plexos_units_all, name, huc4),
              by = "name") %>%
    left_join(select(huc4_outlet_grid_id, huc4, grid_id),
              by = "huc4") %>%
    mutate(grid_id = as.character(grid_id)) %>%
    select(-huc4) ->
    units_grid_id_hydro

  dir.create(paste0(output_dir, "/hydro"))

  left_join(
    flow_future_yrs,
    flow_base_yr,
    by = "grid_id"
  ) %>%
    mutate(flow_ratio = flow / base_yr_flow) %>%
    select(grid_id, year, flow_ratio) %>%
    split(.$year) %>%
    map(function(x){
      x %>% .$year %>% unique() -> yr
      x %>% select(-year) %>%
        right_join(units_grid_id_hydro) %>%
        tidyr::replace_na(list(flow_ratio = 1)) %>%
        mutate_if(is.numeric, "*", .$flow_ratio) %>%
        select(-grid_id, -flow_ratio) %>%
        write_csv(paste0(output_dir, "/hydro/monthlyMaxEnergy_hydro_", yr, ".csv"))
    })

}
