#' get_plexos_inputs
#'
#' @param huc4_fn_1 file naming format for the historical hydro_flow_file, excluding the region (e.g., 10_HIST_MOSART.csv, would be "HIST_MOSART.csv")
#' @param huc4_fn_2 second flow file naming format for the future hydro_flow_file, exluding the region (e.g., "RCP85_MOSART.csv")
#' @param flow_dir directory where huc4 flow results reside
#' @param huc4_shape_dir directory where huc4 shape file ("nhd_huc4.gpkg.zip") resides. If blank, download will be attempted.
#' @param base_yr year from huc4_fn_1 (historical) from which to compute deviation. Should be determined offline for each GCM using WECC WSGIF (73rd percentile of historial years).
#' @details derives plexos inputs for thermal, hydro, and fixed hydro.
#' @importFrom purrr map reduce
#' @importFrom readr read_csv cols
#' @importFrom dplyr left_join right_join filter select bind_rows vars contains case_when full_join
#' @importFrom tidyr replace_na gather spread
#' @importFrom sf st_as_sf st_transform st_intersects st_read
#' @importFrom lubridate mdy_hm year month day hour
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

  # filter for plexos units with locations available
  plexos_units_all %>% filter(!is.na(huc4)) ->
    plexos_units_w_loc

  # read in huc4 outlet grid ids and filter for relevant huc4s
  get_huc4_data() %>%
    filter(huc4 %in% plexos_units_w_loc$huc4) %>%
    mutate(huc4 = as.character(huc4)) ->
    huc4_outlet_grid_id

  # get list of historical files
  list.files(flow_dir) %>%
    .[grepl("huc4", .)] %>%
    .[grepl(huc4_fn_1, .)] -> flow_files_his

  # get list of future files
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

  # get plexos hydro units and attach data and grid ids
  read_plexos_hydro_units() %>%
    left_join(select(plexos_units_all, name, huc4),
              by = "name") %>%
    left_join(select(huc4_outlet_grid_id, huc4, grid_id),
              by = "huc4") %>%
    mutate(grid_id = as.character(grid_id)) %>%
    select(-huc4) ->
    units_grid_id_hydro

  get_hydro_fixed_dispatch() %>%
    left_join(select(plexos_units_all, name, huc4),
              by = c("Generator" = "name")) %>%
    left_join(select(huc4_outlet_grid_id, huc4, grid_id),
              by = "huc4") %>%
    mutate(grid_id = as.character(grid_id)) %>%
    select(-huc4) ->
    units_grid_id_fixed_hydro

  # get flow ratios for all future years
  left_join(
    flow_future_yrs,
    flow_base_yr,
    by = "grid_id"
  ) %>%
    mutate(flow_ratio = flow / base_yr_flow) %>%
    select(grid_id, year, flow_ratio) -> flow_ratios


  # create folders in output directory
  dir.create(paste0(output_dir, "/hydro"))
  dir.create(paste0(output_dir, "/hydro_fixed_dispatch"))
  dir.create(paste0(output_dir, "/thermal"))


  # 1. hydro

  # multiply flow ratios, constrain, write to file in plexos format
  flow_ratios %>%
    split(.$year) %>%
    map(function(x){
      x %>% .$year %>% unique() -> yr
      x %>% select(-year) %>%
        right_join(units_grid_id_hydro, by = "grid_id") %>%
        replace_na(list(flow_ratio = 1)) %>%
        gather(month, hydro, -name, -grid_id, -flow_ratio) %>%
        mutate(hydro_adj = hydro * flow_ratio) %>%
        left_join(read_plexos_hydro_units_max() %>% gather(month, max_hydro, -name),
                  by = c("name", "month")) %>%
        left_join(read_plexos_hydro_units_min() %>% gather(month, min_hydro, -name),
                  by = c("name", "month")) %>%
        mutate(hydro_constrained = case_when(
          hydro_adj > max_hydro ~ max_hydro,
          hydro_adj < min_hydro ~ min_hydro,
          hydro_adj <= max_hydro & hydro_adj >= min_hydro ~ hydro_adj
        )) %>%
        mutate(hydro_constrained = round(hydro_constrained, 3)) %>%
        select(name, month, hydro_constrained) %>%
        spread(month, hydro_constrained) %>%
        select(name, one_of(paste0("M", 1:12))) %>%
        write_csv(paste0(output_dir, "/hydro/monthlyMaxEnergy_hydro_", yr, ".csv"))
    })


  # 2. fixed_hydro

  # get plexos hydro fixed dispatch units and attach data and grid ids
  flow_ratios %>%
    split(.$year) %>%
    map(function(x){
      x %>% .$year %>% unique() -> yr
      x %>% select(-year) %>%
        right_join(units_grid_id_fixed_hydro, by = "grid_id") %>%
        replace_na(list(flow_ratio = 1)) %>%
        mutate(MWh_adj = flow_ratio * MWh,
               Month = month(DATETIME)) %>%
        left_join(read_plexos_fixed_hydro_units_max(),
                  by = c("Generator" = "name", "Month")) %>%
        left_join(read_plexos_fixed_hydro_units_min(),
                  by = c("Generator" = "name", "Month")) %>%
        mutate(hydro_constrained = case_when(
          MWh_adj > max ~ max,
          MWh_adj < min ~ min,
          MWh_adj <= max & MWh_adj >= min ~ MWh_adj
        )) %>%
        mutate(hydro_constrained = round(hydro_constrained, 3),
               Year = year(DATETIME), Day = day(DATETIME),
               Period = hour(DATETIME)) %>%
        select(Year, Month, Day, Period, Generator, hydro_constrained) %>%
        spread(Generator, hydro_constrained) %>%
        write_csv(paste0(output_dir,
                         "/hydro_fixed_dispatch/hydro_fixedDispatch_",
                         yr, ".csv"))
    })


  # 3. thermal

  # read thermal plant data (max and min constraints),
  # ... this function also gives huc4 for units cooled with surface water.
  # Units not cooled with surface water are not de-rated
  read_plexos_thermal_max_min() %>%
    left_join(huc4_outlet_grid_id %>% select(grid_id, huc4),
              by = "huc4") %>%
    filter(name %in% plexos_units_all$name) %>%
    select(-huc4) %>% arrange(name) %>%
    mutate(grid_id = as.character(grid_id)) ->
    plexos_surface_thermal_units_grid_id

  flow_ratios %>%
    split(.$year) %>%
    map(function(x){
      x %>% .$year %>% unique() -> yr
      x %>% select(-year) %>%
        right_join(plexos_surface_thermal_units_grid_id, by = "grid_id") %>%
        replace_na(list(flow_ratio = 1)) %>%
        mutate(thermal = flow_ratio * max) %>%
        # for thermal we use the max cap as the production to be derated
        mutate(thermal_constrained = case_when(
          thermal > max ~ max,
          thermal < min ~ min,
          thermal <= max & thermal >= min ~ thermal
        )) %>%
        mutate(thermal_constrained = round(thermal_constrained, 3)) %>%
        select(Name = name, M1 = thermal_constrained) %>%
        mutate(M2 = M1, M3 = M1, M4 = M1, M5 = M1, M6 = M1,
               M7 = M1, M8 = M1, M9 = M1, M10 = M1, M11 = M1, M12 = M1) %>%
        # ^^ plexos input for thermal maxCap do not vary within year
        write_csv(paste0(output_dir,
                         "/thermal/thermalGen_maxCap_",
                         yr, ".csv"))
    })
}
