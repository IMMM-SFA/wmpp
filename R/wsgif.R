#' get_wsgif_hydro
#'
#' @param hydro_flow_file_1 directory of results folder
#' @param hydro_flow_file_2 optional second flow file (in case results are split); will be binded to hydro_flow_file_1
#' @param by spatial unit ("all", "bal_auth", "region", "HUC2", "WECC")
#' @param historical_period ...
#' @param future_period ...
#' @details computes WSGIF for hydropower by balancing authority, region, HUC2, or entire WECC
#' @return time series of WSGIF
#' @importFrom readr read_table2
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @importFrom magrittr set_colnames
#' @export
#'
get_wsgif_hydro <- function(hydro_flow_file_1,
                            hydro_flow_file_2,
                            historical_period = 1955:2004,
                            future_period = 2005:2095,
                            by = "all"){

  # get names of all files and folders in results directory
  list.files(wm_output_dir, recursive = T) -> all_files

  # infer regions from sub-folders
  all_files %>% strsplit("/") %>% sapply(function(x) x[[1]]) %>%
    unique() -> regions
  message(paste("Regions", c(regions)))

  # read in the inflow data
  all_files %>%
    .[grepl(".KW.flow", .)] %>% .[51] -> x

  read_table2(paste0(wm_output_dir, x),
              col_names = F) -> xx
  xx %>%
    as_tibble() %>%
    mutate(X1 = ymd(X1)) -> xx




  }


