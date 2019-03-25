#' compute_wsgif_hydro
#'
#' @param wm_output_dir directory of results folder
#' @param by spatial unit ("all", "bal_auth", "region", "HUC2", "WECC")
#' @details computes WSGIF for hydropower
#' @return time series of WSGIF
#' @importFrom readr read_table2
#' @importFrom lubridate ymd
#' @importFrom dplyr mutate
#' @importFrom magrittr set_colnames
#' @export
#'
compute_wsgif_hydro <- function(wm_output_dir,
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


