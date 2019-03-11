
#' check_storage
#'
#' check the wm storage output
#'
#' @return
#' @importFrom readr read_delim
#' @importFrom lubridate
#' @importFrom magrittr set_colnames
#' @export
#'
#' @examples
check_storage <- function(storage_output_file, res_input_file, in_wm_dir = TRUE){

  # allow for users to simply state wm folder name and input/output names
  if (in_wm_dir == TRUE){
    out_fn <- paste0("output/", storage_output_file)
    in_fn <- paste0("input/", res_input_file)
  }else{
    out_fn <- storage_output_file
    in_fn <- res_input_file
  }

  # read and clean the data
  read_table2(in_fn) -> res_data

  read_table2(out_fn, col_names = F) %>%
    # ^^ read_table2 allows for multiple, varying whitespace between cols
    mutate(X1 = ymd(X1)) %>%
    set_colnames(c("Date", paste0("dam_", res_data$GRAND_ID))) ->
    storage_ts_all

  storage_ts_all %>%
    gather(dam, value, -Date) %>%
    filter(dam %in% c("dam_310", "dam_49")) %>%
    mutate(year = year(Date),
           yday = yday(Date)) %>%
    filter(year %in% seq(1984, 2003)) %>%
    ggplot(aes(yday, value, color = dam)) +
    geom_line() +
    facet_wrap(~year)

}

