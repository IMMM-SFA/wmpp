# process_thermal_data
#
# gets essential data out from matlab file plantFlow_thermal.mat
# this is performed offline to avoid creating dependency for R.matlab
process_thermal_data <- function(){

  extdata_dir <- system.file("extdata/", package = "wmpp")
  data_ml <- readMat(paste0(extdata_dir, "plantFlow_thermal.mat"))


  lapply(seq(dim(data_ml$plantFlow.thermal)[3]), function(x){
    data_ml$plantFlow.thermal[ , , x]
  }
  ) %>% purrr::map(function(x){

    if(is.null(x$coolingTechnology)){
      ct <- NA_character_
    }else{
      ct <- x$coolingTechnology[1,1]
    }

    if(is.null(x$coolingType)){
      ctp <- NA_character_
    }else{
      ctp <- x$coolingType[1,1]
    }

    # if(is.null(x$fuel)){
    #   fu <- NA_character_
    # }else{
    #   fu <- x$fuel[[1]][[1]][1,1]
    # }

    tibble::tibble(refID = x$refID[1,1],
                   name = x$name[1,1],
                   annualMWh = x$annualMWh[1,1],
                   namePlate = x$namePlate[1,1],
                   TEPPCtype = x$TEPPCtype[1,1],
                   plantCode = x$plantCode[1,1],
                   coolingTechnology = ct,
                   coolingType = ctp,
                   #fuel = x$fuel[[1]][[1]][1,1],
                   latAdj = x$latAdj[1,1],
                   lonAdj = x$lonAdj[1,1],
                   region = x$region[1,1],
                   ifDerating = x$ifDerating[1,1],
                   huc4 = x$huc4[1,1],
                   huc4.name = x$huc4.name[1,1],
                   huc2 = x$huc2[1,1])
  }) %>% dplyr::bind_rows()

}
# wmpp:::process_thermal_data() %>% readr::write_csv("inst/extdata/plantFlow_thermal.csv")






