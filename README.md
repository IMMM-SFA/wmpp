
<!-- README.md is generated from README.Rmd. Please edit that file -->
wmpp
====

`wmpp` (in development) produces Water Scarcity Grid Impact Factor (WSGIF) estimates and PLEXOS inputs from WM simulation results.

Installation
------------

You can install `wmpp` using devtools:

`devtools::install_github("IMMM-SFA/wmpp")`

Running on PIC
--------------

### Install `wmpp` in your PIC home directory

To run on PIC, you must first ensure `R` is loaded:

$ `module load R/3.2.3`

If this is the first time you've used `wmpp` on PIC, you'll need to install it in your home directory. To do this, start R directly:

$ `R`

... and install `wmpp` as noted above:

> `devtools::install_github("IMMM-SFA/wmpp")`

Then quit R, without saving the workspace image:

> `q("no")`

**Important:** Even though it's possible to begin running `wmpp` from within the R environment on PIC, this should be avoided. Instead, you should set up some bash scripts to run `wmpp` on a computing node.

### Extracting flow data from WM outputs

Computation of Water Scarcity Grid Impact Factor (WSGIF) requires flow time series for relevant grids where hydropower dams (for hydropower WSGIF) and catchment outlets (for thermal WSGIF) are located. The `prep_flow` function will search inside a WM results directory and extract these data from all available gridded flow outputs. The function has three arguments:

1.  `wm_output_dir` is used to specify the WM results directory.

2.  `hyd_output_dir` set the output directory for the extracted time series.

3.- `in_parallel` (default = FALSE) tells the functions whether or not run the regional results folders across separate cores.

An example WM output directory is located at `/pic/projects/wat_nrg_nex/hydromet/Tian/CMIP5_BCSD_runoffs/output/access1-0/`

To extract the relevant flow information you need to set up a simple .R script. Create a new folder in your `$HOME` directory (e.g. `/people/my_username/wmpp_analysis/`) and add the following R script (named `get_flows.R` or similar):

    #################
    # .R script for extracting flow time series for hydropower and thermal WSGIF computation

    # load the wmpp library
    library(wmpp)

    # exectute prep_flow
    prep_flow("/pic/projects/wat_nrg_nex/hydromet/Tian/CMIP5_BCSD_runoffs/output/access1-0/",
              hyd_output_dir = "/people/my_username/wmpp_analysis/access1-0_output_hydro/",
              in_parallel = TRUE)
    #################

Then create a bash (named something like `batch_get_flows.sh`) in the same directory:

    #!/bin/bash

    #SBATCH -p shared
    #SBATCH -n 4
    #SBATCH -A your_allocation
    #SBATCH -t 120
    #SBATCH --output=/dev/null
    #SBATCH --error=/dev/null

    R CMD BATCH get_flows.R

**Note:** WM gridded flow files are in the order of 1Gb each, causing slow load and computation within the R environment. Currently, the above script can take up perhaps up to 30 minutes (depending on how many flow files are in the results directory), even with the `in_parallel` option set to TRUE.

Then send the job for computation:

$`sbatch batch_get_flows.sh`

### WSGIF computation

The computation of WSGIF is computed on files prepared in the prior step. The function `get_wsgif` is used as follows:

    get_wsgif("HIST45_MOSART_2010_WM.csv",
              "RCP45_MOSART_2010_WM.csv",
              "flows_hyd_huc4")
