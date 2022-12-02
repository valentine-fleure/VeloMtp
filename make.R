#' VeloMtp: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Valentine Fleure \email{valentine.fleure@gmail.com}
#' 
#' @date 2022/12/01



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all(here::here())


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("analyses", "script_X.R"))

source(here::here("analyses","1_prep_data.R"))
source(here::here("analyses", "2_plot.R"))
source(here::here("analyses", "3_kernel.R"))
source(here::here("analyses", "4_plot_temperature.R"))