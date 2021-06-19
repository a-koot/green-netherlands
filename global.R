# Load required packages --------------------------------------------------
library(shiny)
library(shinydashboard)


# Load data ---------------------------------------------------------------
source("external/data_wrangle.R")


# load modules ------------------------------------------------------------
source("external/module_UI_biodiversity.R")
source("external/modules_server_biodiversity.R")
source("external/modules_plots_land.R")

