# Load required packages --------------------------------------------------
library(shiny)
library(shinydashboard)
library(plotly)
library(gifski)
library(gganimate)

# Load data ---------------------------------------------------------------
source("external/data_wrangle.R")


# load modules ------------------------------------------------------------
source("external/module_UI_biotoop.R")
source("external/modules_server_biotoop.R")
source("external/modules_plots_land.R")

