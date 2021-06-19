source("R/modules_plots_biodiversity.R")

biotoopUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      sumTableUI(ns("table1")),
      lineplotBiotoopUI(ns("lineplot_1"))
    ),
    
    fluidRow(
      barplotUI(ns("barplot")),
      slopegraphUI(ns("slopegraph"))
    ),
    
    #module already includes a fluidRow
    lineplotSoortUI(ns("lineplot_2"))
  )
}

