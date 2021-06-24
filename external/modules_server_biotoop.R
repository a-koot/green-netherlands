source("external/modules_plots_biotoop.R")

biotoopServer <- function(id, biotoop_active){
  moduleServer(
    id,
    function(input,output,session) {
      sumTableServer("table1", biotoop_active)
      lineplotBiotoopServer("lineplot_1", biotoop_active)
      barplotServer("barplot", biotoop_active)
      slopegraphServer("slopegraph", biotoop_active)
      #FIXME lineplotSoort server frix reactive inputs 
      lineplotSoortServer("lineplot_2", biotoop_active)
    }
  )
}
