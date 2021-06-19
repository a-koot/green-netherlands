server <- function(input, output) { 
  
  #make active tab as value to use to filter data etc.
  biotoop_active <- reactive({input$tabs})
  
  landServer("land1", biotoop_active)
  biotoopServer("ui_bos", biotoop_active)
  biotoopServer("ui_duinen", biotoop_active)
  biotoopServer("ui_heide", biotoop_active)
 
  
}
