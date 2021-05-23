server <- function(input, output) { 
  
  
  # UI - BIODIVERSITEIT:LAND -----------------------------------------------------
  
  output$plot_biotopen <- renderPlot({
    fauna_biotopen %>% filter(biotoop != "open") %>%
      ggplot(aes(x = jaar, color = biotoop)) +
      geom_line(aes(y = trend_index)) + 
      ylab("Index") +
      theme_bw() +
      gghighlight() +
      scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
      coord_cartesian(ylim = c(20,120)) +
      labs(
        title = "Aantalsontwikkeling kenmerkende soorten natuurgebieden land 1990 - 2018",
        subtitle = "Index 1990 = 100", 
        caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)")
  })
  
  output$barplot_biotopen <- renderPlot({
    soorten_biotopen %>% 
      filter(biotoop != "open") %>% 
      filter(trend_gehele_periode != "onzeker") %>% 
      mutate(biotoop = factor(biotoop, levels = c("duinen","heide","bos")),
             trend_gehele_periode = factor(trend_gehele_periode, 
                                           levels = c("onzeker", "sterke afname",
                                                      "matige afname", "stabiel",
                                                      "matige toename", "sterke toename"))) %>% 
      select(biotoop, soort, trend_gehele_periode,trend_laatste_10jr) %>% 
      unique() %>%
      ggplot() +
      geom_bar(aes(biotoop, fill = trend_gehele_periode),
               position = position_fill(reverse = TRUE)) +
      coord_flip() +
      scale_fill_brewer(palette = "RdYlGn") +
      theme_minimal() +
      theme(legend.position = "top")
  })
  
  
  # UI BIODIVERSITEIT:BOS -------------------------------------------------------
  biotoop_type <- "bos"
  
  output$plot_bos_1 <- renderPlot({
    trend_sum %>% 
      filter(biotoop == biotoop_type) %>% 
      mutate(percentage = round(percentage, digits = 2)) %>% 
      newggslopegraph(trend_periode, percentage, trendklasse,
                      Title = "Percentage soorten per trendbeoordeling",
                      SubTitle = biotoop_type,
                      Caption = "Bron: NEM (Soortenorganisaties, CBS)",
                      LineThickness = 1,
                      LineColor = c("gray","#FDAE61","#FFFFBF",rep("gray",2)),
                      YTextSize = 5
      ) 
  })
  
  output$plot_bos_2 <- renderPlot({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_type,
             trend_gehele_periode != "onzeker") %>% 
      select(fauna_groep, soort, trend_gehele_periode) %>% 
      unique() %>% 
      ggplot() + 
      geom_bar(aes(fauna_groep, fill = trend_gehele_periode)) +
      scale_fill_brewer(palette = "RdYlGn") +
      theme_bw() +
      labs(
        title = paste("Aantal kenmerkende soorten", biotoop_type, "per fauna type"),
        subtitle = "Index 1990 = 100",
        caption = "Bron: NEM (Soortenorganisaties, CBS)") +
      theme(text = element_text(size = 15))
  })
  
  #REACTIVE INPUT PLOT 3 
  # TODO freezing reactive inputs?
  fauna <- reactive({
    filter(soorten_biotopen, fauna_groep == input$bos_fauna & biotoop == "bos")
  })
  observeEvent(fauna(), {
    choices <- unique(fauna()$soort)
    updateSelectInput(inputId = "bos_soort", choices = choices)
  })
  
  #TODO waar moeten onderstaande globals staan?
  biotoop_type <- "bos"
  #fauna_type <- input$bos_fauna
 # soort_highlight <-  input$bos_soort
  output$plot_bos_3 <- renderPlot({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_type,
             fauna_groep == input$bos_fauna,
             soort != "kruisbek") %>% 
      ggplot(aes(x = jaar, y = index, color = soort)) +
      geom_point() + 
      geom_line() +
      expand_limits(x = 2020) +#ruimte voor lable soort highlight
      gghighlight(soort == input$bos_soort, label_params = list(nudge_x = 50)) +
      theme_bw() +
      labs(
        title = paste("Ontwikkeling populatie-aantallen kenmerkende", input$bos_fauna,
                      biotoop_type, "1990 - 2019"),
        subtitle = "Index 1990 = 100",
        caption = "Bron: NEM (Soortenorganisaties, CBS)") +
      theme(text = element_text(size = 15))
    
  })
  
  output$table_bos_1 <- renderTable({
    soorten_biotopen %>% 
      filter(biotoop == "bos") %>% 
      group_by(fauna_groep) %>% 
      summarise(n_distinct(soort))
    
  })
    
  output$table_bos_2 <- renderTable({
    soorten_biotopen %>% 
      filter(biotoop == "bos") %>% 
      select(soort, trend_gehele_periode) %>% 
      unique() %>% 
      count(trend_gehele_periode)
    
    
  })
  
  
}
