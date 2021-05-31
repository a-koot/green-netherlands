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
        #title = "Ontwikkeling fauna natuurgebieden land 1990 - 2018",
        subtitle = "Index 1990 = 100", 
        caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
      theme(text = element_text(size = 15))
  })
  
  #TODO aangeven dat gaat om kenmerkende soorten.Ergens uitleg hierover geven
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
      labs(
        #title = "Percentage soorten per trendbeoordeling",
        #subtitle = "Index 1990 = 100",
        caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)",
        fill = "Trend") +
      theme(legend.position = "top",
            text = element_text(size = 15),
            legend.text = element_text(size = 12))
  })
  
  
  # UI BIODIVERSITEIT:BOS -------------------------------------------------------
  #TODO tabname (input$tabs) gebruiken als variable biotoop_type
  #biotoop_type <- "bos"
  biotoop_active <- reactive({input$tabs})
  
  #test if it works
  #TODO remove code later 
  output$bos_name <- renderText({
    paste("You've selected:", biotoop_active())
  })
  output$duinen_name <- renderText({
    paste("You've selected:", biotoop_active())
  })
  output$heide_name <- renderText({
    paste("You've selected:", biotoop_active())
  })
  
  # UI - BOS - TABLES -------------------------------------------------------
  
  output$table_bos_1 <- renderTable({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_active()) %>% 
      group_by(fauna_groep) %>% 
      summarise(n_distinct(soort))
  },colnames = FALSE)
  
  output$table_bos_2 <- renderTable({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_active()) %>% 
      select(soort, trend_gehele_periode) %>% 
      unique() %>% 
      count(trend_gehele_periode)
    
    
  })
  

  # UI - BOS - PLOT 1 -------------------------------------------------------------

  output$plot_bos_1 <- renderPlot({
    trend_sum %>% 
      filter(biotoop == biotoop_active()) %>% 
      mutate(percentage = round(percentage, digits = 2)) %>% 
      newggslopegraph(trend_periode, percentage, trendklasse,
                      Title = "Percentage soorten per trendbeoordeling",
                      Caption = "Bron: NEM (Soortenorganisaties, CBS)",
                      LineThickness = 1,
                      LineColor = c("gray","#FDAE61","black",rep("gray",3)),
                      YTextSize = 5,
                      XTextSize = 13,
                      CaptionTextSize = 11,
                      DataTextSize = 4.5,
                      TitleTextSize = 14
                      
      ) 
  })


# UI - BOS - PLOT 2 --------------------------------------------------------------

  output$plot_bos_2 <- renderPlot({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_active(),
             trend_gehele_periode != "onzeker") %>% 
      select(fauna_groep, soort, trend_gehele_periode) %>% 
      unique() %>% 
      ggplot() + 
      geom_bar(aes(fauna_groep, fill = trend_gehele_periode)) +
      scale_fill_brewer(palette = "RdYlGn") +
      theme_bw() +
      labs(
        title = paste("Aantal kenmerkende soorten", biotoop_active(), "per fauna type"),
        subtitle = "Index 1990 = 100",
        caption = "Bron: NEM (Soortenorganisaties, CBS)") +
      theme(text = element_text(size = 15))
  })


# UI - BOS - PLOT 3 ---------------------------------------------------------

    
  #REACTIVE INPUT PLOT 3 
  # TODO freezing reactive inputs?
  fauna <- reactive({
    filter(soorten_biotopen, fauna_groep == input$bos_fauna & 
             biotoop == biotoop_active())
  })
  
  observeEvent(fauna(), {
    choices <- unique(fauna()$soort)
    updateSelectInput(inputId = "bos_soort", choices = choices)
  })
  

  #fauna_type <- input$bos_fauna
 # soort_highlight <-  input$bos_soort
  output$plot_bos_3 <- renderPlot({
    soorten_biotopen %>% 
      filter(biotoop == biotoop_active(),
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
                      biotoop_active(), "1990 - 2019"),
        subtitle = "Index 1990 = 100",
        caption = "Bron: NEM (Soortenorganisaties, CBS)") +
      theme(text = element_text(size = 15))
    
  })
  
  #filter trendklasses selected species
  species_trend_geheel <- reactive({soorten_biotopen %>% 
      filter(biotoop == biotoop_active(),
             soort == input$bos_soort) %>% 
      pull(trend_gehele_periode) %>% 
      unique() %>% 
      as.character()
  })
  
  species_trend_laatste_10jr <- reactive({soorten_biotopen %>% 
      filter(biotoop == biotoop_active(),
             soort == input$bos_soort) %>% 
      pull(trend_laatste_10jr) %>% 
      unique() %>% 
      as.character()
  })
  
           
  output$text_species <- renderText({paste("Soort:", input$bos_soort)})
  
  output$text_species_trend <- renderText({paste("Trend gehele periode: ",
                                                 species_trend_geheel())})
  output$text_species_trend_10jr <- renderText({paste("Trend laatste 10 jaar: ",
                                                 species_trend_laatste_10jr())})
  
  #FIXME 
 # output$species_jpg <- renderText({gsub(" ","",paste(input$bos_soort, ".jpg"))})
  output$img <- renderText({gsub(" ","",paste(input$bos_soort, ".jpg"))})
  
# UI - BOS - PLOT 4 -------------------------------------------------------
  output$plot_bos_4 <- renderPlot({
    fauna_biotopen %>% filter(biotoop == "bos") %>%
      ggplot(aes(x = jaar)) +
      geom_point(aes(y = waarneming_index)) +     
      geom_line(aes(y = trend_index)) + 
      ylab("Index") +
      theme_bw() +
      scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
      coord_cartesian(ylim = c(60,125)) +
      labs(
        title = "Aantalsontwikkeling kenmerkende soorten bos 1990 - 2018",
        subtitle = "Index 1990 = 100", 
        caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
      theme(text = element_text(size = 15))
  })

}
