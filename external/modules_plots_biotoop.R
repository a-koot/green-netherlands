


# UI MODULES BIOTOOP per plot/table -----------------------------------------------
sumTableUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Aantal kenmerkende soorten",
    width = 3,
    tableOutput(ns("table_bos_1"))
  )
}

lineplotBiotoopUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Ontwikkeling trend bos",
    plotOutput(ns("plot_bos_4"))
  )
}

barplotUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Fauna type en trendbeoordeling",
    plotOutput(ns("plot_bos_2"))
  )
}

slopegraphUI <- function(id) {
  box(
    title = "Ontwikkeling trendbeoordeligen",
    plotOutput(NS(id,"plot_bos_1"))
  )
}

lineplotSoortUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 7,
           box(
             title = "Ontwikkeling kenmerkende soorten",
             width = NULL,
             plotOutput(ns("plot_bos_3"), height = 500)
           )
    ),
    column(width = 3,
           box(
             title = "Ontwikkeling kenmerkende soorten",
             width = NULL,
             selectInput(inputId = ns("bos_fauna"), label = "Fauna type",
                         #TODO niet elke biotoop heeft alle fauna types, selectie maken?
                         choices = unique(soorten_biotopen$fauna_groep),
                         selected = "broedvogels"),
             selectInput(inputId = ns("bos_soort"), label = "Soort", choices = NULL)
           )
    )
  )
  
}

# SERVER TABLE 1 ----------------------------------------------------------
sumTableServer <- function(id,biotoop_active) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #filter data for active tab "biotoop" 
      data_biotoop <- reactive({soorten_biotopen %>% 
          filter(biotoop == biotoop_active())
      })
      
      # BIOTOOP TABLES ---------------------------------------------------------
      output$table_bos_1 <- renderTable({
        data_biotoop() %>% 
          group_by(fauna_groep) %>% 
          summarise(n_distinct(soort))
      },colnames = FALSE)
    }
  )
}

# SERVER lineplot biotoop  ------------------------------------------------
lineplotBiotoopServer <- function(id, biotoop_active) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot_bos_4 <- renderPlot({
        fauna_biotopen %>% 
          filter(biotoop == biotoop_active()) %>%
          ggplot(aes(x = jaar)) +
          geom_point(aes(y = waarneming_index)) +     
          geom_line(aes(y = trend_index)) + 
          ylab("Index") +
          theme_bw() +
          scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
          # coord_cartesian(ylim = c(60,125)) +
          labs(
            title = "Aantalsontwikkeling kenmerkende soorten bos 1990 - 2018",
            subtitle = "Index 1990 = 100", 
            caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
          theme(text = element_text(size = 15))
      })
      
    }
  )
}

# SERVER BARPLOT ----------------------------------------------------------
barplotServer <- function(id,biotoop_active) {
  moduleServer(
    id,
    function(input, output, session) {
      
      data_biotoop <- reactive({soorten_biotopen %>% 
          filter(biotoop == biotoop_active())
      })
      
      output$plot_bos_2 <- renderPlot({
        data_biotoop() %>% 
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
      
    }
  )
}


# SERVER PLOT 2 -----------------------------------------------------------

biotoopServer_2 <- function(id,biotoop_active,fauna_biotopen) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #filter data for active tab "biotoop" 
      data_biotoop <- reactive({soorten_biotopen %>% 
          filter(biotoop == biotoop_active())
      })
      
      output$plot_bos_2 <- renderPlot({
        data_biotoop() %>% 
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
    }
  )
}



# SERVER SLOPEGRAPH -------------------------------------------------------
slopegraphServer <- function(id, biotoop_active){
  moduleServer(
    id,
    function(input,output,session) {
      output$plot_bos_1 <- renderPlot({
        trend_sum %>% 
          #filter(biotoop == "bos") %>% 
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
      
      
      
    }
  )
}



# SERVER LINEPLOT 2: SOORT ------------------------------------------------

lineplotSoortServer <- function(id, active_tab) {
  moduleServer(
    id,
    function(input,output, session) {
      
      selected_fauna <- reactive({input$bos_fauna})
      selected_soort <- reactive({input$bos_soort})
      
      # REACTIVE INPUT PLOT 3
      # TODO freezing reactive inputs?
      fauna <- reactive({
        filter(soorten_biotopen, fauna_groep == selected_fauna() &
                 biotoop == active_tab())
      })
      
      observeEvent(fauna(), {
        choices <- unique(fauna()$soort)
        updateSelectInput(inputId = "bos_soort", choices = choices)
      })
      
      output$plot_bos_3 <- renderPlot({
        soorten_biotopen %>% 
          filter(biotoop == active_tab(),
                 fauna_groep == selected_fauna(),
                 soort != "kruisbek") %>% 
          ggplot(aes(x = jaar, y = index, color = soort)) +
          geom_point() + 
          geom_line() +
          expand_limits(x = 2020) +#ruimte voor lable soort highlight
          gghighlight(soort == selected_soort(), label_params = list(nudge_x = 50)) +
          theme_bw() +
          labs(
            title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
                          selected_fauna(),
                          active_tab(), "1990 - 2019"),
            subtitle = "Index 1990 = 100",
            caption = "Bron: NEM (Soortenorganisaties, CBS)") +
          theme(text = element_text(size = 15))
        
      })
      
      #       # EXTRA INFO SPECIES ------------------------------------------------------
      #       # #filter trendklasses selected species
      #       # species_trend_geheel <- reactive({soorten_biotopen %>% 
      #       #     filter(biotoop == biotoop_active(),
      #       #            soort == input$bos_soort) %>% 
      #       #     pull(trend_gehele_periode) %>% 
      #       #     unique() %>% 
      #       #     as.character()
      #       # })
      #       # 
      #       # species_trend_laatste_10jr <- reactive({soorten_biotopen %>% 
      #       #     filter(biotoop == biotoop_active(),
      #       #            soort == input$bos_soort) %>% 
      #       #     pull(trend_laatste_10jr) %>% 
      #       #     unique() %>% 
      #       #     as.character()
      #       # })
      #       # 
      #       # 
      #       # output$text_species <- renderText({paste("Soort:", input$bos_soort)})
      #       # 
      #       # output$text_species_trend <- renderText({paste("Trend gehele periode: ",
      #       #                                                species_trend_geheel())})
      #       # output$text_species_trend_10jr <- renderText({paste("Trend laatste 10 jaar: ",
      #       #                                                     species_trend_laatste_10jr())})
      #       # 
      #       # #FIXME 
      #       # # output$species_jpg <- renderText({gsub(" ","",paste(input$bos_soort, ".jpg"))})
      #       # output$img <- renderText({gsub(" ","",paste(input$bos_soort, ".jpg"))})
      
    }
  )
}
