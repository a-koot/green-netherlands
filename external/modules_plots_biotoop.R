


# UI MODULES BIOTOOP per plot/table -----------------------------------------------
sumTableUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Aantal kenmerkende soorten",
    width = 3,
    tableOutput(ns("table_bos_1"))
  )
}

#TODO title is het bij elke biotoop de trend voor populatie aantallen?
lineplotBiotoopUI <- function(id,active_tab) {
  ns <- NS(id)
  box(
    title = textOutput(ns("txt_lineplot_1")),
    plotOutput(ns("plot_bos_4"))
  )
}

barplotUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Aantal soorten per fauna en trendklasse",
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
           ),
           box(
             title = "Geselecteerde soort",
             width = NULL,
             textOutput(ns("text_species")),
             textOutput(ns("text_species_trend")),
             textOutput(ns("text_species_trend_10jr"))
             # textOutput("species_jpg"),
             # img(src = ("species_jpg"))
           ),
    )
  )
  
}

# SERVER TABLE 1 ----------------------------------------------------------
sumTableServer <- function(id,active_tab) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #filter data for active tab "biotoop" 
      data_biotoop <- reactive({soorten_biotopen %>% 
          filter(biotoop == active_tab())
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
lineplotBiotoopServer <- function(id, active_tab) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$txt_lineplot_1 <- renderText({
        paste("Trend fauna", active_tab())
      })
      
      output$plot_bos_4 <- renderPlot({
        fauna_biotopen %>% 
          filter(biotoop == active_tab()) %>%
          ggplot(aes(x = jaar)) +
          geom_point(aes(y = waarneming_index)) +     
          geom_line(aes(y = trend_index)) + 
          ylab("Index") +
          theme_bw() +
          scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
          # coord_cartesian(ylim = c(60,125)) +
          labs(
            #title = "Aantalsontwikkeling kenmerkende soorten 1990 - 2018",
            subtitle = "Index 1990 = 100", 
            caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
          theme(text = element_text(size = 15))
      })
      
    }
  )
}

# SERVER BARPLOT ----------------------------------------------------------
barplotServer <- function(id,active_tab) {
  moduleServer(
    id,
    function(input, output, session) {
      
      data_biotoop <- reactive({soorten_biotopen %>% 
          filter(biotoop == active_tab())
      })
      
      output$plot_bos_2 <- renderPlot({
        data_biotoop() %>% 
          filter(biotoop == active_tab(),
                 trend_gehele_periode != "onzeker") %>% 
          select(fauna_groep, soort, trend_gehele_periode) %>% 
          unique() %>% 
          ggplot() + 
          geom_bar(aes(fauna_groep, fill = trend_gehele_periode)) +
          scale_fill_brewer(palette = "RdYlGn", name = "Trendklasse",
                            labels = c("--", "-","0","+","++")) +
          xlab("Fauna type") + 
          ylab("Aantal") +
          theme_bw() +
          labs(
            #title = "Aantal kenmerkende soorten per fauna type",
            subtitle = "Index 1990 = 100",
            caption = "Bron: NEM (Soortenorganisaties, CBS)") +
           theme(text = element_text(size = 16),
            legend.text = element_text(size = 13, face = "bold"))
      })
       
    }
  )
}


# SERVER SLOPEGRAPH -------------------------------------------------------
slopegraphServer <- function(id, active_tab){
  moduleServer(
    id,
    function(input,output,session) {
      output$plot_bos_1 <- renderPlot({
        trend_sum %>% 
          filter(biotoop == active_tab()) %>% 
          # mutate(trendklasse = fct_recode(trendklasse,
          #                                 "--" = "sterke afname",
          #                                 "-" = "matige afname",
          #                                 "0" = "stabiel",
          #                                 "+" = "matige toename",
          #                                 "++" = "sterke toename",
          #                                 "~" = "onzeker"
          #                                 )) %>% 
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
          gghighlight(soort == selected_soort(), use_group_by = FALSE, 
                      label_params = list(size = 6, nudge_x = 60)) +
          theme_bw() +
          labs(
            # title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
            #               selected_fauna(),
            #               active_tab(), "1990 - 2019"),
            subtitle = "Index 1990 = 100",
            caption = "Bron: NEM (Soortenorganisaties, CBS)") +
          theme(text = element_text(size = 15))
        
      })
            # EXTRA INFO SPECIES ------------------------------------------------------
            #filter trendklasses selected species
      species_trend_geheel <- reactive({soorten_biotopen %>%
                filter(biotoop == active_tab(),
                       soort == input$bos_soort) %>%
                pull(trend_gehele_periode) %>%
                unique() %>%
                as.character()
            })

      species_trend_laatste_10jr <- reactive({soorten_biotopen %>%
                filter(biotoop == active_tab(),
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
      
    }
  )
}
