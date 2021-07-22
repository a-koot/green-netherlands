


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
    plotlyOutput(ns("plot_bos_4"))
  )
}

barplotUI <- function(id) {
  ns <- NS(id)
  box(
    title = "Aantal soorten per fauna en trendklasse",
    plotlyOutput(ns("plot_bos_2"))
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
             selectizeInput(inputId = ns("bos_soort"), label = "Soort", choices = NULL,
                         multiple = TRUE, options = list(maxItems = 4))
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
      
      output$plot_bos_4 <- renderPlotly({
        gg <- fauna_biotopen %>% 
          filter(biotoop == active_tab()) %>%
          ggplot(aes(x = jaar)) +
          geom_point(aes(y = waarneming_index)) +     
          geom_line(aes(y = trend_index)) + 
          ylab("Index") +
          theme_bw() +
          scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) 
        
        p <- ggplotly(gg, dynamicTicks = TRUE) %>% 
          layout(title = list(text = paste0("",
                                           "<br>",
                                           "<sup>",
                                           "Index 1990 = 100",
                                           "</sup>")),
                 hovermode = "x unified",
                 annotations = list(x = 1, y = 0.01,
                                    text = "Data bron: NEM(Soortenorganisaties CBS)",
                                    showarrow = F, xref = "paper", yref = "paper",
                                    xanchor = "right", yanchor = "auto",
                                    xshift = 0, yshift = 0,
                                    font = list(size = 9)))
        p
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
      
      df_totals <- reactive({data_biotoop() %>% 
        filter(
               trend_gehele_periode != "onzeker") %>%
        group_by(fauna_groep) %>% 
        #to use fauna groep sum as labels in barplot
        mutate(groep_sum = n_distinct(soort)) %>% 
        ungroup() %>% 
        mutate(trend_gehele_symbols = fct_recode(trend_gehele_periode,
                                                 "--" = "sterke afname",
                                                 "-" = "matige afname",
                                                 "0" = "stabiel",
                                                 "+" = "matige toename",
                                                 "++" = "sterke toename",
                                                 "~" = "onzeker"
        )) %>%
        mutate(trend_gehele_symbols = factor(trend_gehele_symbols,
                                             levels = rev(levels(trend_gehele_symbols))
        )
        ) %>% 
        select(fauna_groep, soort, trend_gehele_periode,trend_gehele_symbols,groep_sum) %>%
        unique() %>% 
        #to get number of species per trend to use for hover info with adjusted layout
        group_by(fauna_groep, trend_gehele_periode) %>% 
        mutate(groep_trend_sum = n_distinct(soort)) %>% 
        ungroup()
      })
      
      
      
      output$plot_bos_2 <- renderPlotly({
        gg <- 
          ggplot(df_totals(),aes(reorder(fauna_groep, groep_sum), fill = trend_gehele_symbols,
                     text = paste(fauna_groep,
                                  "<br>Aantal soorten:", groep_trend_sum,
                                  "<br>Trend:", trend_gehele_periode))) +
          geom_bar() + 
          coord_flip() +
          geom_text(aes(fauna_groep, groep_sum,label = groep_sum, fill = NULL,
                        hjust = -1.5)) +
          ylim(0,max(df_totals()$groep_sum) * 1.05) +
          scale_fill_brewer(palette = "RdYlGn", direction = -1,
                            name = "Trend",
                            labels = c("++", "+","0","-","--")) +
          xlab("Fauna type") +
          ylab("Aantal soorten") +
          theme_bw() +
          theme_minimal() +
          theme(text = element_text(size = 16),
                legend.text = element_text(size = 13, face = "bold"),
                legend.position = "top",
                axis.title.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.title = element_blank())
        

        p <- ggplotly(gg, tooltip = c("text")) %>%
          style(textposition = "right",
                hoverinfo = "none", traces = c(6)) %>%
          layout(legend = list(position = "h",x = 1, y = 1,
                               title = list(text = "Trend")),
                 annotations = list(x = 1, y = 0.01,
                                    text = "Data bron: NEM(Soortenorganisaties, \n CBS)",
                                    showarrow = F, xref = "paper", yref = "paper",
                                    xanchor = "right", yanchor = "auto",
                                    xshift = 0, yshift = 0,
                                    font = list(size = 9))
          )
        p
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
          gghighlight(soort %in% selected_soort(), use_group_by = FALSE,
                      label_params = list(size = 6, nudge_x = 60)) +
          theme_bw() +
          labs(
            # title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
            #               selected_fauna(),
            #               active_tab(), "1990 - 2019"),
            subtitle = "Index 1990 = 100",
            caption = "Bron: NEM (Soortenorganisaties, CBS)") +
          theme(text = element_text(size = 16))
                # legend.position = "right",
                # legend.text = element_text(size = 13, face = "bold"))
        
      })
            # EXTRA INFO SPECIES ------------------------------------------------------
            #filter trendklasses selected species
      species_trend_geheel <- reactive({soorten_biotopen %>%
                filter(biotoop == active_tab(),
                       soort %in% input$bos_soort) %>%
                pull(trend_gehele_periode) %>%
                unique() %>%
                as.character()
            })

      species_trend_laatste_10jr <- reactive({soorten_biotopen %>%
                filter(biotoop == active_tab(),
                       soort %in% input$bos_soort) %>%
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
