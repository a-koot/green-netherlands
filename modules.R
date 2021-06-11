library(shiny)
library(shinydashboard)

# Load data ---------------------------------------------------------------
source("data_wrangle.R")


# UI MODULE LAND ----------------------------------------------------------
landUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Trend ontwikkeling natuurgebieden",
      plotOutput(ns("lineplot_land"))
      ),
    
    box(
      title = "Percentage soorten per trendbeoordeling",
      plotOutput(ns("barplot_land"))
      )
  )
}

# UI MODULE BIOTOOP -------------------------------------------------------
biotoopUI_test <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Aantal kenmerkende soorten",
      width = 3,
      tableOutput(ns("table_bos_1"))
      ),
    
    box(
      title = "Ontwikkeling trend bos",
      plotOutput(ns("plot_bos_4"))
    )
  )
}

biotoopUI_2 <- function(id) {
  ns <- NS(id)
  fluidRow(
    # box(
    #   title = "Fauna type en trendbeoordeling",
    #   plotOutput("plot_bos_2")),
    box(
      title = "Ontwikkeling trendbeoordeligen",
      plotOutput(ns("plot_bos_2"))
    )
  )
}

# UI MODULES per plot/table -----------------------------------------------
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
  
}


# OUTER BIOTOOP UI MODULE -------------------------------------------------

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
    
    #below module already includes a fluidRow
    lineplotSoortUI(ns("lineplot_2"))
  )
}


# SERVER LAND -------------------------------------------------------------
landServer <- function(id,biotoop_active) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$lineplot_land <- renderPlot({
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
      output$barplot_land <- renderPlot({
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
    }
  )
}

# SERVER MODULE BIOTOOP ---------------------------------------------------
#TODO use server modules within modules? Per plot bijvoorbeeld
biotoopServer <- function(id,biotoop_active,fauna_biotopen,trend_sum) {
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
      
      # output$plot_bos_4 <- renderPlot({
      #  biotoopPlot4(fauna_biotopen, biotoop_active())
      # })

     # PLOT 4  -----------------------------------------------------------------
      output$plot_bos_4 <- renderPlot({
       fauna_biotopen %>%
          filter(biotoop == biotoop_active()) %>%
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
      

    # PLOT 1 ------------------------------------------------------------------
      output$plot_bos_1 <- renderPlot({
        trend_sum %>% 
          filter(biotoop == "bos") %>% 
          #filter(biotoop == biotoop_active()) %>% 
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
      

# PLOT 2 ------------------------------------------------------------------
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
lineplotSoortServer <- function(id,biotoop_active) {
  moduleServer(
    id,
    function(input, output, session) {
      
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
      
      

      
    }
  )
}

# FUNCTIONS FOR PLOTS  ----------------------------------------------------
#werkt niet : argument 1 (type 'closure') cannot be handled by 'cat'
biotoopPlot4 <- function(data, biotoop_active) {
  data %>% 
    filter(biotoop == biotoop_active()) %>%
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
}

#TODO code voor plots in function or modules?
biotoopPlot4Server <- function(id, biotoop_active) {
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
          coord_cartesian(ylim = c(60,125)) +
          labs(
            title = "Aantalsontwikkeling kenmerkende soorten bos 1990 - 2018",
            subtitle = "Index 1990 = 100", 
            caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
          theme(text = element_text(size = 15))
      })
      
    }
  )
}



# UI TEST -----------------------------------------------------------------


ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "GroenNederland"),
                    
                    dashboardSidebar(sidebarMenu(
                      id = "tabs",
                      menuItem("Biodiversiteit", tabName = "biodiversiteit", icon = icon("crow"),
                               menuSubItem("Land", tabName = "land", icon = icon("leaf")),
                               menuSubItem("Bos", tabName = "bos", icon = icon("tree")),
                               menuSubItem("Duinen", tabName = "duinen",icon = icon("mountain")),
                               menuSubItem("Heide", tabName = "heide", icon = icon("seedling"))
                      ),
                      
                      menuItem("Test", tabName = "test", icon = icon("dashboard"))
                      
                    )),
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "land",
                                h2("Fauna natuurgebieden land"),
                                landUI("land1")
                        ),
                        tabItem(tabName = "bos",
                                biotoopUI("ui1"),
                                #biotoopUI_2("ui1-2")
                        ),
                        tabItem(tabName = "duinen",
                                biotoopUI("ui2")
                        ),
                        tabItem(tabName = "heide",
                                biotoopUI("ui3")
                        ),
                        tabItem(tabName = "test",
                                h3("test")
                        )
                      )
                    )
)

# SERVER OUTER ------------------------------------------------------------
outerServer <- function(id, biotoop_active){
  moduleServer(
    id,
    function(input,output,session) {
      sumTableServer("table1", biotoop_active)
      lineplotBiotoopServer("lineplot_1", biotoop_active)
      barplotServer("barplot", biotoop_active)
      slopegraphServer("slopegraph", biotoop_active)
      #FIXME lineplotSoort server frix reactive inputs 
      # lineplotSoortServer("lineplot_2", biotoop_active)
    }
  )
}



# SERVER APP  -------------------------------------------------------------
server <- function(input, output, session) {
  
  #make active tab as value to use to filter data etc.
  #TODO biotoop_active inside didn't work -> why?
  biotoop_active <- reactive({input$tabs})
  outerServer("ui1", biotoop_active)
  outerServer("ui2", biotoop_active)
  outerServer("ui3", biotoop_active)
  landServer("land1", biotoop_active)

}

shinyApp(ui,server)

