library(shiny)
library(shinydashboard)

# Load data ---------------------------------------------------------------
source("data_wrangle.R")


# UI MODULE BIOTOOP -------------------------------------------------------
biotoopUI_1 <- function(id) {
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
      plotOutput(ns("plot_bos_1"))
    )
  )
}

  

# SERVER MODULE BIOTOOP ---------------------------------------------------
#TODO use server modules within modules? Per plot bijvoorbeeld
biotoopServer <- function(id,biotoop_active,fauna_biotopen) {
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
                        tabItem(tabName = "bos",
                                biotoopUI("ui1")
                                
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

server <- function(input, output, session) {
  
  #make active tab as value to use to filter data etc.
  #TODO biotoop_active inside didn't work -> why?
  biotoop_active <- reactive({input$tabs})
  
  biotoopServer("ui1",biotoop_active,fauna_biotopen)
  biotoopServer("ui2",biotoop_active,fauna_biotopen)
  biotoopServer("ui3", biotoop_active,fauna_biotopen)
}

shinyApp(ui,server)

