#### Shiny App Green Netherlands ####


# Load required packages --------------------------------------------------
library(shiny)
library(shinydashboard)


# Load data ---------------------------------------------------------------
source("R/data_wrangle.R")


# Create UI ---------------------------------------------------------------
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
               
               # fluidRow(
               #   box(
               #     title = "Trend ontwikkeling natuurgebieden",
               #     plotOutput("plot_biotopen")),
               #   
               #   box(
               #     title = "Percentage soorten per trendbeoordeling",
               #     plotOutput("barplot_biotopen"))
               # ),
        ),
        tabItem(tabName = "bos",
                h2("Bos"),
                biotoopUI("ui_bos")
                

                # PREVIOUS CODE UI PLOTS --------------------------------------------------
                # fluidRow(
                #   box(
                #     title = "Aantal kenmerkende soorten",
                #     width = 3,
                #     tableOutput("table_bos_1")),
                #   
                #   # box(
                #   #   title = "Aantal soorten per trendklasse",
                #   #   width = 3,
                #   #     tableOutput("table_bos_2")),
                #   
                #   box(
                #     title = "Ontwikkeling trend bos",
                #     plotOutput("plot_bos_4")
                #   )
                # ),
                # 
                # 
                # fluidRow(
                #   box(
                #     title = "Fauna type en trendbeoordeling",
                #     plotOutput("plot_bos_2")),
                #   box(
                #     title = "Ontwikkeling trendbeoordeligen",
                #     plotOutput("plot_bos_1"))
                # ),
                # 
                # fluidRow(
                #   column(width = 7,
                #     box(
                #       title = "Ontwikkeling kenmerkende soorten",
                #       width = NULL,
                #       plotOutput("plot_bos_3", height = 500)
                #     )
                #   ),
                #   column(width = 3,
                #     box(
                #       title = "Ontwikkeling kenmerkende soorten", 
                #       width = NULL,
                #       selectInput(inputId = "bos_fauna", label = "Fauna type", 
                #                 #TODO niet elke biotoop heeft alle fauna types, selectie maken?
                #                 choices = unique(soorten_biotopen$fauna_groep),
                #                 selected = "broedvogels"),
                #       selectInput(inputId = "bos_soort", label = "Soort", choices = NULL)
                #     ),
                #     box(
                #       title = "Informatie geselecteerde soort",
                #       width = NULL,
                #       textOutput("text_species"),
                #       textOutput("text_species_trend"),
                #       textOutput("text_species_trend_10jr"),
                #       textOutput("species_jpg"),
                #       img(src = ("species_jpg"))
                #   )
                # )
                # )
        ),
        

# TAB DUINEN --------------------------------------------------------------

        
        tabItem(tabName = "duinen",
                h2("Duinen"),
                biotoopUI("ui_duinen")
        ),
        tabItem(tabName = "heide",
                h2("Heide"),
                biotoopUI("ui_heide")
        ),
      tabItem(tabName = "test",
              h3("test")
       )
      )
     )
    )




