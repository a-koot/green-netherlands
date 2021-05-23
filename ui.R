

library(shiny)
library(shinydashboard)

#load data 
source("data_wrangle.R")

ui <- dashboardPage(skin = "green",
                    
    dashboardHeader(title = "GroenNederland"),
    
    dashboardSidebar(sidebarMenu(
      id = "tabs",
      menuItem("Biodiversiteit", tabName = "biodiversiteit", icon = icon("crow"),
               menuSubItem("Land", tabName = "land", icon = icon("leaf")),
               menuSubItem("Bos", tabName = "bos", icon = icon("tree")),
               menuSubItem("Duinen", tabName = "duinen",icon = icon("mountain")),
               menuSubItem("Heide", tabName = "heide", icon = icon("seedling"))),
               
      menuItem("Test", tabName = "test", icon = icon("dashboard"))
                
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "land",
               h2("land tab content"),
               
               fluidRow(
                 box(plotOutput("plot_biotopen"))
               ),
               
               fluidRow(
                 box(plotOutput("barplot_biotopen"))
               )
        ),
        tabItem(tabName = "bos",
                h2("bos tab content"),
                
                fluidRow(
                  box(
                    title = "Aantal kenmerkende soorten",
                    tableOutput("table_bos_1")),
                  box(
                    title = "Aantal kenmerkende soorten2",
                      tableOutput("table_bos_2"))
                ),
                
                
                fluidRow(
                  box(
                    title = "Fauna type en trendbeoordeling",
                    plotOutput("plot_bos_2")),
                  box(
                    title = "Ontwikkeling trendbeoordeligen",
                    plotOutput("plot_bos_1"))
                ),
                
                fluidRow(
                  box(
                    title = "Ontwikkeling kenmerkende soorten", 
                    selectInput(inputId = "bos_fauna", label = "Fauna type", 
                                #TODO niet elke biotoop heeft alle fauna types, selectie maken?
                                choices = unique(soorten_biotopen$fauna_groep),
                                selected = "broedvogels"),
                    selectInput(inputId = "bos_soort", label = "Soort", choices = NULL),
                    width = 7, plotOutput("plot_bos_3", height = 600)))
                
        ),
        tabItem(tabName = "duinen",
                h2("duinen tab content")
        ),
        tabItem(tabName = "heide",
                h2("heide tab content")
        ),
      tabItem(tabName = "test",
              h3("test")
       )
      )
    )
)



