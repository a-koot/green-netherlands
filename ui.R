#### Shiny App Green Netherlands ####

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
               )
               
     # menuItem("Test", tabName = "test", icon = icon("dashboard"))
                
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "land",
               h2("Fauna natuurgebieden land"),
               landUI("land1")
               
        ),
        tabItem(tabName = "bos",
                h2("Bos"),
                biotoopUI("ui_bos")
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




