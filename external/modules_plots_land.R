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