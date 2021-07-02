# UI MODULE LAND ----------------------------------------------------------
landUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Ontwikkeling fauna natuurgebieden",
      plotOutput(ns("lineplot_land"))
    ),
    
    box(
      title = "Verhouding trendbeoordelingen natuurgebieden",
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
          gghighlight(label_params = list(size = 6)) +
          scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) +
          coord_cartesian(ylim = c(20,120)) +
          labs(
           # title = "Ontwikkeling populaties kenmerkende soorten",
            subtitle = "Index (trend 1990 = 100)",
            caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
          theme(text = element_text(size = 16))
      })
      
      #TODO make animation
      
      # output$lineplot_land <- renderImage({
      #   # A temp file to save the output.
      #   # This file will be removed later by renderImage
      #   outfile <- tempfile(fileext = '.gif')
      #   
      #   p <- fauna_biotopen %>% filter(biotoop != "open") %>%
      #     ggplot(aes(x = jaar, color = biotoop)) +
      #     geom_line(aes(y = trend_index)) + 
      #     ylab("Index") +
      #     theme_bw() +
      #     gghighlight(label_params = list(size = 6)) +
      #     scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
      #     coord_cartesian(ylim = c(20,120)) +
      #     labs(
      #       # title = "Ontwikkeling populaties kenmerkende soorten",
      #       subtitle = "Index (trend 1990 = 100)", 
      #       caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
      #     theme(text = element_text(size = 16)) + 
      #     transition_reveal(jaar)
      #   anim_save("outfile.gif", animate(p)) # New
      #   
      #   # Return a list containing the filename
      #   list(src = "outfile.gif",
      #        contentType = 'image/gif'
      #        # width = 400,
      #        # height = 300,
      #        # alt = "This is alternate text"
      #   )}, deleteFile = TRUE)
      
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
          scale_y_continuous(labels = scales::percent) +
          scale_fill_brewer(palette = "RdYlGn", name = "Trendklasse",
                            labels = c("--", "-","0","+","++")) +
          ylab("Percentage") +
          coord_flip() +
          theme_minimal() +
          labs(
            #title = "Percentage soorten per trendbeoordeling",
            #subtitle = "Index 1990 = 100",
            caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)",
            fill = "Trend") +
          theme(legend.position = "top",
                text = element_text(size = 16),
                legend.text = element_text(size = 13, face = "bold")) 
      })
    }
  )
}