install.packages("crosstalk")
library(plotly)
library(crosstalk)

soorten_biotopen %>% 
  filter(biotoop == "bos",
         soort == "appelvink") %>% 
  select(trend_gehele_periode) %>% 
  .[[1,1]] %>% 
  as.character(.)
  
  

soorten_biotopen %>% 
  filter(biotoop == "bos",
         soort == "appelvink") %>% 
  select(trend_gehele_periode)

soorten_biotopen[soorten_biotopen]
soorten_biotopen$trend_gehele_periode
                 
soorten_biotopen %>% 
  filter(biotoop == "bos",
         soort == "appelvink") %>% 
  pull(trend_gehele_periode) %>% 
  unique() %>% 
  as.character()


p <-   fauna_biotopen %>% filter(biotoop != "open") %>%
    ggplot(aes(x = jaar, y = trend_index, color = biotoop, frame = jaar)) +
    geom_line(aes(x )) + 
    ylab("Index") +
    theme_bw() +
    #gghighlight() +
    scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
    coord_cartesian(ylim = c(20,120)) +
    #labs(
      #title = "Ontwikkeling fauna natuurgebieden land 1990 - 2018",
      #subtitle = "Index 1990 = 100", 
      #caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
    theme(text = element_text(size = 15))

ggplotly(p)

shared_fauna <- SharedData$new(fauna_biotopen, ~ biotoop)
p <-   ggplot(shared_fauna, aes(x = jaar, y = trend_index, color = biotoop)) +
  geom_line() + 
  ylab("Index") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
  coord_cartesian(ylim = c(20,120)) +
  theme(text = element_text(size = 15))

gg <- ggplotly(p)
gg
highlight(gg, on = "plotly_hover", dynamic = TRUE)

shared_fauna2 <- SharedData$new(fauna_biotopen, ~ biotoop, group = "Choose a biotoop")
plot_ly(shared_fauna2, x = ~jaar, y = ~trend_index) %>% 
  group_by(biotoop) %>% 
  add_lines(text = ~biotoop, hoverinfo = "text") %>%  
  highlight(on = "plotly_hover", persistent =FALSE, selectize = TRUE)
