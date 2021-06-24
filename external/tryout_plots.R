install.packages("crosstalk")
install.packages("gganimate")
install.packages("gifski")
library(plotly)
library(crosstalk)
library(gifski)
library(gganimate)

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
    geom_line() + 
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
  highlight(on = "plotly_hover", persistent = FALSE, selectize = TRUE)

pbar <- soorten_biotopen %>% 
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
  ylab("Percentage") +
  coord_flip() + 
  scale_fill_brewer(palette = "RdYlGn", name = "Trendklasse",
                    labels = c("--", "-","0","+","++")) +

  theme_minimal() +
  labs(
    #title = "Percentage soorten per trendbeoordeling",
    #subtitle = "Index 1990 = 100",
    caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)",
    fill = "Trend") +
  theme(legend.position = "top",
        #legend.justification = "top",
        text = element_text(size = 16),
        legend.text = element_text(size = 13)) 

ggplotly(pbar,tooltip = "theme")



p <- fauna_biotopen %>% filter(biotoop != "open") %>%
    ggplot(aes(x = jaar, color = biotoop)) +
    geom_line(aes(y = trend_index)) + 
    ylab("Index") +
    theme_bw() +
    #gghighlight(label_params = list(size = 6)) +
    # scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
    # coord_cartesian(ylim = c(20,120)) +
    labs(
      # title = "Ontwikkeling populaties kenmerkende soorten",
      subtitle = "Index (trend 1990 = 100)", 
      caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
    theme(text = element_text(size = 16)) 

p   
test <- p +  transition_reveal(jaar)
animate(test, renderer = gifski_renderer())
anim_save("test-trend-land.gif")
