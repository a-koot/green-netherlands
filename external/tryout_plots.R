install.packages("crosstalk")
install.packages("gganimate")
install.packages("gifski")
install.packages("listviewer")
library(plotly)
library(crosstalk)
library(gifski)
library(gganimate)
library(dplyr)
devtools::install_github("ropensci/plotly") 

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

soorten_bos <- soorten_biotopen %>% 
  filter(biotoop == "bos",
         soort != "kruisbek")

soorten_bos_vogels <- soorten_bos %>% 
  filter(fauna_groep == "broedvogels",
         soort != "kruisbek")
#TODO hoe toevoegen aan shiny? Uitzoeken hoe zit met sharedData objects en modules
shared_soort <- SharedData$new(soorten_bos_vogels, ~ soort, group = "Choose a species")
plot_ly(shared_soort, x = ~jaar, y = ~index, color = I("darkgrey")) %>% 
  group_by(soort) %>% 
  add_lines(x = ~jaar, y = ~index) %>% 
  highlight(on = "plotly_click", persistent = FALSE, selectize = TRUE, color = "lightblue")


soort <- highlight_key(soorten_bos_vogels, ~soort)

base <- plot_ly(soort, color = I("darkgrey")) %>% 
  group_by(soort)

time_series <- base %>% 
  group_by(soort) %>% 
  add_lines(x = ~jaar, y = ~index,
            text = ~soort, hoverinfo = "text+x+y") 

time_series

s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers")


p <- highlight(
  layout(time_series, showlegend = TRUE),
  # time_series,
  on = "plotly_click",
  selectize = TRUE, 
  dynamic = TRUE,
  color = "lightblue",
  persistent = FALSE,
  selected = s
)

plotly_json(p)
p
soorten_biotopen %>%
  filter(biotoop == "bos",
         fauna_groep == "broedvogels",
         soort != "kruisbek") %>% 
  ggplot(aes(x = jaar, y = index, color = soort)) +
  geom_point() + 
  geom_line() +
  expand_limits(x = 2020) +#ruimte voor lable soort highlight
  gghighlight(soort == "appelvink", use_group_by = FALSE, 
              label_params = list(size = 6, nudge_x = 60)) +
  theme_bw() +
  labs(
    # title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
    #               selected_fauna(),
    #               active_tab(), "1990 - 2019"),
    subtitle = "Index 1990 = 100",
    caption = "Bron: NEM (Soortenorganisaties, CBS)") +
  theme(text = element_text(size = 15))






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


p <- soorten_biotopen %>%
  filter(biotoop == "bos",
         fauna_groep == "broedvogels",
         soort != "kruisbek") %>% 
  ggplot(aes(x = jaar, y = index, color = soort)) +
  geom_point() + 
  geom_line() +
  expand_limits(x = 2020) +#ruimte voor lable soort highlight
  gghighlight(soort == c("appelvink","bosuil"), use_group_by = FALSE, 
              label_params = list(size = 6, nudge_x = 60)) +
  theme_bw() +
  labs(
    # title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
    #               selected_fauna(),
    #               active_tab(), "1990 - 2019"),
    subtitle = "Index 1990 = 100",
    caption = "Bron: NEM (Soortenorganisaties, CBS)") +
  theme(text = element_text(size = 15))
p
ggplotly(p)

gg <- soorten_biotopen %>% 
  filter(biotoop == "bos",
         fauna_groep == "broedvogels",
         soort != "kruisbek") %>% 
  highlight_key(~soort) %>% 
  plot_ly(color = I("black")) %>% 
  group_by(soort) %>% 
  add_lines(x = ~jaar, y = ~index)

cols <- toRGB(RColorBrewer::brewer.pal(3, "Dark2"), 0.5)
cols <- toRGB(RColorBrewer::brewer.pal(5, "Set2"))
gg
cols <- list(cols)
s <- attrs_selected(
  showlegend = TRUE,
  mode = "lines+markers"
 # marker = list(symbol = "x")
)


highlight(
  layout(gg, showlegend = TRUE),
  on = "plotly_click",
  selectize = TRUE,
  dynamic = TRUE,
  persistent = TRUE,
  color = cols,
  selected = s)

plotly_json(gg)

soorten_biotopen %>%
  filter(biotoop == "bos",
         fauna_groep == "broedvogels",
         soort != "kruisbek") %>% 
  ggplot(aes(x = jaar, y = index, color = soort)) +
  geom_point() + 
  geom_line() +
  expand_limits(x = 2020) +#ruimte voor lable soort highlight
  gghighlight(soort == "appelvink", use_group_by = FALSE, 
              label_params = list(size = 6, nudge_x = 60)) +
  theme_bw() +
  labs(
    # title = paste("Ontwikkeling populatie-aantallen kenmerkende", 
    #               selected_fauna(),
    #               active_tab(), "1990 - 2019"),
    subtitle = "Index 1990 = 100",
    caption = "Bron: NEM (Soortenorganisaties, CBS)") +
  theme(text = element_text(size = 15))


  p <- data_biotoop() %>%
    filter(biotoop == active_tab(),
           trend_gehele_periode != "onzeker") %>%
    mutate(trend_gehele_periode = str_replace_all(trend_gehele_periode,
                                                  c("sterke afname" = "--",
                                                    "matige afname" = "-",
                                                    "stabiel" = "0",
                                                    "matige toename" = "+",
                                                    "sterke toename" = "++")
    )) %>%
    select(fauna_groep, soort, trend_gehele_periode) %>%
    unique() %>%
    ggplot() +
    geom_bar(aes(fauna_groep, fill = trend_gehele_periode)) +
    scale_fill_brewer(palette = "RdYlGn", name = "Trendklasse",
                      labels = c("--", "-","0","+","++")) +
    xlab("Fauna type") +
    ylab("Aantal") +
    theme_bw() +
    labs(
      #title = "Aantal kenmerkende soorten per fauna type",
      subtitle = "Index 1990 = 100",
      caption = "Bron: NEM (Soortenorganisaties, CBS)") +
    theme(text = element_text(size = 16),
          legend.text = element_text(size = 13, face = "bold"))
  

  
df_totals <- soorten_biotopen %>% 
  filter(biotoop == "duinen",
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

p <- df_totals %>%
  ggplot(aes(reorder(fauna_groep, groep_sum), fill = trend_gehele_symbols,
             text = paste(fauna_groep,
                          "<br>Aantal soorten:", groep_trend_sum,
                          "<br>Trend:", trend_gehele_periode))) +
  geom_bar() + 
  coord_flip() +
  geom_text(aes(fauna_groep, groep_sum,label = groep_sum, fill = NULL,
                               hjust = -1.5)) +
  ylim(0,max(df_totals$groep_sum) * 1.05) +
  scale_fill_brewer(palette = "RdYlGn", direction = -1,
                    name = "Trend",
                    labels = c("++", "+","0","-","--")) +
  xlab("Fauna type") +
  ylab("Aantal soorten") +
  theme_bw() +
  # labs(
  #   #title = "Aantal kenmerkende soorten per fauna type",
  #   caption = "Bron: NEM (Soortenorganisaties, CBS)") +
  theme_minimal() +
  theme(text = element_text(size = 16),
        legend.text = element_text(size = 13, face = "bold"),
        legend.position = "top",
        axis.title.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.title = element_blank()) 


p <- ggplotly(p, tooltip = c("text")) %>%
  style(textposition = "right",
        hoverinfo = "none", traces = c(6)) %>%
  layout(legend = list(position = "h",x = 1, y = 1,
                       title = list(text = "Trend")),
         annotations = list(x = 1, y = 0.01, 
                            text = "Data bron: NEM(Soortenorganisaties, \n CBS)",
                            showarrow = F, xref = "paper", yref = "paper",
                            xanchor = "right", yanchor = "auto", 
                            xshift = 0, yshift = 0,
                            font = list(size = 9)
                            )
  )
p
plotly_json(p)
str(soorten_biotopen)

gg <- fauna_biotopen %>% 
    filter(biotoop == "bos") %>%
    ggplot(aes(x = jaar)) +
    geom_point(aes(y = waarneming_index)) +     
    geom_line(aes(y = trend_index)) + 
    ylab("Index") +
    theme_bw() +
    scale_x_continuous(breaks = seq(1990,2020,5), limits = c(1990,2020)) + 
    # coord_cartesian(ylim = c(60,125)) +
    labs(
      #title = "Aantalsontwikkeling kenmerkende soorten 1990 - 2018",
      subtitle = "Index 1990 = 100", 
      caption = "Bron: NEM (RAVON, Zoogdiervereniging, Sovon, CBS)") +
    theme(text = element_text(size = 15))

gg

ggplotly(gg) %>% 
  layout(hovermode = "x unified")


