##Data wranlge nem data for shiny dashboard
# Author: Amparo Koot, amparokoot@gmail.com
# Last edited: 22-4-2021

library(tidyverse)
library(readxl)
library(gghighlight)
library(plotly)
library(stringr)
source("functions/read_nem.R")
library(RColorBrewer)
library(CGPfunctions)

# Import data -------------------------------------------------------------

#TODO inlezen data met minder code 
land <- import_nem_groep("data/fauna_kenmerkend_land.xlsx")
bos <- import_nem_groep("data/fauna_bos.xlsx")
open <- import_nem_groep("data/fauna_open_natuurgebieden.xlsx")
heide <- import_nem_groep("data/fauna_heide.xlsx")
duinen <- import_nem_groep("data/fauna_duinen.xlsx")

land_soorten <- import_nem_soort("data/fauna_kenmerkend_land.xlsx")
bos_soorten <- import_nem_soort("data/fauna_bos.xlsx")
open_soorten <- import_nem_soort("data/fauna_open_natuurgebieden.xlsx")
heide_soorten <- import_nem_soort("data/fauna_heide.xlsx")
duinen_soorten <- import_nem_soort("data/fauna_duinen.xlsx")

reptielen_soorten <- import_nem_soort("data/reptielen.xlsx")
amfibieen_soorten <-  import_nem_soort("data/amfibieen.xlsx")
broedvogels_soorten <- import_nem_soort("data/broedvogels.xlsx")
zoogdieren_soorten <- import_nem_soort("data/zoogdieren.xlsx")
dagvlinders_soorten <- import_nem_soort("data/vlinders.xlsx")
libellen_soorten <- import_nem_soort("data/libellen-1.xlsx")

# Data cleaning -----------------------------------------------------------
#TODO duidelijke namen
#TODO clean script

#toevoegen aan list zodat zelfde functie kan toepassen op alle groepen via map
groeps_list <- list(land = land, bos = bos, 
                    open = open, heide = heide,
                    duinen = duinen)

fauna_biotopen <- map_dfr(groeps_list, ~factorize_trend(.x), .id = "biotoop")

soorten_list <- list(bos = bos_soorten, 
                     open = open_soorten, heide = heide_soorten,
                     duinen = duinen_soorten)

#TODO land parse problem: staan ' , ' in xlsx file en R gebruikt '.' voor decimals 
#TODO land later weer toevoegen als parse probleem opgelost
#FIXME parse problem land_soorten
#FIXME clean_nem_soort(land_soorten)

#TODO check per df of trendklasses wel goed gespeld zijn etc (dus of in trend_levels)


soorten_biotopen <- map_dfr(soorten_list, ~clean_nem_soort(.x), .id = "biotoop")
#TODO CHECK NA 

##clean all soorten per groep data 

#clean trendklasse zoogdieren
#TODO clean stap toevoegen aan clean_soort functie 
zoogdieren_soorten <-  zoogdieren_soorten %>% 
  mutate( `Trendklasse gehele periode` = str_replace_all(`Trendklasse gehele periode`,
                                                         c("Moderate" = "Matige",
                                                           "increase" = "toename",
                                                           "decrease" = "afname",
                                                           "Strong"   = "Sterke",
                                                           " \\(.*$" = ""
                                                         )),
          `Trendklasse laatste 10 jaar` = str_replace_all(`Trendklasse laatste 10 jaar`,
                                                          c("Moderate" = "Matige",
                                                            "increase" = "toename",
                                                            "decrease" = "afname",
                                                            "Strong"   = "Sterke",
                                                            " \\(.*$" = ""
                                                          ))
  )

fauna_soorten_list <- list(reptielen = reptielen_soorten, amfibieen = amfibieen_soorten, 
                           zoogdieren = zoogdieren_soorten, broedvogels = broedvogels_soorten,
                           dagvlinders = dagvlinders_soorten, libellen = libellen_soorten)

fauna_type_soorten <- map_dfr(fauna_soorten_list, ~clean_nem_soort(.x), .id = "fauna_groep")


# Add the type of fauna to the biotoop soorten data
clean_soort_string <- function(df) {
  df <- df %>% mutate(soort = tolower(soort))
  df <- df %>% mutate(soort = str_squish(soort))
}

#clean soort string zodat namen overeen komen om te kunnen joinen
fauna_type_soorten  <- clean_soort_string(fauna_type_soorten)
soorten_biotopen <- clean_soort_string(soorten_biotopen)

#maak df met unique combi soort en faunagroep, anders zou elk koppel 10x voorkomen,
# want data voor meerdere jaren
fauna_soorten_uniq <- fauna_type_soorten %>% 
  select(fauna_groep, soort) %>% 
  filter(!is.na(soort)) %>% 
  unique()

soorten_biotopen <- left_join(soorten_biotopen, fauna_soorten_uniq, key = "soort")


soorten_biotopen %>% filter(is.na(fauna_groep)) %>% select(soort) %>% unique()
#grote weerschijnvlinder heeft NA bij fauna_ groep -> dagvlinders toevoegen 
soorten_biotopen <-  soorten_biotopen %>% 
  mutate(fauna_groep = if_else(soort == "grote weerschijnvlinder",
                               "dagvlinders", fauna_groep )) 

#clean NA soorten rows , zijn lege rows 
map(soorten_biotopen, ~sum(is.na(.)))
soorten_biotopen <- soorten_biotopen %>% filter(!is.na(soort))

# Summarize data  ---------------------------------------------------------

#bos 37 species, duinen 25 species, heide 30 species, open 48 species
soorten_biotopen %>% 
  group_by(biotoop) %>% 
  summarise(n_distinct(soort))

trend_gehele_periode_sum <- soorten_biotopen %>% 
  select(biotoop, soort, trend_gehele_periode,trend_laatste_10jr) %>% 
  unique() %>%
  count(biotoop, trend_gehele_periode) %>% 
  mutate(trend_periode = "gehele_periode") %>% 
  rename(trendklasse = trend_gehele_periode, count = n) %>% 
  mutate(percentage = case_when(
    biotoop == "bos" ~ count/37 * 100, 
    biotoop == "duinen" ~ count/25 * 100, 
    biotoop == "heide" ~ count/30 * 100, 
    biotoop == "open" ~ count/48 * 100
  ))

trend_laatste_10jr_sum <- soorten_biotopen %>% 
  select(biotoop, soort,trend_laatste_10jr) %>% 
  unique() %>%
  count(biotoop, trend_laatste_10jr) %>% 
  mutate(trend_periode = "laatste_10jr") %>% 
  rename(trendklasse = trend_laatste_10jr, count = n) %>% 
  mutate(percentage = case_when(
    biotoop == "bos" ~ count/37 * 100, 
    biotoop == "duinen" ~ count/25 * 100, 
    biotoop == "heide" ~ count/30 * 100, 
    biotoop == "open" ~ count/48 * 100
  ))

trend_sum <- bind_rows(trend_gehele_periode_sum,trend_laatste_10jr_sum) 
