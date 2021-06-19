## functions to read xlsx files from Netwerk Ecologische Monitoring (NEM)
## Auteur: Amparo Koot, amparokoot@gmail.com
## Last edited: 17-3-2021

#library(readxl)


# Only read the rows with data, and skip irrelevant additional info/text 
# readxl package required for function 
# first 10 rows are header text 
import_nem_groep <- function(file_name, n_skip = 10, sheet = 1) { 
  df <- read_xlsx(file_name,
            sheet = sheet,
            col_names = c("jaar", "waarneming_index", "trend_index", 
                          "trend_ondergrens", "trend_bovengrens"),
            skip = n_skip)
  
  #add trendklasse from footer data as columns
  trend_geheel <- df[[(nrow(df) - 11),3]]
  trend_laatste_10jr <- df[[(nrow(df) - 10),3]]
  df <- df %>% 
    mutate(trend_gehele_periode = trend_geheel,
           trend_laatste_10jr = trend_laatste_10jr)
  
  #last 14 rows are footer text 
  df[1:(nrow(df) - 14),] %>%
    map_df(~parse_guess(.))
}


# Function to read the 'indexen per soort' sheet of NEM xlsx files.
#first 3 rows is header
import_nem_soort <- function(file_name, n_skip = 3, sheet = 2) {
  #First 2 rows within table are nondata text/string 
  #last 11 rows are footer text 
  df <- read_xlsx(file_name,
            sheet = sheet,
            skip = n_skip) 
  
  df[3:(nrow(df) - 11),] %>%
    map_df(~parse_guess(.))
}

# FIXME error met trend_klasses die geheel_lower case zijn
# FIXME sommige jaar columns zijn character type (omdat , gebruikt en 
# R leest '.' als decimal. Kan door parsing problem niet pivoting 
#TODO check per df of trendklasses wel goed gespeld zijn etc (dus of in trend_levels)
#TODO misschien teveel voor in 1 functie? map gebruiken per stap? opsplitsen in meerdere functies
#TODO snake case trend_levels?
library(stringr)

clean_nem_soort <- function(df) {
  df <- df %>%  pivot_longer(5:last_col(),
                       names_to = "jaar",
                       values_to = "index")
  
  df <- df %>%  rename(soort = ...1,
                 type_waarneming = "Trend in",
                 trend_gehele_periode = "Trendklasse gehele periode",
                 trend_laatste_10jr = "Trendklasse laatste 10 jaar")
  
  df <- df %>% mutate_at(vars(matches("trend")), ~tolower(.))
  df <- df %>% mutate_at(vars(matches("trend")), ~str_squish(.))
  
 trend_levels <- c("sterke afname", "matige afname", "stabiel", "matige toename",
                  "sterke toename", "onzeker",  "onbekend")
 
 df <- df %>%  mutate_at(vars(matches("trend")), ~parse_factor(.,
                                                      levels = trend_levels))
 
 df <- df %>%   mutate(jaar = as.integer(jaar))
}



#TODO rekening houden met lower_cases 
#Function to mutate trendklasse columns to a factor
factorize_trend <- function(df) {
  trend_levels <-   c("Sterke afname", "Matige afname", "Stabiel", "Matige toename",
                      "Sterke toename", "Onzeker")
  df %>% 
    mutate(trend_gehele_periode = parse_factor(trend_gehele_periode, 
                                               levels = trend_levels),
           trend_laatste_10jr = parse_factor(trend_laatste_10jr, levels = 
                                               trend_levels))
  
}


#test_trend <- factorize_trend(test)
#test_trend$trend_gehele_periode
#str(test_trend)



#test_soort <- import_nem_soort("amfibieën.xlsx")
#test_soort <- import_nem_soort("reptielen.xlsx")
#test_vlinders <- import_nem_soort("vlinders.xlsx")
#clean_nem_soort(test_vlinders)

#import_nem_soort <- function(file_name, n_soort, n_skip = 3, sheet = 2) {
  #First 2 rows table are nondata text/string 
#  n_row = n_soort + 2
 # read_xlsx(file_name,
           # sheet = sheet,
           # skip = n_skip,
           # n_max = n_row) [-1:-2,] %>%
   # map_df(~parse_guess(.))
#}