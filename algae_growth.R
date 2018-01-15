#################################
#                               #
#       DataViz Battle          #
#        January 2018           #
# Specific Growth Rate of Algae #
#                               #
#################################

# libraries ---------------------------------------------------------------

library(XML)
library(tidyverse)
library(ggplot2)


# data --------------------------------------------------------------------

algae <- "http://aquatext.com/tables/algaegrwth.htm"

algae_tab <-
  readHTMLTable(algae, header = F, which = 2, stringsAsFactors = F)


# tidy table --------------------------------------------------------------

algae_tab_rename <-
  algae_tab %>% 
  rename("species" = "V1", # this creates colnames that can be gathered
         "5.5000" = "V2",
         "5.2500" = "V3",
         "10.5000" = "V4",
         "10.2500" = "V5",
         "25.5000" = "V6",
         "25.2500" = "V7",
         "30.5000" = "V8",
         "30.2500" = "V9") %>% 
  slice(-1:-3) %>% # no longer needed rows
  gather("temp_light", "growth_rate", -1) %>%
  mutate(temp = str_extract(temp_light, "\\d+"),
         light = str_replace(temp_light,"\\d+.",""),
         growth_rate = as.numeric(growth_rate),
         growth_rate = if_else(species == "Isochrysis aff. galbana" & is.na(growth_rate), 0.06, growth_rate)) # this variable was missing


# visualization -----------------------------------------------------------

algae_tab_rename$temp <- factor(algae_tab_rename$temp, levels = c("5", "10", "25", "30"))

algae_tab_rename %>%
  ggplot(aes(y = growth_rate, x = light, fill = light)) +
  geom_point() +
  facet_grid(.~temp)


# cleaned dataset ---------------------------------------------------------

write.csv(algae_tab_rename, "algae_tab_rename.csv")
