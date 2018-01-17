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
library(ggthemes)


# data --------------------------------------------------------------------

algae <- "http://aquatext.com/tables/algaegrwth.htm"

algae_tab <-
  readHTMLTable(algae, header = F, which = 2, stringsAsFactors = F, trim = T)

algae_types <- # data downloaded from algaebase
  read_csv("algae_types.csv")


# tidy table --------------------------------------------------------------

algae_tab_names <- # some species downloaded with tab separators
  algae_tab %>%
  mutate(V1 = if_else(V1 == "Chlorella vulgaris\n      (freshwater)", "Chlorella vulgaris (freshwater)", V1)) %>% 
  mutate(V1 = if_else(V1 == "Nannochlorois\n      oculata", "Nannochlorois oculata", V1))

algae_tab_tidy <-
  algae_tab_names %>% 
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
  mutate(temp = paste0(str_extract(temp_light, "\\d+"), "°"),
         light = str_replace(temp_light,"\\d+.",""),
         growth_rate = as.numeric(growth_rate),
         growth_rate = if_else(species == "Isochrysis aff. galbana" & is.na(growth_rate), 0.06, growth_rate)) # this variable was missing

algae_tab_type <-
  left_join(algae_tab_tidy, algae_types)


# visualization -----------------------------------------------------------

algae_tab_type$temp <- factor(algae_tab_type$temp, levels = c("5°", "10°", "25°", "30°"))

algae_tab_type$salinity <- as.character(algae_tab_type$salinity)
algae_tab_type$salinity <- factor(algae_tab_type$salinity, levels = c("35", "0"))

algae_tab_type %>%
  ggplot(aes(y = growth_rate, x = light, color = type)) +
  geom_jitter(width = .1, size = 4) +
  facet_grid(~temp) +
  labs(x = "Light Intensity (lux)", y = "Growth Rate", title = "Specific growth rates of algae (divisions per day) at different light intensities and temperatures",  caption = "Source: Aquatext, Algaebase") + 
  theme_economist_white(gray_bg=FALSE) +
  scale_color_economist(name = "Algae Type") +
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

# cleaned dataset ---------------------------------------------------------

write.csv(algae_tab_type, "./processed/algae_processed.csv")
