###
### Name: ARU application for JPRF avian community
### 
### Author: Sunny Tseng
### Date: 2023-06-21
###

###
### Species comparison between eBird and ARU
###

# library
library(tidyverse)
library(here)
library(colortools)
library(iNEXT)
library(gridExtra)

# data import
data_ARU <- read_csv(here("data", "JPRF_species_list", "species_aru_85_validation_info_1.csv"))
data_eBird <- read_csv(here("data", "JPRF_species_list", "species_eBird_info.csv"))

# basic comparison with numbers
aru_only_species <- setdiff(data_ARU$common_name, data_eBird$species) # 31
eBird_only_species <- setdiff(data_eBird$species, data_ARU$common_name) # 24, mostly waterfowl
both_species <- intersect(data_eBird$species, data_ARU$common_name) # 69, these would be common species
all_species <- union(data_eBird$species, data_ARU$common_name) # 124 species in JPRF wooow!

# species list
data_list <- full_join(data_ARU, data_eBird, by = c("common_name" = "species", "scientific name", "order", "family")) %>%
  arrange(order, family, best) %>%
  mutate(ARU = if_else(is.na(best), "N", "Y"),
         eBird = if_else(is.na(Jan), "N", "Y")) %>%
  select(common_name, `scientific name`, order, family, ARU, c_85_100, eBird)

# write_csv(data_list, here("data", "JPRF_species_list", "species_combined.csv"))

# comparison by orders
data_all <- full_join(data_ARU, data_eBird, by = c("common_name" = "species", 
                                                   "scientific name", 
                                                   "order", 
                                                   "family")) %>%
  group_nest(order) %>%
  mutate(ARU = map_dbl(.x = data, .f =~ .x %>% select(c_10_25) %>% drop_na() %>% nrow()),
         eBird = map_dbl(.x = data, .f =~ .x %>% select(Jan) %>% drop_na() %>% nrow()),
         both = map_dbl(.x = data, .f =~ .x %>% drop_na(c_10_25, Jan) %>% nrow())) %>%
  mutate(ARU_only = ARU - both,
         eBird_only = eBird - both,
         ARU_p = ARU/(ARU_only + eBird_only + both),
         eBird_p = eBird/(ARU_only + eBird_only + both),
         ARU_only_p = ARU_only/(ARU_only + eBird_only + both),
         eBird_only_p = eBird_only/(ARU_only + eBird_only + both),
         both_p = both/(ARU_only + eBird_only + both)) %>%
  arrange(ARU_p, ARU_only_p) %>%
  mutate(order = as_factor(order)) %>%
  select(-data)


order_figure <- data_all %>%
  select(order, eBird_only_p, both_p, ARU_only_p) %>%
  pivot_longer(names_to = "method", cols = -order) %>%
  ggplot(aes(fill = method, y = value, x = order)) +
    geom_bar(position = "fill", stat = "identity") +
    coord_flip() + 
    scale_y_reverse() +
    scale_fill_manual(values = c("#A57CCD", "#CD7CA5", "#7CCD7C")) +
    theme_light()
  
order_figure

splitComp("palegreen3")
analogous("plum3")














###
### ARU species detected in each of the sites
###

species_ARU <- read_csv(here("data", "detection_aru_target_sp_85.csv")) 

# Clean out the detection with confidence higher than 0.85, got 126 unique species
data_ARU_sites <- species_ARU  %>% # only keep the species of interest
  group_nest(site) %>%
  mutate(richness = map_dbl(.x = data, .f =~ .x %>% pull(common_name) %>% n_distinct()),
         ARU_day = map_dbl(.x = data, .f =~ .x %>% distinct(year, month, day) %>% nrow())) %>%
  select(-data)


  










