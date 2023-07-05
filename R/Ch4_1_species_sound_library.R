###
### Name: JPRF ARU bird sound library
### 
### Author: Sunny Tseng
### Date: 2023-06-08
###

library(tidyverse)
library(here)

###
### Species comparison between eBird and ARU (2023 Jun. 21) 
###
data_ARU <- read_csv(here("data", "JPRF_species_list", "species_list_above_85_validation_info.csv"))
data_eBird <- read_csv(here("data", "JPRF_species_list", "eBird_JPRF_info.csv"))

# basic comparison with numbers
aru_only_species <- setdiff(data_ARU$common_name, data_eBird$species) # 31
eBird_only_species <- setdiff(data_eBird$species, data_ARU$common_name) # 24, mostly waterfowl
both_species <- intersect(data_eBird$species, data_ARU$common_name) # 69, these would be common species
all_species <- union(data_eBird$species, data_ARU$common_name) # 124 species in JPRF wooow!

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
  mutate(order = fct_reorder(order, ARU_only_p)) %>%
  select(-data)


order_figure <- data_all %>%
  select(order, ARU_only_p, both_p, eBird_only_p) %>%
  pivot_longer(names_to = "method", cols = -order) %>%
  ggplot(aes(fill = method, y = value, x = order)) +
    geom_bar(position = "fill", stat = "identity")
  
order_figure













###
### ARU species detected in each of the sites
###

species_ARU <- read_csv(here("data", "JPRF_species_list", "species_list_above_85_validation_info.csv")) %>%
  pull(common_name)

file_names <- c("2020_passerine_BirdNET.csv", 
                "2021_passerine_BirdNET.csv",
                "2022_passerine_BirdNET.csv")
data_all <- tibble()
for (file in file_names){
  data <- read_csv(here("data", "JPRF_2020_2021_2022_sound_processed", file))
  data_all <- bind_rows(data_all, data)
}

# Clean out the detection with confidence higher than 0.85, got 126 unique species
data_ARU_sites <- data_all %>%
  filter(confidence >= 0.85) %>% # get right detections
  mutate(common_name = case_when( # get the common name correct
    common_name == "American Yellow Warbler" ~ "Yellow Warbler",
    common_name == "Audubon's Warbler" ~ "Yellow-rumped Warbler",
    common_name == "Northwestern Crow" ~ "American Crow",
    common_name == "Slate-colored Fox Sparrow" ~ "Fox Sparrow",
    TRUE ~ common_name))  %>%
  filter(common_name %in% species_ARU) %>% # only keep the species of interest
  group_nest(site) %>%
  mutate(richness = map_dbl(.x = data, .f =~ .x %>% pull(common_name) %>% n_distinct()),
         ARU_day = map_dbl(.x = data, .f =~ .x %>% distinct(year, month, day) %>% nrow()))


  










