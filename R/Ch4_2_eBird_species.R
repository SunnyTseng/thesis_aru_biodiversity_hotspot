###
### Purpose: create JPRF eBird bird list
### 
### Author: Sunny Tseng
### Date: 2023-07-04
### Input: 
### Output: 
###

data_observed <- read_csv(here("data", "JPRF_species_list", "ebird_JPRF.csv"))
clements_species <- read_csv(here("data", "JPRF_species_list", "Clements-Checklist-v2022-October-2022.csv"))

data_observed_1 <- data_observed %>%
  mutate(species = case_when(
    species == "Eurasian Collared Dove" ~ "Eurasian Collared-Dove",
    species == "Eurasian/Hudsonian Whimbrel" ~ "Whimbrel",
    species == "Western Wood Pewee" ~ "Western Wood-Pewee",
    species == "American Dusky Flycatcher" ~ "Dusky Flycatcher",
    species == "Northern Raven" ~ "Common Raven",
    species == "Two-barred Crossbill" ~ "White-winged Crossbill",
    species == "Myrtle/Audubon's Warbler" ~ "Yellow-rumped Warbler",
    TRUE ~ species)) %>%
  left_join(clements_species, by = c("species" = "English name")) %>%
  select(species, "scientific name", order, family, 2:50) %>%
  drop_na("scientific name")

# write_csv(data_observed_1, here("data", "JPRF_species_list", "eBird_JPRF_info.csv"))