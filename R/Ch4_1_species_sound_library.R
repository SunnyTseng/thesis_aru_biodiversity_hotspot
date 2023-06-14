###
### Name: JPRF ARU bird sound library
### 
### Author: Sunny Tseng
### Date: 2023-06-08
###

library(seewave)
library(tuneR)
library(tidyverse)
library(here)



###
### Data preparation
###

# Detection list from 2020, 2021, and 2022 passerine
file_names <- c("2020_passerine_BirdNET.csv", 
                "2021_passerine_BirdNET.csv",
                "2022_passerine_BirdNET.csv")
data_all <- tibble()
for (file in file_names){
  data <- read_csv(here("data", "JPRF_2020_2021_2022_sound_processed", file))
  data_all <- bind_rows(data_all, data)
}

# Clean out the detection with confidence higher than 0.85, got 126 unique species
data_all_0.85 <- data_all %>%
  filter(confidence >= 0.85) 

# Make a list of potential recordings based on individual species
species_recording <- data_all_0.85 %>%
  unite("recording_u", site, recording, start_s, end_s, sep = "_") %>%
  arrange(desc(confidence)) %>%
  group_nest(common_name) %>%
  mutate(recording_1 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[1]),
         recording_2 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[2]),
         recording_3 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[3]),
         recording_4 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[4]),
         recording_5 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[5])) %>%
  select(-data)
# write_csv(species_recording, here("Ch0_JPRF_library", "docs", "species_list_above_85.csv"))




###
### Recording validation
###
# listen to the target recordings
species <- 126 # range from 1 to 126
for (info in 2:6) {
  file <- species_recording[species, info] %>%
    str_split(pattern = "_") %>%
    unlist()
  
  dir <- paste0("E:/Audio/", str_sub(file[3], 1,4), "_passerine")
  site <- paste0(file[1], "_", file[2])
  recording <- paste0(file[3], "_", file[4])
  start_s <- file[5] %>% as.numeric()
  end_s <- file[6] %>% as.numeric()
  song <- readWave(paste0(dir, "/", site, "/", recording, ".wav"), 
                   from = (start_s - 2), 
                   to = (end_s + 2), 
                   units = "seconds")

  print(paste0("This is recording ", recording, " from ", start_s, " to ", end_s))
  play(song, ... = "/play /close")
}

# plot out spectrum whenever needed
spec <- spectro(song, flim = c(0, 5), tlim = c(2, 3))



###
### Creating folders and move soundfiles around
###

# import the validation datasheet 
data_validated <- read_csv(here("Ch0_JPRF_library", "docs", "species_list_above_85_validation.csv"))

data_validated_cleaned <- data_validated %>%
  drop_na(best) 

# ready for for loop. Here we are doing a species for first iteration
for (j in 1:nrow(data_validated_cleaned)){
  
  species <- data_validated_cleaned[j,]
  
  # create file folder
  dir.create(here("Ch0_JPRF_library", "data", species$common_name))
  
  # For single species, iteration for 5 recordings (if true) and rename the best one
  for (i in 7:11) {
    if (species[i] == "Y") {
      
      file <- species[i-5] %>%
        str_split(pattern = "_") %>%
        unlist()
      
      year <- str_sub(file[3], 1,4)
      site <- paste0(file[1], "_", file[2])
      recording <- paste0(file[3], "_", file[4])
      dir <- paste0("E:/Audio/", year, "_passerine")
      
      from_file <- paste0(dir, "/", site, "/", recording, ".WAV")
      to_folder <- here("Ch0_JPRF_library", "data", species$common_name)
      
      # file copy and rename
      file.copy(from_file, to_folder)
      
      file.rename(paste0(here("Ch0_JPRF_library", "data", species$common_name), "/", 
                         recording, ".WAV"),
                  paste0(here("Ch0_JPRF_library", "data", species$common_name), "/", 
                         year, "_", site, "_", recording, ".WAV"))
    }
  }
  
  best_index <- as.numeric(species[12]) + 6
  
  file <- species[best_index - 5] %>%
    str_split(pattern = "_") %>%
    unlist()
  
  year <- str_sub(file[3], 1,4)
  site <- paste0(file[1], "_", file[2])
  recording <- paste0(file[3], "_", file[4])
  dir <- paste0("E:/Audio/", year, "_passerine")
  
  file.rename(paste0(here("Ch0_JPRF_library", "data", species$common_name), "/", 
                     year, "_", site, "_", recording, ".WAV"),
              paste0(here("Ch0_JPRF_library", "data", species$common_name), "/", 
                     year, "_", site, "_", recording, "_A", ".WAV"))
}


###
### Summarizing data by species (how many detections were received for each of the confidence range, 
### how many ARUs out of 66 were getting that specific species?)
###
data_species <- data_all %>%
  mutate(confidence_cat = cut(confidence, breaks = seq(from = 0.1, to = 1.0, by = 0.15))) %>%
  group_nest(common_name) %>%
  mutate(c_10_25 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU = map_dbl(.x = data, .f =~ .x %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% pull(site) %>% n_distinct())) %>%
  select(-data)

data_validated <- read_csv(here("docs", "species_list_above_85_validation.csv"))

data_species_all <- left_join(data_validated, data_species)
write_csv(data_species_all, here("docs", "species_list_above_85_validation_info.csv"))

