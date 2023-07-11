#######################
###
### Name: Checking the JPRF ARU effort
### 
### Author: Sunny Tseng
### Date: 2023 July 7th
###
########################

### Library
library(tidyverse)
library(here)
library(lubridate)

###
### Check ARU efforts
###

# dir <- "E:/Audio/2023_passerine"
# 
# effort_info <- tibble()
# sites <- list.files(dir)
# for (site in sites) {
#   
#   files <- list.files(file.path(dir, site), pattern = 'WAV')
#   for (file in files) {
#     
#     size <- file.info(file.path(dir, site, file))$size
#     year <- str_sub(file, start = 1, end = 4)
#     month <- str_sub(file, start = 5, end = 6)
#     day <- str_sub(file, start = 7, end = 8)
#     hour <- str_sub(file, start = 10, end = 11)
#     minute <- str_sub(file, start = 12, end = 13)
#     
#     temp <- c(site, year, month, day, hour, minute, size)
#     effort_info <- rbind(effort_info, temp)
#   }
# }
# names(effort_info) <- c("site", "year", "month", "day", "hour", "minute", "size")
# write_csv(effort_info, "E:/Audio/2023_passerine_effort.csv")

effort_list <- c("2020_passerine_effort.csv", 
                 "2021_passerine_effort.csv",
                 "2022_passerine_effort.csv")

effort_all <- tibble()
for (file in effort_list) {
  effort_temp <- read_csv(here("data", "JPRF_aru_effort", file))
  effort_all <- rbind(effort_all, effort_temp)
}

effort_all_1 <- effort_all %>%
  unite(col = date, year, month, day, remove = FALSE) %>%
  mutate(date = ymd(date)) %>%
  filter(size >= 5760000 | size <= 5760500) %>% # remove the recordings when ARU is not functioning 
  filter() # remove unreasonable dates, such as 1969... unreasonable months, such as Jan... 
  group_nest(date, site)





