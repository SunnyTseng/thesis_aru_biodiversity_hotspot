#######################
###
### Name: Rarfaction curve
### 
### Author: Sunny Tseng
### Date: 2023 July 7th
###
########################


###
### Rarefaction curve for all sites 
###

### Import data
data <- read_csv(here("data", "detection_aru_target_sp_85.csv"))

n_sites <- data$site %>% n_distinct()

inc_data <- data %>%
  group_nest(common_name)
