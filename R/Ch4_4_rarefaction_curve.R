#######################
###
### Name: Rarefaction curve for all sites 
### 
### Author: Sunny Tseng
### Date: 2023 July 7th
###
########################

### Library
library(tidyverse)
library(here)
library(iNEXT)
library(patchwork)
library(lubridate)


### Import data
dat <- read_csv(here("data", "detection_aru_target_sp_85.csv"))

data <- dat %>%
  mutate(year = as.character(year)) %>%
  unite(date, year, month, day) %>%
  mutate(date =  ymd(date))

### Data wrangling to the iNEXT format - survey sites
inc_sites <- data %>%
  group_nest(common_name) %>%
  mutate(n_sites = map_dbl(.x = data, .f =~ .x %>% pull(site) %>% n_distinct())) %>%
  select(-data)
  
project_level <- list()
project_level[[1]] <- c(data$site %>% n_distinct(), 
                        setNames(inc_sites$n_sites, inc_sites$common_name) %>% sort(decreasing = T))
names(project_level) <- "project_level"

### iNEXT - survey sites
out <- iNEXT(project_level,          # The data frame
             q=0,                    # The type of diversity estimator (see discussion of the options below)
             datatype="incidence_freq",   # The type of analysis
             knots=40,                    # The number of data points in your line (more = smoother)
             se=TRUE,                     # Logical statement if you want confidence intervals
             conf=0.95,                   # The level of confidence intervals
             nboot=50)                    # The number of replications to perform - this generates your confidence interval - the bigger the number the longer the run time


p1 <- ggiNEXT(out, type=1,  grey = TRUE) + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARUs", y = "Richness") +
  theme(legend.position = "none",
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.line = element_line(linewidth = 2))


### Data wrangling to the iNEXT format - survey period
inc_days <- data %>%
  group_nest(common_name) %>%
  mutate(n_days = map_dbl(.x = data, .f =~ .x %>% select(date, site) %>% n_distinct())) %>%
  select(-data)

project_level <- list()
project_level[[1]] <- c(data %>% select(date, site) %>% n_distinct(), 
                        setNames(inc_days$n_days, inc_days$common_name) %>% sort(decreasing = T))
names(project_level) <- "project_level"

### iNEXT - survey period
out <- iNEXT(project_level,          # The data frame
             q=0,                    # The type of diversity estimator (see discussion of the options below)
             datatype="incidence_freq",   # The type of analysis
             knots=40,                    # The number of data points in your line (more = smoother)
             se=TRUE,                     # Logical statement if you want confidence intervals
             conf=0.95,                   # The level of confidence intervals
             nboot=50)                    # The number of replications to perform - this generates your confidence interval - the bigger the number the longer the run time


p2 <- ggiNEXT(out, type=1, grey = TRUE) + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  theme(legend.position = "none",
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 25, face = "bold"),
        axis.line = element_line(linewidth = 2))

p2

patch <- p1 + p2 

ggsave(plot = patch,
       filename = here("docs", "figures", "patch.png"),
       height = 15,
       width = 30,
       units = "cm",
       dpi = 300)

