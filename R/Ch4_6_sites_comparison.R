#######################
###
### Name: site clustering with rarefaction curve analysis
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
library(pastecs) # won't work if you have not installed the package first!!
library(graphics)
library(stats)


### Import data
data_species <- read_csv(here("data", "detection_aru_target_sp_85.csv")) %>%
  mutate(year = as.character(year)) %>%
  unite(date, year, month, day) %>%
  mutate(date = ymd(date)) 

data_site_lidar <- read_csv(here("data", "JPRF_site_veg", "JPRF_veg_Lidar_2015_250.csv")) %>%
  mutate(Site = if_else(str_detect(Site, pattern = "N"), 
                        paste0(str_sub(Site, start = 1, end = 1), "_", str_sub(Site, start = 2, end = 3)), 
                        Site)) %>%
  rename("site" = "Site")

data_site_ground <- read_csv(here("data", "JPRF_site_veg", "JPRF_veg_ground_2022.csv")) %>%
  mutate(canopy_cover = rowMeans(select(., starts_with("canopy"))),
         shrub_height = rowSums(select(., starts_with("shrub")))) %>% 
  select(site, canopy_cover, shrub_height, starts_with("tree"))
  

data_site_sub <- data_site_lidar %>%
  select(chm_cat)

###
### Use the Age_binary info
###
data_site_group <- data_site_lidar %>%
  mutate(group = chm_cat) %>%
  select(site, group)

data_site_group <- data_site_lidar %>%
  mutate(group = AGE_BIN) %>%
  select(site, group)

###
### Non-Hierarchical Clustering
###

wss <- rep(0, 8) ## calculate the within sums of squares (wss) for 1 to 6 clusters
wss[1] <- (nrow(data_site_sub) - 1) * sum(sapply(data_site_sub, var)) 
for (i in 2:8){wss[i] <- sum(kmeans(data_site_sub, centers = i)$withinss)}
plot(1:8, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")

# k-means with 4 clusters
groups <- kmeans(data_site_sub, centers = 3)  # since this is in brackets, the object - threegroups - is created and then also "printed" out.

# add the cluster no to justx
groups
str(groups) 
data_site_group <- data_site_lidar %>%
  mutate(group = groups$cluster) %>%
  select(site, group)



###
### Rarefaction curve by grouping 
###
inc_sites <- data_species %>%
  left_join(data_site_group, by = c("site")) 

site_all <- inc_sites %>%
  group_by(group) %>%
  summarize(detections_total = n_distinct(site, date))

site_group <- inc_sites %>%
  group_nest(group, common_name) %>%
  mutate(n_days = map_dbl(.x = data, .f =~ .x %>% select(date, site) %>% n_distinct())) %>%
  select(-data) %>%
  group_nest(group) %>%
  mutate(detections_named = map(.x = data, .f =~ setNames(.x$n_days, 
                                                          .x$common_name) %>%
                                  sort(decreasing = T)))

site_level <- list()
site_level[[1]] <- c(site_all %>% .[1, 2] %>% as.numeric(), 
                     site_group$detections_named[[1]])
site_level[[2]] <- c(site_all %>% .[2, 2] %>% as.numeric(), 
                     site_group$detections_named[[2]])
site_level[[3]] <- c(site_all %>% .[3, 2] %>% as.numeric(), 
                     site_group$detections_named[[3]])
names(site_level) <- site_group %>% pull(group)


### iNEXT - survey sites
out <- iNEXT(site_level,          # The data frame
             q=0,                    # The type of diversity estimator (see discussion of the options below)
             datatype="incidence_freq",   # The type of analysis
             knots=40,                    # The number of data points in your line (more = smoother)
             se=TRUE,                     # Logical statement if you want confidence intervals
             conf=0.95,                   # The level of confidence intervals
             nboot=50)                    # The number of replications to perform - this generates your confidence interval - the bigger the number the longer the run time


###
### iNEXT - ARU days
###
### chm_chat, canopy height
p_canopy_height <- ggiNEXT(out, type = 1, color.var = "Assemblage") + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Canopy Height")

### AGE_BIN, forest age
p_forest_age <- ggiNEXT(out, type = 1) + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Forest Age")

p_forest_age <- ggiNEXT(out, type=1) + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  theme(legend.position = "none",
        axis.text = element_text(size = 15, face = "bold"),
        axis.title = element_text(size = 20, face = "bold"),
        axis.line = element_line(linewidth = 2))

### distance to water, #49 with kmeans
p_distance_water <- ggiNEXT(out, type = 1, color.var = "Assemblage") + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Distance to Water")

### basal area, #35 with kmeans
p_basal_area <- ggiNEXT(out, type = 1, color.var = "Assemblage") + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Sum of Basal Area")

### number of deciduous trees, #23 with kmeans
p_deciduous_count <- ggiNEXT(out, type = 1, color.var = "Assemblage") + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Count of Deciduous Trees")


### shrub height, groupd measure with kmeans
p_shrub_height <- ggiNEXT(out, type = 1, color.var = "Assemblage") + 
  theme_classic() +   #  type 1 = the diversity estimator
  labs(x = "ARU days", y = "Richness") +
  ggtitle("Shrub Height")


save(p_canopy_height, 
     p_forest_age,
     p_distance_water,
     p_basal_area,
     p_deciduous_count,
     p_shrub_height,
     here("R", "rarefaction_curves_figures.RData"))

###
### Hierarchical Cluster Analysis (Optional)
###
### cluster 1
hc1 <- data_site_sub  %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete") %>% # apply hierarchical clustering 
  plot(labels = data_site$site,
       hang = -1, 
       main = "Complete, Euclidean",
       cex = 0.6) # Use the labels (uses hc$order to put the correct labels on), and place the labels at the same level.

### cluster 2
hc2 <- data_site_sub %>%
  dist(method = "euclidean") %>%
  hclust(method = "single") %>% # apply hierarchical clustering 
  plot(labels = data_site$site,
       hang = -1, 
       main = "Single, Euclidean",
       cex=0.6)

### cluster 3
hc3 <- data_site_sub %>%
  scale(center = TRUE, scale = TRUE) %>%
  dist(method = "euclidean") %>%
  hclust(method = "complete") %>%
  plot(labels = data_site$site,
       hang = -1, 
       main = "Complete, Standardized, Euclidean",
       cex = 0.6)

### cluster 4
hc4 <- data_site_sub %>%
  dist(method = "euclidean") %>%
  .^2 %>%
  hclust(method = "centroid") %>%
  plot(labels = data_site$site,
       hang = -1, 
       main = "Centroid, Squared, Euclidean",
       cex = 0.6)
