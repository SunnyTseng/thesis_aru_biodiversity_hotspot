

# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# import data -------------------------------------------------------------

bird_data <- read_csv(here("data", "detection_aru_target_sp_85.csv")) %>%
  unite(date, year, month, day, remove = FALSE) %>%
  mutate(date = ymd(date),
         week = week(date))


# group data by week and site ---------------------------------------------

group_bird <- bird_data %>%
  group_nest(year, week, site) %>%
  mutate(richness = map_dbl(.x = data, .f =~ .x %>%
                              pull(common_name) %>%
                              n_distinct())) %>%
  select(-data) 

richness_table <- group_bird %>%
  pivot_wider(names_from = site, values_from = richness) 

# write_csv(richness_table, here("docs", "tables", "richness_table.csv"))

richness_plot <- group_bird %>%
  group_by(week) %>%
  mutate(richness_average = mean(richness)) %>%
  ggplot(aes(x = week, y = richness)) +
  geom_boxplot(aes(group = week), colour = "darkslategrey", size = 0.7) +
  geom_jitter(colour = "slategrey", size = 1, alpha = 0.3) +
  geom_smooth(aes(x = week, y = richness_average), 
              colour = "indianred2", linewidth = 1.2, method = "loess", linetype = "dashed") +
  facet_wrap(vars(year), nrow = 3) +
  theme_bw() +
  labs(x = "Week of a year", 
       y = "No. of bird species detected in JPRF ARU grid") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(margin = margin(t = 5)),
        axis.title.y = element_text(margin = margin(r = 5)),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

ggsave(plot = richness_plot,
       filename = here("docs", "figures", "richness_plot.png"),
       width = 18,
       height = 24,
       units = "cm",
       dpi = 300)
  
  
  
  
  
  