## Mapping Value ~ Size  

## Data Clean 

rm(list = ls())

library(readxl)
library(tidyverse)
library(stringi)
library(lme4)
library(lmerTest)

dat <- ... %>% 
  janitor::clean_names()

dat_long <- dat %>% 
  pivot_longer(
    size_12_oclock_adam:size_9_oclock_cate,
    names_to = "group",
    values_to = "size"
  )

dat_long$position <- paste0(readr::parse_number(dat_long$group), "_oclock")

dat_long$rater <- str_sub(dat_long$group, start= -4)

dat_long <- dat_long %>% 
  mutate(recon = ifelse(recon == "Y", 1, 0)) %>% 
  mutate_if(is.character, as.factor)

dat_long_an <- dat_long %>% 
  select(
    c("mrn", "recon", "age", "sex", "size", "position", "rater")
  )

dat_map <- ... %>% 
  janitor::clean_names() %>% 
  drop_na()

names(dat_map)[names(dat_map) == 'patient_id'] <- 'mrn'

dat_full <- merge(dat_long_an, dat_map, by = "mrn")

dat_full <- dat_full %>% 
  select(mrn, size, mapping_value, position, location, rater.y, pass_num, measurement_num, zone)

dat_full$location <- str_remove(dat_full$location, pattern = "T2-")

dat_full$rater.y <- as.factor(dat_full$rater.y)
dat_full$pass_num <- as.factor(dat_full$pass_num)
dat_full$measurement_num <- as.factor(dat_full$measurement_num)
dat_full$zone <- as.factor(dat_full$zone)

dat_full <- dat_full %>% 
  filter(location == "Labrum")


{
  library(gtsummary)
  
  reset_gtsummary_theme() 
  
  summary_render <- list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"
  )
  
  
  anova_stat_render <- list(
    all_continuous() ~ "aov", 
    all_categorical() ~ "chisq.test")
  
  
  ttest_stat_render <- list(
    all_continuous() ~ "t.test", 
    all_categorical() ~ "chisq.test")
  
  }

## Analysis 

# 12 o'clock
mod12 <- lmer(mapping_value ~ size + (1 | mrn:rater.y:pass_num:measurement_num:zone),
             data = dat_full %>% filter(position == "12_oclock"))

summary(mod12)
mod12_sum <- tbl_regression(
  mod12,
  estimate_fun = ~ style_number(.x, digits = 2),
  intercept = TRUE,
  label = list(
    size ~ "Labral Size"
  )
) %>% 
  add_global_p() %>%
  bold_labels() %>%
  italicize_labels() %>%
  italicize_levels() %>% 
  modify_column_unhide(column = c(std.error, statistic)) %>% 
  modify_spanning_header(
    c(estimate, std.error, statistic, ci, p.value) ~
      "**12 O'Clock Measurements**")


p_12 <- ggplot(
  data = dat_full %>% 
    filter(position == "12_oclock"),
  aes(
    x = size,
    y = mapping_value
  )
) + 
  geom_point(
    color = "grey"
  ) + 
  geom_abline(
    slope = 0.27,
    intercept = 83.85,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Labral Size",
    y = "Mapping Value",
    subtitle = "12 O'Clock"
  )


# 3 o'clock
mod3 <- lmer(mapping_value ~ size + (1 | mrn:rater.y:pass_num:measurement_num:zone),
              data = dat_full %>% filter(position == "3_oclock"))

summary(mod3)

mod3_sum <- tbl_regression(
  mod3,
  estimate_fun = ~ style_number(.x, digits = 2),
  intercept = TRUE,
  label = list(
    size ~ "Labral Size"
  )
) %>% 
  add_global_p() %>%
  bold_labels() %>%
  italicize_labels() %>%
  italicize_levels() %>% 
  modify_column_unhide(column = c(std.error, statistic)) %>% 
  modify_spanning_header(
    c(estimate, std.error, statistic, ci, p.value) ~
      "**3 O'Clock Measurements**")


p_3 <- ggplot(
  data = dat_full %>% 
    filter(position == "3_oclock"),
  aes(
    x = size,
    y = mapping_value
  )
) + 
  geom_point(
    color = "grey"
  ) + 
  geom_abline(
    slope = -0.19,
    intercept = 87.07,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Labral Size",
    y = "Mapping Value",
    subtitle = "3 O'Clock"
  )


# 9 o'clock
mod9 <- lmer(mapping_value ~ size + (1 | mrn:rater.y:pass_num:measurement_num:zone),
             data = dat_full %>% filter(position == "9_oclock"))

summary(mod9)

mod9_sum <- tbl_regression(
  mod9,
  estimate_fun = ~ style_number(.x, digits = 2),
  intercept = TRUE,
  label = list(
    size ~ "Labral Size"
  )
) %>% 
  add_global_p() %>%
  bold_labels() %>%
  italicize_labels() %>%
  italicize_levels() %>% 
  modify_column_unhide(column = c(std.error, statistic)) %>% 
  modify_spanning_header(
    c(estimate, std.error, statistic, ci, p.value) ~
      "**9 O'Clock Measurements**")


p_9 <- ggplot(
  data = dat_full %>% 
    filter(position == "9_oclock"),
  aes(
    x = size,
    y = mapping_value
  )
) + 
  geom_point(
    color = "grey"
  ) + 
  geom_abline(
    slope = -0.1915,
    intercept = 87.0048,
    linetype = "dashed",
    color = "black"
  ) + 
  theme_bw(
    
  ) + 
  labs(
    x = "Labral Size",
    y = "Mapping Value",
    subtitle = "9 O'Clock"
  )
