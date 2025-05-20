#### ICC 

library(psych)
library(tidyverse)
library(kableExtra)

icc_row_names <- c(
  "Single Raters (Absolute)",
  "Single Raters (Random)",
  "Single Raters (Fixed)",
  "Average Raters (Absolute)",
  "Average Raters (Random)",
  "Average Raters (Fixed)"
)

# Labrum 

dat_labr_icc <- dat_labr %>% 
  select(
    patient_id, rater, mapping_value
  ) %>% 
  group_by(
    patient_id, rater
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  pivot_wider(
    names_from = rater,
    values_from = mean_mapping_value
  )

dat_labr_icc_sum <- as.data.frame(ICC(dat_labr_icc[, 2:5])$results) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(Type = "type", `F-Stat` = "F") %>% 
  mutate(
    `95% CI` = paste0("[", `lower bound`, ", ", `upper bound`, "]"),
    `p-value` = case_when(
      `p` <= 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p`))) %>% 
  select(-c(`upper bound`, `lower bound`, `p`, contains("df"))) %>% 
  `rownames<-`(icc_row_names) %>% 
  kable(caption = "Labrum Mapping Values") %>% 
  kable_classic(html_font = "cambria", full_width = F)



# Femur

dat_fem_icc <- dat_fem %>% 
  select(
    patient_id, rater, mapping_value
  ) %>% 
  group_by(
    patient_id, rater
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  pivot_wider(
    names_from = rater,
    values_from = mean_mapping_value
  )

dat_fem_icc_sum <- as.data.frame(ICC(dat_fem_icc[, 2:5])$results) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(Type = "type", `F-Stat` = "F") %>% 
  mutate(
    `95% CI` = paste0("[", `lower bound`, ", ", `upper bound`, "]"),
    `p-value` = case_when(
      `p` <= 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p`))) %>% 
  select(-c(`upper bound`, `lower bound`, `p`, contains("df"))) %>% 
  `rownames<-`(icc_row_names) %>% 
  kable(caption = "Femur Mapping Values") %>% 
  kable_classic(html_font = "cambria", full_width = F)






# Acetabulum 

dat_acet_icc <- dat_acet %>% 
  select(
    patient_id, rater, mapping_value
  ) %>% 
  group_by(
    patient_id, rater
  ) %>% 
  summarize(mean_mapping_value = mean(mapping_value)) %>% 
  pivot_wider(
    names_from = rater,
    values_from = mean_mapping_value
  )

dat_acet_icc_sum <- as.data.frame(ICC(dat_acet_icc[, 2:5])$results) %>% 
  mutate_if(is.numeric, round, digits = 3) %>% 
  rename(Type = "type", `F-Stat` = "F") %>% 
  mutate(
    `95% CI` = paste0("[", `lower bound`, ", ", `upper bound`, "]"),
    `p-value` = case_when(
      `p` <= 0.001 ~ "< 0.001",
      TRUE ~ as.character(`p`))) %>% 
  select(-c(`upper bound`, `lower bound`, `p`, contains("df"))) %>% 
  `rownames<-`(icc_row_names) %>% 
  kable(caption = "Acetabulum Mapping Values") %>% 
  kable_classic(html_font = "cambria", full_width = F)



