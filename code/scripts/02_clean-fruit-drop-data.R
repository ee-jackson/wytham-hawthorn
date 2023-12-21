#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: clean-fruit-data
## Desc:
## Date created: 2023-02-07

# packages ----------------------------------------------------------------

library("here")
library("tidyverse")
library("lubridate")
library("readxl")


# get data ----------------------------------------------------------------

fruit_counts_21_22 <- read.csv(here::here("data", "raw", "fruit_drop_data.csv"),
  header = TRUE, na.strings = c("", "NA"))

fruit_counts_22_23 <- read_excel(
  here::here("data", "raw", "flower_and_fruit_counts_hawthorn_2023.xlsx"),
  sheet = 1, na = "NA") %>%
  select(-notes, - n_flowers, - n_damaged_flowers, - n_galled_flowers) %>%
  mutate(branch_id = tolower(branch_id),
         year = as.factor(2023)) %>%
  filter(n_mature_fruits <= n_immature_fruits) %>%
  mutate(branch_id = paste0(tree_id, branch_id),
         n_dropped = n_immature_fruits - n_mature_fruits) %>%
  rename(total_fruit = n_immature_fruits) %>%
  select(- n_mature_fruits)


# clean -------------------------------------------------------------------

fruit_counts_21_22 %>%
  mutate(date = dmy(date), branch = str_sub(branch_id, -1)) %>%
  mutate(branch = tolower(branch)) %>%
  mutate(branch_id = paste0(tree_id, branch)) %>%
  mutate(survey = case_when(
    date == "2022-03-04" | date == "2022-03-06" | date == "2022-03-07" ~ 8,
    date == "2022-01-28" | date == "2022-01-27" ~ 7,
    date == "2021-12-21" | date == "2021-12-20" ~ 6,
    date == "2021-11-25" | date == "2021-11-24" ~ 5,
    date == "2021-10-29" | date == "2021-10-28" ~ 4,
    date == "2021-09-29" | date == "2021-09-28" | date == "2021-09-27" ~ 3,
    date == "2021-09-02" ~ 2,
    date == "2021-08-06" | date == "2021-08-05" ~ 1,
  )) %>%
  mutate(exclusion = ifelse(branch == "d" | branch == "e" | branch == "f",
                            FALSE, TRUE)) %>%
  group_by(branch_id) %>%
  reframe(across(everything()),
            length_cm = median(length_cm, na.rm = TRUE)) %>%
  select(- notes) -> survey_data_21_22

# exclusion cages were added on survey 3 and counted on these
# branches for the first time

# summarise ---------------------------------------------------------------

survey_data_21_22 %>%
  filter(exclusion == TRUE) %>%
  select(- branch) %>%
  group_by(branch_id) %>%
  reframe(across(c(tree_id, exclusion, length_cm)),
            total_fruit = max(n_fruit),
          min_fruit = min(n_fruit)) %>%
  distinct() %>%
  mutate(n_dropped = total_fruit - min_fruit) %>%
  select(-min_fruit) -> summary_fruit_long

survey_data_21_22 %>%
  filter(survey == 1 | survey == 3) %>%
  filter(branch == "a" | branch == "b" | branch == "c") %>%
  select(-exclusion, -length_cm, -date, - branch) %>%
  group_by(branch_id) %>%
  reframe(across(tree_id),
          total_fruit = max(n_fruit),
          min_fruit = min(n_fruit)) %>%
  distinct() %>%
  mutate(n_dropped = total_fruit - min_fruit) %>%
  select(-min_fruit) %>%
  mutate(year = as.factor(2021)) %>%
  bind_rows(fruit_counts_22_23)  -> summary_fruit_short


# add dbh -----------------------------------------------------------------

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  mutate(plot = as.numeric(plot)) %>%
  select(plot, dbh) %>%
  distinct() %>%
  inner_join(summary_fruit_long, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) -> summary_fruit_dbh

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  mutate(plot = as.numeric(plot)) %>%
  select(plot, dbh) %>%
  distinct() %>%
  inner_join(summary_fruit_short, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) -> summary_fruit_short_dbh


# add connectivity and save -----------------------------------------------

readRDS(here::here("data", "clean", "connectivity_data.rds")) %>%
  inner_join(summary_fruit_dbh, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) %>%
  saveRDS(here::here("data", "clean", "fruit_drop_data.rds"))

readRDS(here::here("data", "clean", "connectivity_data.rds")) %>%
  inner_join(summary_fruit_short_dbh, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) %>%
  saveRDS(here::here("data", "clean", "fruit_drop_data_short.rds"))
