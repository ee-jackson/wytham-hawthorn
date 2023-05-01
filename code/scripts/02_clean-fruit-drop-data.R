#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: clean-fruit-data
## Desc:
## Date created: 2023-02-07

# packages ----------------------------------------------------------------

library("here")
library("tidyverse")
library("lubridate")


# get data ----------------------------------------------------------------

fruit_counts <- read.csv(here::here("data", "raw", "fruit_drop_data.csv"),
  header = TRUE, na.strings = c("", "NA"))


# clean -------------------------------------------------------------------

fruit_counts %>%
  mutate(date = dmy(date), branch = str_sub(branch_id, -1)) %>%
  mutate(branch = tolower(branch)) %>%
  mutate(branch_id = paste0(tree_id, branch)) %>%
  mutate(survey = case_when(
    date == "2022/03/04" | date == "2022/03/06" | date == "2022/03/07" ~ 8,
    date == "2022/01/28" | date == "2022/01/27" ~ 7,
    date == "2021/12/21" | date == "2021/12/20" ~ 6,
    date == "2021/11/25" | date == "2021/11/24" ~ 5,
    date == "2021/10/29" | date == "2021/10/28" ~ 4,
    date == "2021/09/29" | date == "2021/09/28" | date == "2021/09/27" ~ 3,
    date == "2021/09/02" ~ 2,
    date == "2021/08/06" | date == "2021/08/05" ~ 1,
  )) %>%
  mutate(exclusion = ifelse(branch == "d" | branch == "e" | branch == "f",
                            FALSE, TRUE)) %>%
  group_by(branch_id) %>%
  reframe(across(everything()),
            length_cm = median(length_cm, na.rm = TRUE)) %>%
  select(- branch, - notes) -> survey_data


# summarise ---------------------------------------------------------------

survey_data %>%
  group_by(branch_id) %>%
  reframe(across(c(tree_id, exclusion, length_cm)),
            total_fruit = max(n_fruit),
            n_dropped = max(n_fruit) - min(n_fruit)) %>%
  distinct() %>%
  mutate(proportion_dropped = n_dropped / total_fruit) -> summary_fruit


# add dbh -----------------------------------------------------------------

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  mutate(plot = as.numeric(plot)) %>%
  select(plot, dbh) %>%
  inner_join(summary_fruit, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) -> summary_fruit_dbh


# add connectivity and save -----------------------------------------------

readRDS(here::here("data", "clean", "connectivity_data.rds")) %>%
  inner_join(summary_fruit_dbh, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) %>%
  saveRDS(here::here("data", "clean", "fruit_drop_data.rds"))
