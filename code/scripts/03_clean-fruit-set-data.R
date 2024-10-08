#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: clean-fruit-set-data
## Desc:
## Date created: 2023-03-20

# packages ----------------------------------------------------------------

library("here")
library("tidyverse")


# tidy data ---------------------------------------------------------------

readRDS(here::here("data", "clean", "connectivity_data.rds")) -> connectivity

read.csv(here::here("data", "raw", "fruit_set_2023.csv")) -> fruit_set_2023

clean_raw_data <- function(filename, input_col, output_col) {
  read.csv(here::here("data", "raw", filename),
           header = TRUE, na.strings = c("", "NA")) %>%
    rename(count = all_of(input_col)) %>%
    mutate(unit = output_col) %>%
    select(tree_id, branch_id, count, unit) %>%
    group_by(tree_id, branch_id) %>%
    slice_max(count, with_ties = FALSE) %>%
    ungroup()
}

tibble(filename = c("fruit_set_flowers.csv", "fruit_set_flowers.csv",
                    "fruit_set_initial.csv", "fruit_set_final.csv"),
       input_col = c("n_flowers", "n_flowers_damaged",
                    "n_fruits", "n_fruits"),
       output_col = c("n_flowers", "n_damaged_flowers",
                     "n_immature_fruits", "n_mature_fruits")) -> raw_data_input

pmap(raw_data_input, clean_raw_data) %>%
  bind_rows() %>%
  pivot_wider(
    id_cols = c(tree_id, branch_id),
    names_from = unit,
    values_from = count
  ) %>%
  mutate(
    n_flowers = n_flowers - n_damaged_flowers
  ) %>%
  filter(n_flowers > 0
  ) %>%
  filter(n_immature_fruits < n_flowers
  ) %>%
  drop_na() %>%
  mutate(
    year = "2022"
  )  -> all_counts_2022


# extract data on if branch was bagged ------------------------------------

read.csv(
  here::here("data", "raw", "fruit_set_flowers.csv"),
  header = TRUE,
  na.strings = c("", "NA")
) %>%
  select(tree_id, branch_id, bagged) %>%
  distinct() %>%
  right_join(all_counts_2022, by = join_by(tree_id, branch_id)) %>%
  select(- n_mature_fruits) -> all_counts_bag_2022


# add 2023 data -----------------------------------------------------------

# in 2023 damaged flowers were not included in n_flowers count
# so we don't have to subtract n_damaged_flowers from n_flowers

fruit_set_2023 %>%
  mutate(
    year = "2023",
    bagged = "FALSE",
    n_damaged_flowers = n_damaged_flowers + n_galled_flowers
  ) %>%
  select(- n_galled_flowers, - notes) %>%
  drop_na(n_flowers, n_immature_fruits) %>%
  rbind(all_counts_bag_2022) -> all_yrs_counts


# add connectivity data ---------------------------------------------------

all_yrs_counts %>%
  inner_join(connectivity, by = c("tree_id" = "plot")) %>%
  mutate(branch_id = tolower(branch_id)) %>%
  mutate(branch_id = paste0(tree_id, branch_id)) -> all_counts_bag_con


# add dbh -----------------------------------------------------------------

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  mutate(plot = as.numeric(plot)) %>%
  select(plot, dbh) %>%
  inner_join(all_counts_bag_con, by = c("plot" = "tree_id"),
             multiple = "all") %>%
  rename(tree_id = plot) %>%
  filter(bagged == FALSE) %>%
  filter(n_flowers != 0) %>%
  saveRDS(here::here("data", "clean", "fruit_set_data.rds"))
