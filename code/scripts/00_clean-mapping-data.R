#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_clean-mapping-data.R
## Desc: Read mapping data in from the Google Drive and make it tidy
## Date created: July 2023

# Load packages -----------------------------------------------------------

library("tidyverse")
library("janitor")
library("googledrive")
library("here")

# Get data from Google Drive ----------------------------------------------

googledrive::drive_auth(email = "eleanor.elizabeth.j@gmail.com",
                        scopes = "https://www.googleapis.com/auth/drive")

source(here::here("code", "secrets.R"))

files <- drive_ls(GDRIVE_FOLDER_URL,
                  pattern = "^[0-9]{2}.csv")

files  %>%
  mutate(id = sprintf("https://docs.google.com/uc?id=%s&export=download", id)) %>%
  pull(id) -> urls

names(urls) <- pull(files, name)

data_list <- lapply(urls, read.csv, na.strings = c("","NA"))

# Tidy column names -------------------------------------------------------

repair_cols <- function (plot) {
  plot %>%
    clean_names() %>%
    mutate(across(c(dbh, other_d), as.numeric)) %>%
    mutate(notes = as.character(notes))
}

data_list_col_repair <- lapply(data_list, repair_cols)


# Add missing plots -------------------------------------------------------

# Tree 26 is in the same plot as tree 34 but needs it's own df
data_list_col_repair$`34.csv` %>%
  mutate(name = recode(name,
                       `0` = as.integer(123),
                       `3` = as.integer(0))) -> data_list_col_repair$`26.csv`

# Tree 23 is in the same plot as tree 35 but needs it's own df
data_list_col_repair$`35.csv` %>%
  mutate(name = recode(name,
                       `0` = as.integer(153),
                       `11` = as.integer(0))) -> data_list_col_repair$`23.csv`


# Tidy rows ---------------------------------------------------------------

all_plots <- bind_rows(data_list_col_repair, .id = "plot")

all_plots %>%
  select(-description) %>%
  mutate(plot = gsub(".csv", "", x = plot)) %>%
  rename(
    tree_id = name,
    dead = d,
    horizontal = h,
    broken = q,
    susie_id = susie
  ) %>%
  mutate(stem = replace_na(stem, "main"),
         susie_id = case_when(!is.na(tree_id) ~ susie_id,
                              TRUE ~ NA_character_)) %>%
  fill(
    c("tree_id", "longitude", "latitude", "solution_status", "cs_name",
      "form","reproductive", "susie_id")
  ) %>%
  group_by(plot) %>%
  mutate(stem_id = 1:n()) %>%
  ungroup() %>%
  add_count(tree_id, plot, name = "n_stems") %>%
  mutate_if(is.character, tolower) %>%
  mutate(across(
    c(dead, horizontal, broken),
    ~ case_when(.x == "y" ~ TRUE, .x == "n" ~ FALSE, TRUE ~ NA)
  )) %>%
  mutate(susie_id = dplyr::na_if(susie_id, "n")) %>%
  mutate(cs_name = gsub(" ", "_", cs_name) ) %>%
  mutate(
    tree_id = paste("tree", tree_id, sep = "_"),
    stem_id = paste("stem", stem_id, sep = "_")
  ) -> clean_plots

# only keep the largest stem of a multi-stemmed tree
clean_plots %>%
  drop_na(dbh) %>% # this will drop diameter measurements that were below breast height
  filter(dead == FALSE) %>%
  group_by(plot, tree_id) %>%
  slice_max(dbh, with_ties = FALSE) %>%
  ungroup() %>%
  select(- other_d, - split_height) -> largest_stems

saveRDS(largest_stems, file = here::here("data", "clean", "hawthorn_plots.rds"))
