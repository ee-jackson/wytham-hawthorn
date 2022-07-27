#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: 00_clean-mapping-data.R
## Desc: Read mapping data in from the Google Drive and make it tidy
## Date: July 2023

# Load packages -----------------------------------------------------------

library("tidyverse")
library("janitor")
library("googledrive")
library("here")

# Get data from Google Drive ----------------------------------------------

googledrive::drive_auth(email = "eleanor.elizabeth.j@gmail.com",
                        scopes = "https://www.googleapis.com/auth/drive")

files <- drive_ls(GDRIVE_FOLDER_URL,
                  pattern = "^[0-9]{2}.csv")

files  %>%
  mutate(id = sprintf("https://docs.google.com/uc?id=%s&export=download", id)) %>%
  pull(id) -> urls

names(urls) <- pull(files, name)

data_list <- lapply(urls, read.csv, na.strings = c("","NA"))

# Tidy column names -------------------------------------------------------

repair_col_names <- function (plot) {
  plot %>%
    rename_with(recode, "BHD" = "DBH") %>%
    rename_with(recode, "BHC" = "CBH") %>%
    rowwise() %>%
    mutate(DBH = ifelse("DBH" %in% names(.), DBH, NA),
           CBH = ifelse("CBH" %in% names(.), CBH, NA),
           Other.HC = ifelse("Other.HC" %in% names(.), Other.HC, NA)) %>%
    ungroup() %>%
    mutate(across(c(DBH, CBH, Other.HC, Other.HD), as.numeric)) %>%
    mutate(Notes = as.character(Notes)) %>%
    select(-contains("X"))
}

data_list_col_repair <- lapply(data_list, repair_col_names)

# Tidy tree IDs -----------------------------------------------------------

repair_tree_names <- function (plot, plot_name) {
  plot %>%
    # remove any letters
    mutate(Name = gsub("[a-zA-Z]+", "", x = Name, ignore.case = TRUE)) %>%
    # remove leading zeros
    mutate(Name = gsub("^0+", "", Name) ) %>%
    # rename 1st row as focal tree
    mutate(Name = replace(Name, pluck(1, 1),
                          paste0("focal_", readr::parse_number(plot_name)))) %>%
    mutate(Name = as.factor(Name))
}

data_list_name_repair <- imap(data_list_col_repair, repair_tree_names)

# Tidy rows ---------------------------------------------------------------

all_plots <- bind_rows(data_list_name_repair, .id = "plot")

all_plots %>%
  clean_names() %>%
  fill(c("name", "longitude", "latitude", "solution_status",
         "cs_name", "form", "reproductive")) %>%
  mutate(plot = gsub(".csv", "", x = plot)) %>%
  rename(tree_id = name) %>%
  group_by(plot) %>%
  mutate(stem_id = 1:n()) %>%
  ungroup() %>%
  add_count(tree_id, plot, name = "n_stems") %>%
  rowwise() %>%
  mutate(dbh = ifelse(is.na(dbh), cbh/pi, dbh)) %>%
  ungroup() %>%
  mutate(status = ifelse(is.na(status), "A", status)) %>%
  mutate_if(is.character, tolower) %>%
  mutate(tree_id = paste("tree", tree_id, sep = "_"),
         stem_id = paste("stem", stem_id, sep = "_")) -> clean_plots

# only keep the largest stem of a multi-stemmed tree
clean_plots %>%
  drop_na(dbh) %>% # this will drop dbh that were below breast height
  filter(status != "D") %>%
  group_by(plot, tree_id) %>%
  slice_max(dbh, with_ties = FALSE) %>%
  ungroup() -> largest_stems

saveRDS(largest_stems, file = here::here("data", "clean", "hawthorn_plots.rds"))
