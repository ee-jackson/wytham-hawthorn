#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: calculate-connectivity
## Desc:
## Date created: 2023-02-09


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("geosphere")


# Get data ----------------------------------------------------------------

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  group_split(plot) -> plot_dfs

readRDS(here::here("data", "clean", "fruit_drop_data.rds")) -> fruit_data

# Calculate pairwise distances --------------------------------------------

calculate_dist <- function (data) {

  data %>%
    select(longitude, latitude) -> plot_matrix

  geosphere::distm(plot_matrix,
                   fun = distGeo) -> dists

  as.data.frame(dists) -> dists_df

  unlist(data$tree_id) -> colnames(dists_df)

  cbind(data, dists_df)

}

dist_dfs <- lapply(plot_dfs, calculate_dist)

dist_dfs_focal <- map(dist_dfs, ~select(.x, plot, tree_id, tree_0, dbh))


# Calculate connectivity --------------------------------------------------

calculate_connectivity <- function (data) {
  data %>%
    filter(tree_id != "tree_0") %>%
    rename(distance = tree_0) %>%
    filter(distance <= 50) %>%
    mutate(x = exp(- 0.02 * distance ) * dbh ) %>%
    summarise(connectivity = sum(x),
              plot = plot) %>%
    distinct()
}

connectivity_dfs <- lapply(dist_dfs_focal, calculate_connectivity)

connectivity_dfs %>%
  dplyr::bind_rows() %>%
  mutate(plot = as.numeric(plot)) -> all_connectivity_dfs


# Combine with fruit data -------------------------------------------------

fruit_data %>%
  right_join(all_connectivity_dfs, by = c("tree_id" = "plot")) %>%
  mutate(tree_id = as.factor(tree_id)) %>%
  saveRDS(here::here("data", "clean", "connectivity_data.rds"))
