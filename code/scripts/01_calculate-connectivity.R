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

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0" | reproductive == "y") %>%
  group_split(plot) -> plot_dfs_repro

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0" | reproductive == "n") %>%
  group_split(plot) -> plot_dfs_non_repro


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

# all trees
dist_dfs <- lapply(plot_dfs, calculate_dist)

dist_dfs_focal <- map(dist_dfs, ~select(.x, plot, tree_id, tree_0, dbh))

# reproductive trees only
dist_dfs_repro <- lapply(plot_dfs_repro, calculate_dist)

dist_dfs_focal_repro <- map(dist_dfs_repro,
                            ~select(.x, plot, tree_id, tree_0, dbh))

# non-reproductive trees only
dist_dfs_non_repro <- lapply(plot_dfs_non_repro, calculate_dist)

dist_dfs_focal_non_repro <- map(dist_dfs_non_repro,
                            ~select(.x, plot, tree_id, tree_0, dbh))


# Calculate connectivity --------------------------------------------------

calculate_connectivity <- function (data) {
  data %>%
    filter(tree_id != "tree_0") %>%
    rename(distance = tree_0) %>%
    filter(distance <= 50) %>%
    mutate(x = exp(- 0.02 * distance ) * dbh) %>%
    reframe(connectivity = sum(x),
              plot = plot) %>%
    distinct()
}

# all trees
connectivity_dfs <- lapply(dist_dfs_focal, calculate_connectivity)

connectivity_dfs %>%
  dplyr::bind_rows() %>%
  mutate(plot = as.numeric(plot)) -> all_connectivity_dfs

# reproductive trees only
connectivity_dfs_repro <- lapply(dist_dfs_focal_repro, calculate_connectivity)

connectivity_dfs_repro %>%
  dplyr::bind_rows() %>%
  mutate(plot = as.numeric(plot)) %>%
  rename(repro_connectivity = connectivity) -> all_connectivity_dfs_repro

# non-reproductive trees only
connectivity_dfs_non_repro <- lapply(dist_dfs_focal_non_repro, calculate_connectivity)

connectivity_dfs_non_repro %>%
  dplyr::bind_rows() %>%
  mutate(plot = as.numeric(plot)) %>%
  rename(non_repro_connectivity = connectivity) -> all_connectivity_dfs_non_repro


# Calculate distance to neighbours ----------------------------------------

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  mutate(plot = as.numeric(plot)) %>%
  filter(plot %%1 != 0) %>%
  filter(tree_id == "tree_0" |
           tree_id == "tree_f" ) %>%
  group_split(plot) -> non_focal

lapply(non_focal, calculate_dist) %>%
  dplyr::bind_rows() %>%
  filter(tree_f > 0 ) %>%
  rename(dist_to_focal = tree_f) %>%
  select(plot, dist_to_focal) %>%
  filter(dist_to_focal > 6) %>%
  pull(plot) -> to_drop


# Combine and save --------------------------------------------------------

all_connectivity_dfs_repro %>%
  full_join(all_connectivity_dfs, by = "plot") %>%
  full_join(all_connectivity_dfs_non_repro, by = "plot") %>%
  filter(! plot %in% to_drop) %>%
  mutate(non_repro_connectivity = replace_na(non_repro_connectivity, 0)) %>%
  saveRDS(here::here("data", "clean", "connectivity_data.rds"))
