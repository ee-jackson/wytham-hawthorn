#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figures
## Desc:
## Date created: 2023-03-29


# packages ----------------------------------------------------------------

library("here")
library("tidyverse")
library("kableExtra")
library("ggmap")
library("ggsn")
library("sf")
library("modelsummary")
library("patchwork")


# Summarise data ----------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  filter(exclusion == TRUE) %>%
  drop_na() %>%
  group_by(tree_id) %>%
  summarise(
    sum_dropped = sum(n_dropped, na.rm = TRUE),
    sum_total = sum(total_fruit, na.rm = TRUE),
    .groups = "rowwise"
  ) %>%
  mutate(`Proportion of fruit dropped per tree` =
           sum_dropped / sum_total) %>%
  ungroup() -> fruit_drop_plot

readRDS(here::here("data", "clean", "fruit_set_data.rds")) %>%
  filter(bagged == FALSE) %>%
  filter(year == 2023) %>%
  filter(n_flowers != 0) %>%
  filter(!(tree_id %% 1)) %>%
  group_by(tree_id) %>%
  summarise(sum_flowers = sum(n_flowers, na.rm = TRUE),
            sum_fruits = sum(n_immature_fruits, na.rm = TRUE),
            .groups = "rowwise") %>%
  mutate(`Proportion of flowers turning into fruits per tree` =
           sum_fruits / sum_flowers) %>%
  ungroup() %>%
  drop_na() -> fruit_set_plot

full_join(fruit_set_plot, fruit_drop_plot) -> all_plotting_data


# Make table --------------------------------------------------------------

emptycol <- function(x) " "

tmp_list <- as.list(select(all_plotting_data,
                           `Proportion of flowers turning into fruits per tree`,
                           `Proportion of fruit dropped per tree` ))


datasummary(
  `Proportion of flowers turning into fruits per tree` +
    `Proportion of fruit dropped per tree` ~ Mean + SD + N +
    Heading("Boxplot") * emptycol +
    Heading("Histogram") * emptycol,
  data = all_plotting_data
) %>%
  kable_classic() %>%
  column_spec(column = 1, width = "10em") %>%
  column_spec(
    column = 5,
    image = spec_boxplot(
      tmp_list,
      width = 700,
      height = 300,
      add_label = TRUE,
      same_lim = FALSE
    )
  ) %>%
  column_spec(
    column = 6,
    image = spec_hist(
      tmp_list,
      breaks = 10,
      width = 700,
      col = "lightgray",
      border = "lightgray",
      height = 200,
      same_lim = FALSE
    )
  ) %>%
  kable_styling(font_size = 16, html_font = "arial") %>%
  kableExtra::as_image(file = here::here("output", "figures", "summary_stats.png"),
                       width = 4.92)

# for kableExtra::as_image units are in inches


# Make map ----------------------------------------------------------------

# get locations of trees and connectivity

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  filter(near(as.numeric(plot), as.integer(as.numeric(plot))) == TRUE) %>%
  filter(plot != "34") %>%
  filter(plot != "35") -> mapping_data

readRDS(here::here("data", "clean", "connectivity_data.rds")) -> conn_dat

mapping_data %>%
  mutate(plot = as.numeric(plot)) %>%
  left_join(conn_dat) -> mapping_data_con

mapping_data %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(
    "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ) -> mapping_data_geo

mapping_data_geo %>%
  mutate(plot = as.numeric(plot)) %>%
  left_join(conn_dat) -> gg_data

# get a basemap of Wytham woods

bbox <-
  make_bbox(c(
    min(mapping_data$longitude) - 0.001,
    max(mapping_data$longitude) + 0.001
  ),
  c(
    min(mapping_data$latitude) - 0.001,
    max(mapping_data$latitude) + 0.001
  ))


wytham_basemap <- ggmap::get_map(bbox,
                                 force = TRUE,
                                 source = "stamen",
                                 maptype = "terrain")

# plot trees on Wytham map

ggmap(wytham_basemap) +
  geom_point(aes(x = longitude, y = latitude,
                 colour = connectivity, size = dbh),
             data = mapping_data_con, alpha = 0.9) +
  scale_colour_viridis_c(option = "D") +
  ggsn::scalebar(gg_data,
                 location = "bottomright",
                 dist = 250,
                 dist_unit = "m",
                 transform = TRUE,
                 st.size = 2.5,
                 border.size = 0.5,
                 model = "WGS84") +
  labs(colour = "Total connectivity") +
  labs(size = "DBH /mm") +
  theme_void(base_size = 10) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.border = element_rect(colour = "black",
                                fill = NA,
                                linewidth = 1)) -> wytham_map

# get a map of the UK

uk <- c(left = -6.0, bottom = 50, right = 2.5, top = 55)

uk_basemap <- ggmap::get_stamenmap(uk, zoom = 8,
                                 force = TRUE,
                                 maptype = "terrain-background")

ggmap(uk_basemap) +
  geom_point(aes(x = -1.329375, y = 51.769244),
             size = 4, alpha = 0.6, shape = 4) +
  theme_void() +
  theme(
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black",
                                    fill = NA,
                                    linewidth = 1)) -> uk_map

# inset the UK map in the Wytham map

wytham_map + inset_element(uk_map, left = 0.5, bottom = 0.5,
                           right = 1.05, top = 1,
                           align_to = "plot")

ggsave(here::here("output","figures","map_new.png"),
       width = 1476, height = 1000, units = "px")
