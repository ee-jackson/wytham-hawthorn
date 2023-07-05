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
library("sf")
library("modelsummary")


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

readRDS(here::here("data", "clean", "hawthorn_plots.rds")) %>%
  filter(tree_id == "tree_0") %>%
  filter(near(as.numeric(plot), as.integer(as.numeric(plot))) == TRUE) %>%
  filter(plot != "34") %>%
  filter(plot != "35") -> mapping_data

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
mapping_data %>%
  st_as_sf(coords = c('longitude', 'latitude')) %>%
  st_set_crs(
    "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  ) -> mapping_data_geo

st_buffer(mapping_data_geo$geometry, units::as_units(50, "meter")) -> buffs

png(here::here("output","figures","map.png"),
    width = 1476, height = 1476, units = "px", bg = "transparent")
plot(
  st_transform(buffs, crs = 3857),
  bgMap = wytham_basemap,
  col = adjustcolor("#2171b5", .4),
  border = FALSE, plot = FALSE
)
plot(
  st_geometry(st_centroid(st_transform(buffs, crs = 3857))),
  pch = 4,
  cex = 2,
  col = "black",
  lwd = 3,
  add = TRUE
)
dev.off()

## map with points coloured by connectivity
#
# st_buffer(mapping_data_geo$geometry, units::as_units(50, "meter")) -> mapping_data_geo$buffs
#
# readRDS(here::here("data", "clean", "connectivity_data.rds")) -> conn_dat
#
# mapping_data_geo %>%
#   mutate(plot = as.numeric(plot)) %>%
#   left_join(conn_dat) -> gg_data
#
# ggplot() +
#   geom_sf(aes(fill = gg_data$connectivity), data = gg_data$buffs, colour = NA) +
#   geom_sf(data = gg_data$geometry, shape = 4, colour = "white") +
#   scale_fill_viridis_c(option = "C") +
#   theme_classic()
