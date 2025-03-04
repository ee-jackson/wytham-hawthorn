#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figures
## Desc:
## Date created: 2024-02-25

# packages ----------------------------------------------------------------

library("here")
library("tidyverse")
library("tidybayes")
library("ggdist")
library("ggblend")
library("patchwork")
library("viridisLite")
library("modelr")

# global parameters -------------------------------------------------------

font_size <- 23
pal <- viridisLite::viridis(n = 3, begin = 0.2, end = 0.8, option = "mako")
names(pal) <- c("2021", "2022", "2023")
dens_col <- "lightgrey"

# fruit set ---------------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_set_data.rds")) %>%
  mutate(
    non_repro_connectivity_sc = scale(non_repro_connectivity),
    repro_connectivity_sc = scale(repro_connectivity),
    dbh_sc = scale(dbh),
    tree_id = as.factor(tree_id)
  ) -> fruit_set_data

readRDS(here::here("output", "models",
                   "fruit_set_fit.rds")) -> fruit_set_mod


fruit_set_data %>%
  modelr::data_grid(
    non_repro_connectivity_sc = modelr::seq_range(non_repro_connectivity_sc, n = 5),
    repro_connectivity_sc = modelr::seq_range(repro_connectivity_sc, n = 5),
    n_flowers = modelr::seq_range(n_flowers, n = 5),
    dbh_sc = modelr::seq_range(dbh_sc, n = 5),
    year = ordered(c(2022, 2023))
  ) %>%
  mutate(n_flowers = as.integer(n_flowers)) %>%
  add_epred_draws(fruit_set_mod, re_formula = NA) %>%
  mutate(
    non_repro_connectivity_us = non_repro_connectivity_sc *
      attr(fruit_set_data$non_repro_connectivity_sc, 'scaled:scale') +
      attr(fruit_set_data$non_repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    repro_connectivity_us = repro_connectivity_sc *
      attr(fruit_set_data$repro_connectivity_sc, 'scaled:scale') +
      attr(fruit_set_data$repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    dbh_us = dbh_sc *
      attr(fruit_set_data$dbh_sc, 'scaled:scale') +
      attr(fruit_set_data$dbh_sc, 'scaled:center')
  ) -> fruit_set_pred

# panel a -----------------------------------------------------------------

tidybayes::tidy_draws(fruit_set_mod) %>%
  rename(
    `Diameter at\nbreast height` = b_dbh_sc,
    `Reproductive\nconspecific density` = b_repro_connectivity_sc,
    `Non-reproductive\nconspecific density` = b_non_repro_connectivity_sc,
    `Year` = `b_year2023`
  ) %>%
  select(
    `Reproductive\nconspecific density`,
    `Non-reproductive\nconspecific density`,
    `Diameter at\nbreast height`,
    `Year`
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = factor(
               parameter,
               levels = c(
                 "Year",
                 "Diameter at\nbreast height",
                 "Non-reproductive\nconspecific density",
                 "Reproductive\nconspecific density"
               )
             ))) +
  ggdist::stat_halfeye(
    aes(slab_alpha = after_stat(-pmax(abs(
      1 - 2 * cdf
    ), .95))),
    fill_type = "gradient",
    stroke = 2,
    size = 12,
    linewidth = 10,
    shape = 21,
    point_fill = "white", slab_fill = dens_col,
  ) +
  scale_slab_alpha_continuous(guide = "none") +
  theme_classic(base_size = font_size) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 1) +
  labs(x = "Parameter value", y = "") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 0.1,
      r = 0.7,
      b = 0.1,
      l = 0,
      "in"
    ),
    axis.text.y = element_text(colour = "black")
  ) -> pa

# panel b -----------------------------------------------------------------

fruit_set_pred |>
  mutate(year = as.factor(year)) |>
  group_by(year) |>
  ggplot(aes(x = repro_connectivity_us, fill = year, color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / n_flowers, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) |> adjust(aes(partition = year)) |> blend("multiply") +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = fruit_set_data,
             aes(
               x = repro_connectivity,
               y = n_immature_fruits / n_flowers,
               size = n_flowers,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Reproductive conspecific density") +
  ylab("Fruit set") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pb

# panel c -----------------------------------------------------------------

fruit_set_pred |>
  mutate(year = as.factor(year)) |>
  group_by(year) |>
  ggplot(aes(x = non_repro_connectivity_us,  fill = year, color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / n_flowers, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) |> adjust(aes(partition = year)) |> blend("multiply") +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = fruit_set_data,
             aes(
               x = non_repro_connectivity,
               y = n_immature_fruits / n_flowers,
               size = n_flowers,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Non-reproductive conspecific density") +
  ylab("Fruit set") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pc

# panel d -----------------------------------------------------------------

fruit_set_pred |>
  mutate(year = as.factor(year)) |>
  group_by(year) |>
  ggplot(aes(x = dbh_us,  fill = year, color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / n_flowers, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) |> adjust(aes(partition = year)) |> blend("multiply") +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = fruit_set_data,
             aes(
               x = dbh,
               y = n_immature_fruits / n_flowers,
               size = n_flowers,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Diameter at breast height /mm") +
  ylab("Fruit set") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pd

# fruit drop early --------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_drop_early.rds")) %>%
  mutate(repro_connectivity_sc = scale(repro_connectivity),
         non_repro_connectivity_sc = scale(non_repro_connectivity),
         dbh_sc = scale(dbh),
         tree_id = as.factor(tree_id)
  ) -> early_drop_data

readRDS(here::here("output", "models",
                   "early_drop_fit.rds")) -> early_drop_mod

early_drop_data %>%
  modelr::data_grid(
    non_repro_connectivity_sc = modelr::seq_range(non_repro_connectivity_sc, n = 5),
    repro_connectivity_sc = modelr::seq_range(repro_connectivity_sc, n = 5),
    total_fruit = modelr::seq_range(total_fruit, n = 5),
    dbh_sc = modelr::seq_range(dbh_sc, n = 5)
  ) %>%
  mutate(total_fruit = as.integer(total_fruit),
         year = factor(2023)) %>%
  add_epred_draws(early_drop_mod, re_formula = NA) %>%
  mutate(
    non_repro_connectivity_us = non_repro_connectivity_sc *
      attr(early_drop_data$non_repro_connectivity_sc, 'scaled:scale') +
      attr(early_drop_data$non_repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    repro_connectivity_us = repro_connectivity_sc *
      attr(early_drop_data$repro_connectivity_sc, 'scaled:scale') +
      attr(early_drop_data$repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    dbh_us = dbh_sc *
      attr(early_drop_data$dbh_sc, 'scaled:scale') +
      attr(early_drop_data$dbh_sc, 'scaled:center')
  ) -> early_drop_pred

# panel e -----------------------------------------------------------------

tidybayes::tidy_draws(early_drop_mod) %>%
  rename(
    `Diameter at\nbreast height` = b_dbh_sc,
    `Reproductive\nconspecific density` = b_repro_connectivity_sc,
    `Non-reproductive\nconspecific density` = b_non_repro_connectivity_sc
  ) %>%
  select(
    `Reproductive\nconspecific density`,
    `Non-reproductive\nconspecific density`,
    `Diameter at\nbreast height`
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = factor(
               parameter,
               levels = c(
                 "Diameter at\nbreast height",
                 "Non-reproductive\nconspecific density",
                 "Reproductive\nconspecific density"
               )
             ))) +
  ggdist::stat_halfeye(
    aes(slab_alpha = after_stat(-pmax(abs(
      1 - 2 * cdf
    ), .95))),
    fill_type = "gradient",
    stroke = 2,
    size = 12,
    linewidth = 10,
    shape = 21,
    point_fill = "white",
    slab_fill = dens_col,
  ) +
  scale_slab_alpha_continuous(guide = "none") +
  theme_classic(base_size = font_size) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 1) +
  labs(x = "Parameter value", y = "") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 0.1,
      r = 0.7,
      b = 0.1,
      l = 0,
      "in"
    ),
    axis.text.y = element_text(colour = "black")
  ) -> pe


# panel f -----------------------------------------------------------------

early_drop_pred |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = repro_connectivity_us, fill = year, colour = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = early_drop_data,
             aes(
               x = repro_connectivity,
               y = n_dropped / total_fruit,
               size = total_fruit,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Reproductive conspecific density") +
  ylab("Early fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pf

# panel g -----------------------------------------------------------------

early_drop_pred |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = non_repro_connectivity_us, fill = year, color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = early_drop_data,
             aes(
               x = non_repro_connectivity,
               y = n_dropped / total_fruit,
               size = total_fruit,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Non-reproductive conspecific density") +
  ylab("Early fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pg

# panel h -----------------------------------------------------------------

early_drop_pred |>
  mutate(year = as.factor(year)) |>
  group_by(year) |>
  ggplot(aes(x = dbh_us,  fill = year, color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit, fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = early_drop_data,
             aes(
               x = dbh,
               y = n_dropped / total_fruit,
               size = total_fruit,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Diameter at breast height /mm") +
  ylab("Early fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> ph


# fruit drop late ---------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_drop_late.rds")) %>%
  mutate(repro_connectivity_sc = scale(repro_connectivity),
         non_repro_connectivity_sc = scale(non_repro_connectivity),
         dbh_sc = scale(dbh),
         tree_id = as.factor(tree_id)
  ) -> late_drop_data

readRDS(here::here("output", "models",
                   "late_drop_fit.rds")) -> late_drop_mod

late_drop_data %>%
  modelr::data_grid(
    non_repro_connectivity_sc = modelr::seq_range(non_repro_connectivity_sc, n = 5),
    repro_connectivity_sc = modelr::seq_range(repro_connectivity_sc, n = 5),
    total_fruit = modelr::seq_range(total_fruit, n = 5),
    dbh_sc = modelr::seq_range(dbh_sc, n = 5)
  ) %>%
  mutate(total_fruit = as.integer(total_fruit),
         year = factor(2021)) %>%
  add_epred_draws(late_drop_mod, re_formula = NA) %>%
  mutate(
    non_repro_connectivity_us = non_repro_connectivity_sc *
      attr(late_drop_data$non_repro_connectivity_sc, 'scaled:scale') +
      attr(late_drop_data$non_repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    repro_connectivity_us = repro_connectivity_sc *
      attr(late_drop_data$repro_connectivity_sc, 'scaled:scale') +
      attr(late_drop_data$repro_connectivity_sc, 'scaled:center')
  ) %>%
  mutate(
    dbh_us = dbh_sc *
      attr(late_drop_data$dbh_sc, 'scaled:scale') +
      attr(late_drop_data$dbh_sc, 'scaled:center')
  ) %>%
  mutate(year = ordered("2021"),
         exclusion = as.factor("TRUE")) -> late_drop_pred

# panel i -----------------------------------------------------------------

tidybayes::tidy_draws(late_drop_mod) %>%
  rename(
    `Diameter at\nbreast height` = b_dbh_sc,
    `Reproductive\nconspecific density` = b_repro_connectivity_sc,
    `Non-reproductive\nconspecific density` = b_non_repro_connectivity_sc
  ) %>%
  select(
    `Reproductive\nconspecific density`,
    `Non-reproductive\nconspecific density`,
    `Diameter at\nbreast height`
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = factor(
               parameter,
               levels = c(
                 "Diameter at\nbreast height",
                 "Non-reproductive\nconspecific density",
                 "Reproductive\nconspecific density"
               )
             ))) +
  ggdist::stat_halfeye(
    aes(slab_alpha = after_stat(-pmax(abs(
      1 - 2 * cdf
    ), .95))),
    fill_type = "gradient",
    stroke = 2,
    size = 12,
    linewidth = 10,
    shape = 21,
    point_fill = "white",
    slab_fill = dens_col,
  ) +
  scale_slab_alpha_continuous(guide = "none") +
  theme_classic(base_size = font_size) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 1) +
  labs(x = "Parameter value", y = "") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 0.1,
      r = 0.7,
      b = 0.1,
      l = 0,
      "in"
    ),
    axis.text.y = element_text(colour = "black")
  ) -> pi


# panel j -----------------------------------------------------------------

late_drop_pred |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = repro_connectivity_us,
             fill = year,
             color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit,
        fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = late_drop_data,
             aes(
               x = repro_connectivity,
               y = n_dropped / total_fruit,
               size = total_fruit,
               colour = year,
               shape = exclusion
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Reproductive conspecific density") +
  ylab("Late fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pj

# panel k -----------------------------------------------------------------

late_drop_pred |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = non_repro_connectivity_us,
             fill = year,
             color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit,
        fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = late_drop_data,
             aes(
               x = non_repro_connectivity,
               y = n_dropped / total_fruit,
               size = total_fruit,
               shape = exclusion,
               colour = year
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape= 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Non-reproductive conspecific density") +
  ylab("Late fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pk

# panel l -----------------------------------------------------------------

late_drop_pred |>
  mutate(year = as.factor(year)) |>
  ggplot(aes(x = dbh_us,
             fill = year,
             color = year)) +
  ggdist::stat_lineribbon(show.legend = TRUE,
    aes(y = .epred / total_fruit,
        fill_ramp = after_stat(.width)),
    .width = ppoints(40) ) +
  ggdist::scale_fill_ramp_continuous(range = c(0.8, 0), guide = "none") +
  geom_point(data = late_drop_data,
             aes(
               x = dbh,
               y = n_dropped / total_fruit,
               size = total_fruit,
               colour = year,
               shape = exclusion
             ),
             show.legend = FALSE,
             inherit.aes = FALSE,
             alpha = 0.8,
             shape = 16
  ) +
  scale_colour_manual(values = pal, drop = FALSE,
                      limits = names(pal)) +
  scale_fill_manual(values = pal, drop = FALSE,
                    limits = names(pal)) +
  theme_classic(base_size = font_size) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0.005, 0.005)) +
  xlab("Diameter at breast height /mm") +
  ylab("Late fruit drop") +
  guides(colour = guide_legend("Year"), fill = guide_legend("Year"),
         size = "none") -> pl


# calendar ----------------------------------------------------------------

survey_segs <- tibble(
  year = factor(c(2022, 2023, 2023, 2021)),
  start = as.Date(c("2023-05-09", "2023-05-31", "2023-06-30", "2023-08-05"),
                  format = "%Y-%m-%d"),
  end = as.Date(c("2023-06-17", "2023-06-29", "2023-09-29", "2023-10-29"),
                format = "%Y-%m-%d"),
  type = factor(c("Fruit set", " Fruit set", "Early fruit drop",
                  "Late fruit drop"))
)

ggplot(survey_segs,
       aes(x = start, xend = end,
           y = reorder(type, start, decreasing = TRUE), yend = type,
           colour = year, group = type)) +
  geom_segment(linewidth = 5, lineend = "round") +
  scale_x_date(date_breaks = "1 month",  expand = c(0,0),
               date_labels = "%b",
               limits = as.Date(c("2023-05-01", "2023-11-01"))) +
  scale_color_manual(values = pal) +
  theme_classic(base_size = font_size) +
  theme(panel.grid.major.y = element_line(linetype = 2),
        axis.text.y = element_text(colour = "black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  guides(linetype = "none", colour = "none") -> cal


# combine -----------------------------------------------------------------

layout2 <- "
AAA
BCD
BCD
FGH
FGH
JKL
JKL
NOP
NOP
"


patchwork::free(cal) +
  patchwork::free(pa) + patchwork::free(pe) + patchwork::free(pi) +
  pb + pf + pj +
  pc + pg + pk +
  pd + ph + pl +
  plot_layout(design = layout2,
              guides = "collect",
              axis_titles = "collect") +
  patchwork::plot_annotation(tag_levels = "a") &
  theme(legend.text = element_text(size = 20),
        legend.title = element_text(size = 25),
        legend.position = "top",
        legend.justification = "left") -> test_full


png(
  here::here("output", "figures", "model_preds_new.png"),
  width = 1476,
  height = 1800,
  units = "px",
  type = "cairo"
)
test_full
dev.off()
