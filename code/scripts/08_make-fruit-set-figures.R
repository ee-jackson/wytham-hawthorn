#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: make-figures
## Desc:
## Date created: 2023-03-29

# packages ----------------------------------------------------------------

library("here")
library("tidyverse")
library("tidybayes")
library("ggdist")
library("patchwork")


# Get data ----------------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_set_data.rds")) %>%
  filter(bagged == FALSE) %>%
  filter(year == 2023) %>%
  filter(!(tree_id %% 1)) %>%
  filter(n_flowers != 0) %>%
  mutate(
    connectivity_sc = scale(connectivity),
    repro_connectivity_sc = scale(repro_connectivity),
    dbh_sc = scale(dbh),
    tree_id = as.factor(tree_id)
  ) -> fruit_set_data

readRDS(here::here("output", "models",
                   "fruit_set_repro_fit.rds")) -> fruit_set_repro_mod

readRDS(here::here("output", "models",
                   "fruit_set_tot_fit.rds")) -> fruit_set_tot_mod


# Panel a, total connectivity parameter estimates -------------------------

tidybayes::tidy_draws(fruit_set_tot_mod) %>%
  rename(
    `Diameter at breast height` = b_dbh_sc,
    `Total connectivity` = b_connectivity_sc ,
    `Diameter at breast height :\nTotal connectivity` = `b_connectivity_sc:dbh_sc`
  ) %>%
  select(
    `Total connectivity`,
    `Diameter at breast height`,
    `Diameter at breast height :\nTotal connectivity`
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = factor(
               parameter,
               levels = c(
                 "Diameter at breast height :\nTotal connectivity",
                 "Diameter at breast height",
                 "Total connectivity"
               )
             ))) +
  ggdist::stat_halfeye(
    aes(fill = parameter, slab_alpha = after_stat(-pmax(abs(
      1 - 2 * cdf
    ), .95))),
    fill_type = "gradient",
    stroke = 2,
    size = 20,
    linewidth = 10,
    shape = 21,
    point_fill = "white"
  ) +
  scale_slab_alpha_continuous(guide = "none") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  theme_classic(base_size = 30) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 1) +
  labs(x = "Possible parameter values", y = "") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 0.1,
      r = 0.4,
      b = 0.1,
      l = 0,
      "in"
    ),
    axis.text.y = element_text(colour = "black")
  ) +
  labs(tag = "a") -> p2a


# Panel b, total connectivity ---------------------------------------------

fruit_set_data %>%
  modelr::data_grid(
    connectivity_sc = modelr::seq_range(connectivity_sc, n = 51),
    n_flowers = modelr::seq_range(n_flowers, n = 51),
    dbh_sc = rep(mean(fruit_set_data$dbh_sc), 51)
  ) %>%
  mutate(n_flowers = as.integer(n_flowers)) %>%
  add_epred_draws(fruit_set_tot_mod, ndraws = 500, re_formula = NA) %>%
  mutate(
    connectivity_us = connectivity_sc *
      attr(fruit_set_data$connectivity_sc, 'scaled:scale') +
      attr(fruit_set_data$connectivity_sc, 'scaled:center')
  ) %>%
  ggplot(aes(
    x = connectivity_us,
    y = .epred / n_flowers,
    fill_ramp = after_stat(.width)
  )) +
  stat_lineribbon(.width = ppoints(50), fill = "#009E73") +
  scale_fill_ramp_continuous(range = c(1, 0), guide = guide_rampbar(to = "#009E73")) +
  geom_point(
    data = fruit_set_data,
    aes(
      x = connectivity,
      y = n_immature_fruits / n_flowers,
      size = n_flowers
    ),
    inherit.aes = FALSE,
    alpha = 0.7,
    shape = 16
  ) +
  theme_classic(base_size = 30) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  xlab("Total connectivity") +
  ylab("Proportion of flowers turning to fruits") +
  theme(legend.position = "none") +
  labs(tag = "b") -> p2b


# Panel c, DBH total connectivity -----------------------------------------

fruit_set_data %>%
  modelr::data_grid(
    dbh_sc = modelr::seq_range(dbh_sc, n = 51),
    n_flowers = modelr::seq_range(n_flowers, n = 51),
    connectivity_sc = rep(mean(fruit_set_data$connectivity_sc), 51)
  ) %>%
  mutate(n_flowers = as.integer(n_flowers)) %>%
  add_epred_draws(fruit_set_tot_mod, ndraws = 500, re_formula = NA) %>%
  mutate(
    dbh_us = dbh_sc *
      attr(fruit_set_data$dbh_sc, 'scaled:scale') +
      attr(fruit_set_data$dbh_sc, 'scaled:center')
  ) %>%
  ggplot(aes(
    x = dbh_us,
    y = .epred / n_flowers,
    fill_ramp = after_stat(.width)
  )) +
  stat_lineribbon(.width = ppoints(50), fill = "#E69F00") +
  scale_fill_ramp_continuous(range = c(1, 0), guide = guide_rampbar(to = "#E69F00")) +
  geom_point(
    data = fruit_set_data,
    aes(
      x = dbh,
      y = n_immature_fruits / n_flowers,
      size = n_flowers
    ),
    inherit.aes = FALSE,
    alpha = 0.7,
    shape = 16
  ) +
  theme_classic(base_size = 30) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  xlab("Diameter at breast height /mm") +
  ylab("Proportion of flowers turning to fruits") +
  theme(legend.position = "none") +
  labs(tag = "c") -> p2c


# Panel d, reproductive connectivity parameter estimates ------------------

tidybayes::tidy_draws(fruit_set_repro_mod) %>%
  rename(
    `Diameter at breast height` = b_dbh_sc,
    `Reproductive connectivity` = b_repro_connectivity_sc ,
    `Diameter at breast height :\nReproductive connectivity` = `b_repro_connectivity_sc:dbh_sc`
  ) %>%
  select(
    `Reproductive connectivity`,
    `Diameter at breast height`,
    `Diameter at breast height :\nReproductive connectivity`
  ) %>%
  pivot_longer(cols = everything(), names_to = "parameter") %>%
  ggplot(aes(x = value,
             y = factor(
               parameter,
               levels = c(
                 "Diameter at breast height :\nReproductive connectivity",
                 "Diameter at breast height",
                 "Reproductive connectivity"
               )
             ))) +
  ggdist::stat_halfeye(
    aes(fill = parameter, slab_alpha = after_stat(-pmax(abs(
      1 - 2 * cdf
    ), .95))),
    fill_type = "gradient",
    stroke = 2,
    size = 20,
    linewidth = 10,
    shape = 21,
    point_fill = "white"
  ) +
  scale_slab_alpha_continuous(guide = "none") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  theme_classic(base_size = 30) +
  geom_vline(xintercept = 0,
             linetype = 2,
             linewidth = 1) +
  labs(x = "Possible parameter values", y = "") +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 0.1,
      r = 0.4,
      b = 0.1,
      l = 0,
      "in"
    ),
    axis.text.y = element_text(colour = "black")
  ) +
  labs(tag = "d") -> p2d


# Panel e, reproductive connectivity --------------------------------------

fruit_set_data %>%
  modelr::data_grid(
    repro_connectivity_sc = modelr::seq_range(repro_connectivity_sc, n = 51),
    n_flowers = modelr::seq_range(n_flowers, n = 51),
    dbh_sc = rep(mean(fruit_set_data$dbh_sc), 51)
  ) %>%
  mutate(n_flowers = as.integer(n_flowers)) %>%
  add_epred_draws(fruit_set_repro_mod,
                  ndraws = 500,
                  re_formula = NA) %>%
  mutate(
    repro_connectivity_us = repro_connectivity_sc *
      attr(fruit_set_data$repro_connectivity_sc, 'scaled:scale') +
      attr(fruit_set_data$repro_connectivity_sc, 'scaled:center')
  ) %>%
  ggplot(aes(
    x = repro_connectivity_us,
    y = .epred / n_flowers,
    fill_ramp = after_stat(.width)
  )) +
  stat_lineribbon(.width = ppoints(50), fill = "#009E73") +
  scale_fill_ramp_continuous(range = c(1, 0), guide = guide_rampbar(to = "#009E73")) +
  geom_point(
    data = fruit_set_data,
    aes(
      x = repro_connectivity,
      y = n_immature_fruits / n_flowers,
      size = n_flowers
    ),
    inherit.aes = FALSE,
    alpha = 0.7,
    shape = 16
  ) +
  theme_classic(base_size = 30) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  xlab("Reproductive connectivity") +
  ylab("Proportion of flowers turning to fruits") +
  theme(legend.position = "none") +
  labs(tag = "e") -> p2e


# Panel f, DBH reproductive connectivity ----------------------------------

fruit_set_data %>%
  modelr::data_grid(
    dbh_sc = modelr::seq_range(dbh_sc, n = 51),
    n_flowers = modelr::seq_range(n_flowers, n = 51),
    repro_connectivity_sc = rep(mean(fruit_set_data$repro_connectivity_sc), 51)
  ) %>%
  mutate(n_flowers = as.integer(n_flowers)) %>%
  add_epred_draws(fruit_set_repro_mod,
                  ndraws = 500,
                  re_formula = NA) %>%
  mutate(
    dbh_us = dbh_sc *
      attr(fruit_set_data$dbh_sc, 'scaled:scale') +
      attr(fruit_set_data$dbh_sc, 'scaled:center')
  ) %>%
  ggplot(aes(
    x = dbh_us,
    y = .epred / n_flowers,
    fill_ramp = after_stat(.width)
  )) +
  stat_lineribbon(.width = ppoints(50), fill = "#E69F00") +
  scale_fill_ramp_continuous(range = c(1, 0), guide = guide_rampbar(to = "#E69F00")) +
  geom_point(
    data = fruit_set_data,
    aes(
      x = dbh,
      y = n_immature_fruits / n_flowers,
      size = n_flowers
    ),
    inherit.aes = FALSE,
    alpha = 0.7,
    shape = 16
  ) +
  theme_classic(base_size = 30) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  xlab("Diameter at breast height /mm") +
  ylab("Proportion of flowers turning to fruits") +
  theme(legend.position = "none") +
  labs(tag = "f") -> p2f


# Combine panels ----------------------------------------------------------

png(
  here::here("output", "figures", "fruit_set_6_effects_colour.png"),
  width = 1476,
  height = 1800,
  units = "px",
  type = "cairo"
)
(p2a + p2d) / wrap_elements(full = (p2b + p2e)) /  wrap_elements(full = (p2c + p2f)) +
  plot_layout(heights = c(1, 1.5, 1.5))
dev.off()
