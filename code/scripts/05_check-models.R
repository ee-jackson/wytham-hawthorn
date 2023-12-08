#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: check-models
## Desc: perform posterior predictive checks
## Date created: 2023-03-31

# packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("brms")
library("patchwork")
library("stringr")
library("bayesplot")
library("bayestestR")
library("gt")


# Get models --------------------------------------------------------------

file_names <- as.list(dir(path = here::here("output", "models"),
                          pattern = "*.rds", full.names = TRUE))

model_list <- lapply(file_names, readRDS)

names(model_list) <- lapply(file_names, basename)


# MCMC diagnostics --------------------------------------------------------

plot_mcmc_check <- function(model) {
  mcmc_trace(model, regex_pars = "b_",
             iter1 = 1000) +
    ggtitle(str_wrap(formula(model), 70)) +
    theme_classic(base_size = 5)
}

mcmc_plot_list <- lapply(model_list, plot_mcmc_check)

wrap_plots(mcmc_plot_list, ncol = 2)

ggsave(here::here("output","figures","mcmc_checks.png"),
       width = 1961, height = 1500, units = "px")


# Posterior predictive checks ---------------------------------------------

plot_pp_check <- function(model) {
  pp_check(model, ndraws = 50) +
    labs(x = "Response", y = "Density") +
    theme_classic(base_size = 15)
}


# Get posterior param estimates -------------------------------------------

get_table <- function(model) {
  bayestestR::describe_posterior(model,
                                 ci = 0.95,
                                 ci_method = "HDI",
                                 centrality = "median",
                                 test = FALSE) %>%
    mutate(across(!Rhat & !Parameter, round, 2)) %>%
    gt()
}


# Plot for fruit drop -----------------------------------------------------

drop_r_t <- get_table(model_list$fruit_drop_fit.rds)
gtsave(drop_r_t, here::here("output", "results", "fruit_drop_repro.png"))
drop_r_t_png <- png::readPNG(here::here("output", "results", "fruit_drop_repro.png"),
                         native = TRUE)

drop_r_pp <- plot_pp_check(model_list$fruit_drop_fit.rds)

drop_t_t <- get_table(model_list$fruit_drop_tot_fit.rds)
gtsave(drop_t_t, here::here("output", "results", "fruit_drop_total.png"))
drop_t_t_png <- png::readPNG(here::here("output", "results", "fruit_drop_total.png"),
                         native = TRUE)

drop_t_pp <- plot_pp_check(model_list$fruit_drop_tot_fit.rds)

((drop_r_pp / drop_r_t_png) | (drop_t_pp / drop_t_t_png)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "fruit_drop_si.png"),
  width = 1000,
  height = 500,
  units = "px"
)


# Plot for fruit set ------------------------------------------------------

set_r_t <- get_table(model_list$fruit_set_repro_fit.rds)
gtsave(set_r_t, here::here("output", "results", "fruit_set_repro.png"))
set_r_t_png <- png::readPNG(here::here("output", "results", "fruit_set_repro.png"),
                           native = TRUE)

set_r_pp <- plot_pp_check(model_list$fruit_set_repro_fit.rds)

set_t_t <- get_table(model_list$fruit_set_tot_fit.rds)
gtsave(set_t_t, here::here("output", "results", "fruit_set_total.png"))
set_t_t_png <- png::readPNG(here::here("output", "results", "fruit_set_total.png"),
                           native = TRUE)

set_t_pp <- plot_pp_check(model_list$fruit_set_tot_fit.rds)

((set_r_pp / set_r_t_png) | (set_t_pp / set_t_t_png)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "fruit_set_si.png"),
  width = 1000,
  height = 500,
  units = "px"
)


# Plot for short fruit drop -----------------------------------------------

drops_r_t <- get_table(model_list$fruit_drop_short_fit.rds)
gtsave(drops_r_t, here::here("output", "results", "fruit_drops_repro.png"))
drops_r_t_png <- png::readPNG(here::here("output", "results", "fruit_drops_repro.png"),
                            native = TRUE)

drops_r_pp <- plot_pp_check(model_list$fruit_drop_short_fit.rds)

drops_t_t <- get_table(model_list$fruit_drop_short_tot_fit.rds)
gtsave(drops_t_t, here::here("output", "results", "fruit_drops_total.png"))
drops_t_t_png <- png::readPNG(here::here("output", "results", "fruit_drops_total.png"),
                            native = TRUE)

drops_t_pp <- plot_pp_check(model_list$fruit_drop_short_tot_fit.rds)

((drops_r_pp / drops_r_t_png) | (drops_t_pp / drops_t_t_png)) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "fruit_drop_short_si.png"),
  width = 1000,
  height = 500,
  units = "px"
)
