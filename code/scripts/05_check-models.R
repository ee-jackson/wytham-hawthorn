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

# re-order list
model_list <- model_list[c("fruit_set_fit.rds",
                           "early_drop_fit.rds",
                           "late_drop_fit.rds")]


# MCMC diagnostics --------------------------------------------------------

plot_mcmc_check <- function(model, title) {
  mcmc_trace(model, regex_pars = "b_",
             iter1 = 1000) +
    ggtitle(title) +
    theme_classic(base_size = 5)
}

mcmc_plot_list <- map2(.x = model_list,
                       .y = names(model_list),
                       .f = plot_mcmc_check)

wrap_plots(mcmc_plot_list, ncol = 1)

ggsave(here::here("output","figures","mcmc_checks.png"),
       width = 1500, height = 2000, units = "px")


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
    mutate(across(Parameter, str_replace,
                  'connectivity', 'conspecific_density')) %>%
    gt()
}


# Plot for fruit set ------------------------------------------------------

set_t <- get_table(model_list$fruit_set_fit.rds)
gtsave(set_t, here::here("output", "results", "fruit_set.png"))
set_t_png <- png::readPNG(here::here("output", "results", "fruit_set.png"),
                          native = TRUE)

set_pp <- plot_pp_check(model_list$fruit_set_fit.rds)

(set_pp / set_t_png) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "fruit_set_si.png"),
  width = 500,
  height = 500,
  units = "px"
)


# early fruit drop --------------------------------------------------------

drop_t <- get_table(model_list$early_drop_fit.rds)
gtsave(drop_t, here::here("output", "results", "early_fruit_drop.png"))
drop_t_png <- png::readPNG(here::here("output", "results", "early_fruit_drop.png"),
                         native = TRUE)

drop_pp <- plot_pp_check(model_list$early_drop_fit.rds)

(drop_pp / drop_t_png) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "early_fruit_drop_si.png"),
  width = 500,
  height = 500,
  units = "px"
)


# late fruit drop ---------------------------------------------------------

drops_t <- get_table(model_list$late_drop_fit.rds)
gtsave(drops_t, here::here("output", "results", "late_fruit_drop.png"))
drops_t_png <- png::readPNG(here::here("output", "results", "late_fruit_drop.png"),
                            native = TRUE)

drops_pp <- plot_pp_check(model_list$late_drop_fit.rds)

(drops_pp / drops_t_png) +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(size = 20))

png(
  here::here("output", "figures", "late_fruit_drop_si.png"),
  width = 500,
  height = 500,
  units = "px"
)

