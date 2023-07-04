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


# Get models --------------------------------------------------------------

file_names <- as.list(dir(path = here::here("output", "models"),
                          pattern = "*.rds", full.names = TRUE))

model_list <- lapply(file_names, readRDS)

names(model_list) <- lapply(file_names, basename)


# Posterior predictive checks ---------------------------------------------

plot_pp_check <- function(model) {
  pp_check(model, ndraws = 50) +
    ggtitle(str_wrap(formula(model), 60)) +
    theme_classic(base_size = 5)
}

plot_list <- lapply(model_list, plot_pp_check)

wrap_plots(plot_list)

ggsave(here::here("output","figures","pp_checks.png"),
       width = 1476, height = 1000, units = "px")


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


# Check influence of prior information ------------------------------------

prior_predictor <- distribution_normal(n = 50, mean = 0, sd = 1)

prior_intercept <- distribution_student_t(n = 50, df = 3, ncp = 0)

plot_prior_check <- function(model, parameter, prior) {
  draws_fit <- as_draws_df(model, variable = parameter)

  ggplot() +
    geom_density(
      aes(x = draws_fit[[parameter]]),
      fill = "orange",
      linewidth = 0
    ) +
    geom_density(
      aes(x = prior),
      fill = "orange",
      alpha = 1 / 3,
      linewidth = 0
    ) +
    theme_classic(base_size = 5) +
    ggtitle(str_wrap(formula(model), 60)) +
    xlab(parameter)
}


plot_list_prior_1 <-
  lapply(model_list[c(1,3)], plot_prior_check, "b_repro_connectivity_sc", prior_predictor)

plot_list_prior_2 <-
  lapply(model_list, plot_prior_check, "b_dbh_sc", prior_predictor)

plot_list_prior_3 <-
  lapply(model_list, plot_prior_check, "b_Intercept", prior_intercept)

plot_list_prior_4 <-
  lapply(model_list[c(2,4)], plot_prior_check, "b_connectivity_sc", prior_predictor)

append(plot_list_prior_1, plot_list_prior_4) %>%
  append(plot_list_prior_2) %>%
  append(plot_list_prior_3) -> plot_list_prior_all

wrap_plots(plot_list_prior_all, ncol = 2)

ggsave(here::here("output","figures","prior_checks.png"),
       width = 1476, height = 2000, units = "px")
