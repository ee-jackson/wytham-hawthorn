#!/usr/bin/env Rscript

## Author: E E Jackson, eleanor.elizabeth.j@gmail.com
## Script: fit-models
## Desc:
## Date created: 2023-02-13


# Packages ----------------------------------------------------------------

library("tidyverse")
library("here")
library("brms")
library("bayestestR")

options(brms.file_refit = "on_change")


# Get data ----------------------------------------------------------------

readRDS(here::here("data", "clean", "fruit_set_data.rds")) %>%
  mutate(repro_connectivity_sc = scale(repro_connectivity),
         non_repro_connectivity_sc = scale(non_repro_connectivity),
         dbh_sc = scale(dbh),
         tree_id = as.factor(tree_id),
         year = as.factor(year)
  ) -> fruit_set_data

readRDS(here::here("data", "clean", "fruit_drop_data.rds")) %>%
  mutate(repro_connectivity_sc = scale(repro_connectivity),
         non_repro_connectivity_sc = scale(non_repro_connectivity),
         dbh_sc = scale(dbh),
         tree_id = as.factor(tree_id)
           ) -> fruit_drop_data

readRDS(here::here("data", "clean", "fruit_drop_data_short.rds")) %>%
  mutate(repro_connectivity_sc = scale(repro_connectivity),
         non_repro_connectivity_sc = scale(non_repro_connectivity),
         dbh_sc = scale(dbh),
         tree_id = as.factor(tree_id),
         year = as.factor(year)
  ) -> fruit_drop_data_short


# Prior -------------------------------------------------------------------

bprior <- c(prior(normal(0, 1), class = b))


# fruit set ---------------------------------------------------------------

fruit_set_mod <-
  brm(data = fruit_set_data,
      family = binomial(link = logit),
      n_immature_fruits | trials(n_flowers) ~
        repro_connectivity_sc +
        non_repro_connectivity_sc +
        dbh_sc + year + (1|tree_id),
      prior = bprior,
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      seed = 9,
      file = (here::here("output", "models", "fruit_set_fit.rds")))

summary(fruit_set_mod)

bayestestR::describe_posterior(
  fruit_set_mod,
  ci = 0.95,
  ci_method = "HDI",
  centrality = "median"
) %>%
  as.data.frame() %>%
  write_csv(here::here(
    "output",
    "results",
    "fruit_set_describe_posterior.csv"
  ))


# fruit drop --------------------------------------------------------------

fruit_drop_mod <-
  brm(data = fruit_drop_data,
      family = binomial(link = logit),
      n_dropped | trials(total_fruit) ~
        repro_connectivity_sc +
        non_repro_connectivity_sc +
        dbh_sc + (1|tree_id),
      prior = bprior,
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      seed = 9,
      file = (here::here("output", "models", "fruit_drop_fit.rds")))

summary(fruit_drop_mod)

bayestestR::describe_posterior(fruit_drop_mod,
                               ci = 0.95,
                               ci_method = "HDI",
                               centrality = "median") %>%
  as.data.frame() %>%
  write_csv(here::here("output", "results", "fruit_drop_describe_posterior.csv"))


# short fruit drop --------------------------------------------------------

fruit_drop_short_mod <-
  brm(data = fruit_drop_data_short,
      family = binomial(link = logit),
      n_dropped | trials(total_fruit) ~
        repro_connectivity_sc +
        non_repro_connectivity_sc +
        dbh_sc + year + (1|tree_id),
      prior = bprior,
      iter = 2000,
      warmup = 1000,
      chains = 4,
      cores = 4,
      seed = 9,
      file = (here::here("output", "models", "fruit_drop_short_fit.rds")))

summary(fruit_drop_short_mod)

bayestestR::describe_posterior(
  fruit_drop_short_mod,
  ci = 0.95,
  ci_method = "HDI",
  centrality = "median"
) %>%
  as.data.frame() %>%
  write_csv(here::here(
    "output",
    "results",
    "fruit_drop_short_describe_posterior.csv"
  ))
