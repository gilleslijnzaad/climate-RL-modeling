## ----rmd-setup, include = FALSE-----------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 8)

## ----setup--------------------------------------------------------------------
rm(list = ls())
# setwd("~/research/climate-RL-mod/model_recovery")
main_dir <- "~/research/climate-RL-mod/"
recov_dir <- paste0(main_dir, "model_recovery/")
util_dir <- paste0(main_dir, "utilities/")
models_dir <- paste0(main_dir, "models/")

n_runs <- 100

settings <- list(
  n_part = 50,
  n_trials = 30,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)

fitting <- new.env()
source(paste0(util_dir, "fit_utils.R"), local = fitting)

## ----sim-0--------------------------------------------------------------------
sim_model_name <- "0_null"
sim_path <- paste0(models_dir, sim_model_name, ".R")
sim <- new.env()
source(sim_path, local = sim)

sim_dir <- paste0(recov_dir, "dat/", sim_model_name, "/sim/")
if (!dir.exists(sim_dir)) dir.create(sim_dir)

sim$run_many(settings, sim_dir, n_runs)

## ----fit-0.0------------------------------------------------------------------
fit_model_name <- "0_null"

model_path <- paste0(models_dir, fit_model_name, ".stan")
fit_dir <- paste0(recov_dir, "dat/", sim_model_name, "/", fit_model_name, "/")
if (!dir.exists(fit_dir)) dir.create(fit_dir)

# DOES NOT WORK YET
# fitting$fit_many(sim_dir, model_path, fit_dir, n_runs)

## ----sim-1--------------------------------------------------------------------
sim_model_name <- "1_std"
sim_path <- paste0(models_dir, sim_model_name, ".R")
sim <- new.env()
source(sim_path, local = sim)

sim_dir <- paste0(recov_dir, "dat/", sim_model_name, "/sim/")
if (!dir.exists(sim_dir)) dir.create(sim_dir)

sim$run_many(settings, sim_dir, n_runs)

## ----fit-1.1------------------------------------------------------------------
fit_model_name <- "1_std"

model_path <- paste0(models_dir, fit_model_name, ".stan")
fit_dir <- paste0(recov_dir, "dat/", sim_model_name, "/", fit_model_name, "/")
if (!dir.exists(fit_dir)) dir.create(fit_dir)

# WOULD TAKE AROUND AN HOUR TO RUN
# fitting$fit_many(sim_dir, model_path, fit_dir, n_runs)

## -----------------------------------------------------------------------------
library(bridgesampling)

