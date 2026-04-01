## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 8)

## ----create-data, comment = NA------------------------------------------------
rm(list = ls())
# setwd("~/research/climate-RL-mod/stan_models")
main_dir <- "~/research/climate-RL-mod/"
sim_dir <- paste0(main_dir, "R_simulation/")

sim <- new.env()
source(paste0(sim_dir, "sim.R"), local = sim) # access functions using sim$fun()

params <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 0.5,
  initQ_group = list(F = 8, U = 2),
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)

sim_dat <- sim$run_std(params)

cat(paste0("PARAMETER SETTINGS:"), capture.output(dplyr::glimpse(params)), sep = "\n")
cat(paste0("SIMULATED DATA:"), capture.output(dplyr::glimpse(sim_dat)), sep = "\n")

## ----inspecting-data, fig.height = 4------------------------------------------
plot <- new.env()
source(paste0(main_dir, "plot_utils.R"), local = plot) # access functions using plot$fun()

plot$sim_plots(sim_dat, params)

## ----explmod-data, comment = NA-----------------------------------------------
mod_code <- readLines("climate-RL.stan")
start <- grep("data \\{", mod_code)[1]
end <- grep("transformed data \\{", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-transf-data, eval = FALSE, comment = NA--------------------------
# start <- grep("transformed data \\{", mod_code)
# end <- grep("parameters \\{", mod_code)[1] - 2
# cat(mod_code[start:end], sep = "\n")

## ----explmod-params, comment = NA---------------------------------------------
start <- grep("parameters \\{", mod_code)[1]
end <- grep("transformed parameters \\{", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-transf-params, eval = FALSE, comment = NA------------------------
# start <- grep("transformed parameters \\{", mod_code)
# end <- grep("model \\{", mod_code) - 2
# cat(mod_code[start:end], sep = "\n")

## ----explmod-model-priors, comment = NA---------------------------------------
start <- grep("model \\{", mod_code)
end <- grep("participant loop", mod_code)[1] - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-model-part, comment = NA-----------------------------------------
start <- grep("participant loop", mod_code) + 1
end <- grep("trial loop", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-model-trial, comment = NA----------------------------------------
start <- grep("trial loop", mod_code) + 1
end <- grep("generated quantities", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-gen-quant, comment = NA------------------------------------------
start <- grep("generated quantities", mod_code)
end <- length(mod_code)
cat(mod_code[start:end], sep = "\n")

## ----run-model----------------------------------------------------------------
dat_dir <- paste0(main_dir, "stan_models/single_run_dat/")
dat_file <- paste0(dat_dir, "sim_dat.json")
dat_changed <- sim$did_sim_dat_change(dat_file, sim_dat)
sim$save_sim_dat(params, sim_dat, dat_file)
model_changed <- FALSE

library(cmdstanr)
options(mc.cores = parallel::detectCores())
if (dat_changed | model_changed) {
  m <- cmdstan_model("climate-RL.stan")
  it <- 1000
  fit <- m$sample(
    data = dat_file,
    iter_sampling = it,
    chains = 1,
    thin = 1,
    iter_warmup = it / 2,
    refresh = it / 5,
    seed = 1234
  )
  fit$save_object(file = paste0(dat_dir, "climate-RL_single_fit.rds"))
} else {
  fit <- readRDS(file = paste0(dat_dir, "climate-RL_single_fit.rds"))
}
draws <- posterior::as_draws_df(fit$draws()) # df makes it easier to handle
draws <- draws %>%
  dplyr::rename(
    LR_group = `means[1]`,
    inv_temp_group = `means[2]`,
    `initQ_group$F` = `means[3]`,
    `initQ_group$U` = `means[4]`
  )

## ----posterior-plots----------------------------------------------------------
to_inspect <- c("LR_group", "inv_temp_group", "initQ_group$F", "initQ_group$U")
plot$posterior_density(draws, to_inspect, params)

## ----posterior-table----------------------------------------------------------
util <- new.env()
source(paste0(main_dir, "utils.R"), local = util) # access functions using util$fun()
util$print_posterior_table(draws, params, to_inspect)

## ----sim-vs-fit---------------------------------------------------------------
participant_params <- rjson::fromJSON(file = paste0(dat_dir, "sim_param_settings.json"))
plot$pp_level_param_fit(draws, c("LR", "inv_temp", "initQF", "initQU"), participant_params)

## ----many-runs----------------------------------------------------------------
n_runs <- 10
dat_dir <- paste0(main_dir, "stan_models/many_runs_dat/")
m <- cmdstan_model("climate-RL.stan")
it <- 1000
free_params <- c("LR_group", "inv_temp_group", "initQ_group$F", "initQ_group$U")

# takes current list of params, randomizes the free ones and returns the list
randomize_free_params <- function(params) {
  for (p in free_params) {
    bounds <- sim$param_bounds[[p]]
    params[[p]] <- runif(1, min = bounds[1], max = bounds[2])
  }

  return(params)
}

if (FALSE) {
  for (k in 1:n_runs) {
    # simulate
    params <- randomize_free_params(params)
    sim_dat <- sim$run_std(params)
    dat_file <- paste0(dat_dir, "sim_dat_", sprintf("%02d", k), ".json")
    sim$save_sim_dat(params, sim_dat, dat_file)

    # fit
    fit <- m$sample(
      data = dat_file,
      iter_sampling = it,
      chains = 1,
      thin = 1,
      iter_warmup = it / 2,
      refresh = it / 5,
      seed = 1234
    )
    fit_file <- paste0(dat_dir, "climate-RL_fit_", sprintf("%02d", k), ".rds")
    fit$save_object(file = fit_file)
  }
}
# run time: around 40 seconds per sim, 6-7 min total

## ----inspect-many-runs--------------------------------------------------------
sim_params <- data.frame(k = 1:n_runs)
fit_params <- data.frame(k = 1:n_runs)

for (k in 1:n_runs) {
  sim_file <- paste0(dat_dir, "sim_param_settings_", sprintf("%02d", k), ".json")
  sim <- rjson::fromJSON(file = sim_file)

  fit_file <- paste0(dat_dir, "climate-RL_fit_", sprintf("%02d", k), ".rds")
  fit <- readRDS(fit_file)
  draws <- posterior::as_draws_df(fit$draws()) %>%
    dplyr::rename(
      LR_group = `means[1]`,
      inv_temp_group = `means[2]`,
      `initQ_group$F` = `means[3]`,
      `initQ_group$U` = `means[4]`
    )

  for (p in free_params) {
    if (grepl("\\$", p)) { # parameter is part of a list
      split_name <- strsplit(p, "\\$")[[1]]
      sim_value <- sim[[split_name[1]]][(split_name[2] == "U") + 1]
      sim_params[[p]][k] <- sim_value
    } else {
      sim_params[[p]][k] <- sim[[p]]
    }
    fit_params[[p]][k] <- median(draws[[p]])
  }
}

source(paste0(main_dir, "plot_utils.R"), local = plot)
plot$many_runs_param_fit(sim_params, fit_params, free_params)

