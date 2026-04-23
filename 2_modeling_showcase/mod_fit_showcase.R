## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 8)

## ----create-data, comment = NA------------------------------------------------
rm(list = ls())
setwd("~/research/climate-RL-mod/2_modeling_showcase")
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
mod_dir <- paste0(main_dir, "0_models/")
model_path <- paste0(mod_dir, "1_std.stan")

sim <- new.env()
source(paste0(mod_dir, "1_std.R"), local = sim) # access functions using sim$fun()

params <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)

sim_dat <- sim$run(params)

cat(paste0("PARAMETER SETTINGS:"), capture.output(dplyr::glimpse(params)), sep = "\n")
cat(paste0("SIMULATED DATA:"), capture.output(dplyr::glimpse(sim_dat)), sep = "\n")

## ----inspecting-data, fig.height = 4------------------------------------------
plot <- new.env()
source(paste0(util_dir, "plot_utils.R"), local = plot) # access functions using plot$fun()

plot$sim_plots(sim_dat, params)

## ----explmod-data, comment = NA-----------------------------------------------
mod_code <- readLines(model_path)
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
dat_dir <- paste0(main_dir, "2_modeling_showcase/dat/1_run/")
dat_file <- paste0(dat_dir, "sim_dat_001.json")

sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils) # access functions using sim_utils$fun()

dat_changed <- sim_utils$did_sim_dat_change(dat_file, sim_dat)
sim_utils$save_sim_dat(params, sim_dat, dat_file)
model_changed <- FALSE

fitting <- new.env()
source(paste0(util_dir, "fit_utils.R"), local = fitting) # access functions using fitting$fun()
if (dat_changed | model_changed) {
  model <- cmdstan_model(model_path)
  draws <- fitting$fit(model, dat_file, return = "draws", show_iteration_progress = TRUE)
  saveRDS(draws, paste0(dat_dir, "draws_001.rds"))
} else {
  draws <- readRDS(file = paste0(dat_dir, "draws_001.rds"))
}

## ----posterior-plots----------------------------------------------------------
draws <- draws %>%
  rename(
    LR_group = `means[1]`,
    inv_temp_group = `means[2]`,
    initQF_group = `means[3]`,
    initQU_group = `means[4]`,
  )
to_plot <- list("LR_group", "inv_temp_group", c("initQF_group", "initQU_group"))
plot$posterior_densities(draws, to_plot, params)

## ----posterior-table----------------------------------------------------------
util <- new.env()
source(paste0(util_dir, "utils.R"), local = util) # access functions using util$fun()
to_inspect <- c("LR_group", "inv_temp_group", "initQF_group", "initQU_group")
util$print_posterior_table(draws, params, to_inspect)

## ----sim-vs-fit---------------------------------------------------------------
participant_params <- rjson::fromJSON(file = paste0(dat_dir, "sim_param_settings_001.json"))
free_params_pp <- c("LR", "inv_temp", "initQF", "initQU")
plot$pp_level_param_fit(draws, free_params_pp, participant_params)

## ----many-runs----------------------------------------------------------------
n_runs <- 100
free_params <- c("LR_group", "inv_temp_group", "initQF_group", "initQU_group")
dat_dir <- paste0(main_dir, "2_modeling_showcase/dat/100_runs/")

sim$run_many(params, dat_dir, n_runs)
# running this bit below takes at least an hour
fitting$fit_many(dat_dir, model_path, fit_data_type = "draws", dat_dir, n_runs)

## ----inspect-many-runs--------------------------------------------------------
sim_params <- data.frame(k = 1:n_runs)
fit_params <- data.frame(k = 1:n_runs)

for (k in 1:n_runs) {
  sim_file <- paste0(dat_dir, "param_settings_", sprintf("%03d", k), ".json")
  sim_dat <- rjson::fromJSON(file = sim_file)

  fit_file <- paste0(dat_dir, "draws_", sprintf("%03d", k), ".rds")
  fit_dat <- readRDS(fit_file) %>%
    rename(
      LR_group = `means[1]`,
      inv_temp_group = `means[2]`,
      initQF_group = `means[3]`,
      initQU_group = `means[4]`,
    )

  for (p in free_params) {
    sim_params[[p]][k] <- sim_dat[[p]]
    fit_params[[p]][k] <- median(fit_dat[[p]])
  }
}

plot$many_runs_param_fit(sim_params, fit_params, free_params)

