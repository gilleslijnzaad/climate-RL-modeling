rm(list = ls())
library(bridgesampling)
n_runs <- 1

#' ------------------------------------------------------------
#' SET DIRS AND ENVS ------------------------------------------
#' ------------------------------------------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
recov_dir <- paste0(main_dir, "4_model_recovery/")
models_dir <- paste0(main_dir, "0_models/")
sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils)
plot <- new.env()
source(paste0(util_dir, "plot_utils.R"), local = plot)
fitting <- new.env()
source(paste0(util_dir, "fit_utils.R"), local = fitting)
util <- new.env()
source(paste0(util_dir, "utils.R"), local = util)

full_mod_names <- list(
  "0" = "0_nolearning",
  "1" = "1_std",
  "2" = "2_LRN_gauss"
)

param_settings <- list(
  n_part = 50,
  n_trials = 30,
  # LR_max_group = 0.01,
  # sigma_LR_group = 0.5,
  inv_temp_group = 5,
  initQ_dev_group = 3,
  mu_R_group = 5.5,
  sigma_R_group = 2
)
save_dir <- paste0(recov_dir, "sandbox/exampleF/")

# free_params_pp <- c("LR_max", "sigma_LR", "inv_temp", "initQ_dev")
free_params_pp <- c("inv_temp", "initQ_dev")

sim_model <- "0"
sim_path <- paste0(models_dir, full_mod_names[[sim_model]], ".R")
sim <- new.env()
source(sim_path, local = sim)

sim_dat <- sim$run(param_settings)
sim_dat_path <- paste0(save_dir, "dat_001.json")
sim_utils$save_sim_dat(param_settings, sim_dat, sim_dat_path, free_params_pp)

for (fit_model in as.character(0:2)) {
  model_path <- paste0(models_dir, full_mod_names[[fit_model]], ".stan")
  fit_dir <- paste0(save_dir, "fit", fit_model, "/")
  sim_dir <- save_dir

  fitting$fit_many(sim_dir, model_path, fit_data_type = "draws", fit_dir, n_runs, free_params = free_params_pp)
}

fit0 <- readRDS(paste0(save_dir, "fit0/bridge_001.rds"))
fit1 <- readRDS(paste0(save_dir, "fit1/bridge_001.rds"))
fit2 <- readRDS(paste0(save_dir, "fit2/bridge_001.rds"))
