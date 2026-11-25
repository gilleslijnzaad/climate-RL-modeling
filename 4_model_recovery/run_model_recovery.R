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

set_params <- function() {return(list(
  n_part = 50,
  n_trials = 30,
  inv_temp_group = NA, # will be randomized as free param anyway
  initQ_dev_group = NA,
  mu_R_group = 5.5,
  sigma_R_group = 2
))}

for (sim_model in as.character(0:2)) {
  #' SIM
  sim_path <- paste0(models_dir, full_mod_names[[sim_model]], ".R")
  sim <- new.env()
  source(sim_path, local = sim)

  param_settings <- set_params()
  if (sim_model == "1") {
    param_settings[["LR_group"]] <- NA
  } else if (sim_model == "2") {
    param_settings[["LR_max_group"]] <- NA
    param_settings[["sigma_LR_group"]] <- NA
  }

  sim_dir <- paste0(recov_dir, "sim", sim_model, "/sim_dat/")
  sim$run_many(param_settings, sim_dir, n_runs)

  #' FIT
  for (fit_model in as.character(0:2)) {
    model_path <- paste0(models_dir, full_mod_names[[fit_model]], ".stan")
    fit_dir <- paste0(recov_dir, "sim", sim_model, "/fit", fit_model, "/")

    model_nums <- list("sim" = sim_model, "fit" = fit_model)
    fitting$fit_many(sim_dir, model_path, fit_data_type = "bridge", fit_dir, n_runs, model_nums)
  }

  #' WIN
  winning_models <- data.frame(
    run = 1:n_runs
  )

  bridge_fits <- list(
    "0" = NA,
    "1" = NA,
    "2" = NA
  )
  
  for (run in 1:n_runs) {
    sim_param_settings <- rjson::fromJSON(file = sprintf("%ssim%s/sim_dat/param_settings_%.03d.json", recov_dir, sim_model, run))
    for (p in free_params) { # TODO: define free_params somewhere
      winning_models[[p]] <- sim_param_settings[[p]]
    }

    for (fit_model in as.character(0:2)) {
      bridge_fits[[fit_model]] <- readRDS(sprintf("%ssim%s/fit%s/bridge_%.03d.rds", recov_dir, sim_model, fit_model, run))
    }
    winner_I <- ifelse(bf(bridge_fits[["1"]], bridge_fits[["0"]])$bf > 3,
                       "1",
                       "0")
    winner_II <- ifelse(bf(bridge_fits[["2"]], bridge_fits[["1"]])$bf > 3,
                        "2",
                        "1")
    final_winner <- ifelse(bf(bridge_fits[[winner_II]], bridge_fits[[winner_I]])$bf > 3,
                           winner_II,
                           winner_I)
    winning_models$winner <- final_winner
  }
  saveRDS(winning_models, file = sprintf("%ssim%s/winning_models.rds", recov_dir, sim_model))
}