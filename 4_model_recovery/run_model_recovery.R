rm(list = ls())
# # use in command line: Rscript run_model_recovery.R "[model_num]" "[model_num]"
# # if used in VSCode: edit mod names manually
# arg <- commandArgs(trailingOnly = TRUE)
# if (length(arg == 2)) {
#   sim_model <- arg[1]
#   fit_model <- arg[2]
# } else {
#   sim_model <- "1"
#   fit_model <- "2"
# }
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

for (sim_model in as.character(0:2)) {
  sim_path <- paste0(models_dir, full_mod_names[[sim_model]], ".R")
  sim <- new.env()
  source(sim_path, local = sim)

  #' ------------------------------------------------------------
  #' SET PARAMS -------------------------------------------------
  #' ------------------------------------------------------------
  set_params <- function() {return(list(
    n_part = 50,
    n_trials = 30,
    inv_temp_group = 0.5,
    initQ_dev_group = 2,
    mu_R_group = 5.5,
    sigma_R_group = 2
  ))}

  param_settings <- set_params()

  if (sim_model == "1") {
    param_settings[["LR_group"]] <- 0.4
  } else if (sim_model == "2") {
    param_settings[["LR_max_group"]] <- 0.6
    param_settings[["sigma_LR_group"]] <- 2
  }

  sim_dir <- paste0(recov_dir, "sim", sim_model, "/sim_dat/")
  sim$run_many(param_settings, sim_dir, n_runs)

  for (fit_model in as.character(0:2)) {
    model_path <- paste0(models_dir, full_mod_names[[fit_model]], ".stan")
    fit_dir <- paste0(recov_dir, "sim", sim_model, "/fit", fit_model, "/")

    fitting$fit_many(sim_dir, model_path, fit_data_type = "bridge", fit_dir, n_runs)
  }
}