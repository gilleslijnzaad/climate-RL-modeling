rm(list = ls())
# use in command line: Rscript run_param_recovery.R "[model_name]"
# if used in VSCode: edit mod_name manually
# arg <- commandArgs(trailingOnly = TRUE)
# if (length(arg == 1)) {
#   mod_name <- arg
# } else {
#   mod_name <- "2_LRN_gauss"
# }
n_runs <- 1

#' ------------------------------------------------------------
#' SET DIRS AND ENVS ------------------------------------------
#' ------------------------------------------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
mod_dir <- paste0(main_dir, "0_models/")
recov_dir <- paste0(main_dir, "3_param_recovery/")
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
  inv_temp_group = 0.5,
  initQ_dev_group = 2,
  mu_R_group = 5.5,
  sigma_R_group = 2
))}

for (sim_model in as.character(0:2)) {
  sim <- new.env()
  source(paste0(mod_dir, full_mod_names[[sim_model]], ".R"), local = sim)
  model_path <- paste0(mod_dir, full_mod_names[[sim_model]], ".stan")
  current_dir <- paste0(recov_dir, full_mod_names[[sim_model]], "/")

  param_settings <- set_params()
  if (sim_model == "0") {
    free_params_group <- c("inv_temp_group", "initQ_dev_group")
  } else if (sim_model == "1") {
    param_settings[["LR_group"]] <- 0.4
    free_params_group <- c("LR_group", "inv_temp_group", "initQ_dev_group")
  } else if (sim_model == "2") {
    param_settings[["LR_max_group"]] <- 0.4
    param_settings[["sigma_LR_group"]] <- 3
    free_params_group <- c("LR_max_group", "sigma_LR_group", "inv_temp_group", "initQ_dev_group")
  }

  dat_dir <- paste0(current_dir, n_runs, ifelse(n_runs == 1,
                                         "_run/",
                                         "_runs/"))
  if (n_runs == 1) {
    sim_dat <- sim$run(param_settings)
    dat_file <- paste0(dat_dir, "dat_001.json")
    sim_utils$save_sim_dat(param_settings, sim_dat, dat_file, gsub("_group", "", free_params_group))

    pl <- plot$sim_plots(sim_dat, param_settings)
    ggsave("sim_plots.png", plot = pl, path = dat_dir, width = 10, height = 4, units = "in")

    model <- cmdstan_model(model_path)
    f <- fitting$fit(model, dat_file, "draws", free_params = free_params_group, show_iteration_progress = TRUE)
    saveRDS(f, file = paste0(dat_dir, "draws.rds"))
  } else {
    sim$run_many(param_settings, dat_dir, n_runs)
    fitting$fit_many(dat_dir, model_path, fit_data_type = "draws", dat_dir, n_runs, free_params = free_params_group, show)
  }
}