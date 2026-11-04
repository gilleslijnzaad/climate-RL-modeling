library(tidyverse)

rm(list = ls())

mod_name <- "1_std"

if (1) {
  main_dir <- "~/research/climate-RL-mod/"
  util_dir <- paste0(main_dir, "9_utilities/")
  mod_dir <- paste0(main_dir, "0_models/")
  model_path <- paste0(mod_dir, mod_name, ".stan")
  recov_dir <- paste0(main_dir, "3_param_recovery/")
  current_dir <- paste0(recov_dir, "sandbox/")
  sim <- new.env()
  source(paste0(mod_dir, mod_name, ".R"), local = sim) # access functions using sim$fun()
  sim_utils <- new.env()
  source(paste0(util_dir, "sim_utils.R"), local = sim_utils)
  plot <- new.env()
  source(paste0(util_dir, "plot_utils.R"), local = plot)
  fitting <- new.env()
  source(paste0(util_dir, "fit_utils.R"), local = fitting)
  util <- new.env()
  source(paste0(util_dir, "utils.R"), local = util)
}

param_settings <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 2,
  initQ_dev_group = 2,
  mu_R_group = 5.5,
  sigma_R_group = 2
)

free_params_group <- c("LR_group", "inv_temp_group", "initQ_dev_group")
free_params_pp <- gsub("_group", "", free_params_group)

if (1) {
  sim_dat <- sim$run(param_settings)
  dat_path <- sprintf("%ssim_dat.json", current_dir)
  sim_utils$save_sim_dat(param_settings, sim_dat, dat_path, free_params_pp)


  model <- cmdstan_model(model_path)
  draws <- fitting$fit(model, dat_path, return = "draws", show_iteration_progress = TRUE) %>% 
    rename(setNames(paste0("means[", seq_along(free_params_group), "]"),
                    free_params_group))

  pl <- plot$posterior_densities(draws, free_params_group, param_settings)
}

name <- sprintf("%sLR_%.1f_invtemp_%.1f_initQdev_%.1f.png",
                current_dir,
                param_settings[["LR_group"]],
                param_settings[["inv_temp_group"]],
                param_settings[["initQ_dev_group"]])

ggsave(name,
       plot = pl,
       width = 10,
       height = 6,
       units = "in",
       bg = "white")

grid.newpage()
grid.draw(pl)