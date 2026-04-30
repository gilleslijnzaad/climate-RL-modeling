rm(list = ls())
mod_name <- "0_nolearning"

#' ------------------------------------------------------------
#' SET DIRS AND ENVS ------------------------------------------
#' ------------------------------------------------------------
if (TRUE) {
  main_dir <- "~/research/climate-RL-mod/"
  util_dir <- paste0(main_dir, "9_utilities/")
  mod_dir <- paste0(main_dir, "0_models/")
  recov_dir <- paste0(main_dir, "3_param_recovery/")
  current_dir <- paste0(recov_dir, mod_name, "/")
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

#' ------------------------------------------------------------
#' SET PARAMS -------------------------------------------------
#' ------------------------------------------------------------
#' ::: 0_nolearning :::
params <- list(
  n_part = 50,
  n_trials = 30,
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2,
  margin_group = 2
)
free_params_group <- c("inv_temp_group", "initQF_group", "initQU_group")

#' ::: 1_std :::
# params[["LR_group"]] <- 0.4
# free_params_group <- c("LR_group", "inv_temp_group", "initQF_group", "initQU_group")

free_params_pp <- gsub("_group", "", free_params_group)

#' ------------------------------------------------------------
#' SIM FIT SINGLE ---------------------------------------------
#' ------------------------------------------------------------
if (TRUE) {
  #' SIM
  sim_dat <- sim$run(params)

  dat_dir <- paste0(current_dir, "1_run/")
  dat_path <- paste0(dat_dir, "sim_dat_1.json")
  sim_utils$save_sim_dat(params, sim_dat, dat_path, free_params_pp)

  pl <- plot$sim_plots(sim_dat, params)
  ggsave("sim_plots.png", 
         plot = pl,
         path = dat_dir,
         width = 10,
         height = 4,
         units = "in")

  #' FIT
  model_path <- paste0(mod_dir, mod_name, ".stan")
  model <- cmdstan_model(model_path)
  # draws <- fitting$fit(model, dat_path, return = "draws", show_iteration_progress = TRUE)
  # saveRDS(draws, paste0(dat_dir, "draws.rds"))
}

#' ------------------------------------------------------------
#' INSPECT SINGLE ---------------------------------------------
#' ------------------------------------------------------------
if (TRUE) {
  draws <- readRDS(paste0(dat_dir, "draws.rds"))
  draws <- draws %>% 
    rename(setNames(paste0("means[", seq_along(free_params_group), "]"),
                    free_params_group))
  to_plot <- list("inv_temp_group", c("initQF_group", "initQU_group"))
  plot$posterior_densities(draws, to_plot, params)
}

#' ------------------------------------------------------------
#' SIM + FIT MANY ---------------------------------------------
#' ------------------------------------------------------------
if (TRUE) {
  n_runs <- 100
  dat_dir <- paste0(current_dir, "100_runs/")
  sim$run_many(params, dat_dir, n_runs)
  # fitting$fit_many(dat_dir, model_path, fit_data_type = "draws", dat_dir, n_runs)
}

#' ------------------------------------------------------------
#' INSPECT MANY -----------------------------------------------
#' ------------------------------------------------------------
if (FALSE) {
  sim_params <- data.frame(k = 1:n_runs)
  fit_params <- data.frame(k = 1:n_runs)

  for (k in 1:n_runs) {
    sim_file <- paste0(dat_dir, "param_settings_", sprintf("%03d", k), ".json")
    sim_dat <- rjson::fromJSON(file = sim_file)
    if ("LRs_group" %in% names(sim_dat)) {
      sim_dat[["LR_disconf_group"]] <- sim_dat[["LRs_group"]][1]
      sim_dat[["LR_diff_group"]] <- sim_dat[["LRs_group"]][2]
    }

    fit_file <- paste0(dat_dir, "draws_", sprintf("%03d", k), ".rds")
    fit_dat <- readRDS(fit_file) %>%
      rename(setNames(paste0("means[", seq_along(free_params_group), "]"),
                      free_params_group))

    for (p in free_params_group) {
      sim_params[[p]][k] <- sim_dat[[p]]
      fit_params[[p]][k] <- median(fit_dat[[p]])
    }
  }

  plot$many_runs_param_fit(sim_params, fit_params, free_params_group)
}
