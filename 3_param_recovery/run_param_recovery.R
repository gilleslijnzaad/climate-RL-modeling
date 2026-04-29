# SETUP
rm(list = ls())
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
mod_dir <- paste0(main_dir, "0_models/")
recov_dir <- paste0(main_dir, "3_param_recovery/")
mod_name <- "2_LRN_discr_approx_stat"
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

# ------------------------------------------------------------
# SIM + FIT SINGLE -------------------------------------------
# ------------------------------------------------------------
params <- list(
  n_part = 50,
  n_trials = 30,
  LRs_group = list(disconf = 0.2, diff = 0.6),
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2,
  margin_group = 2
)

# SIM
sim_dat <- sim$run(params)

current_dir <- paste0(recov_dir, mod_name, "/simple/")
dat_dir <- paste0(current_dir, "1_run/")
dat_path <- paste0(dat_dir, "sim_dat_1.json")
free_params_pp <- c("LR_disconf", "LR_diff", "inv_temp", "initQF", "initQU")
sim_utils$save_sim_dat(params, sim_dat, dat_path, free_params_pp)

# OPTIONAL: ADD PARAMS TO DATA
# add initQF[] and initQU[] to json data
initQFs <- sim_dat$Q_F[which(sim_dat$trial == 1)]
initQUs <- sim_dat$Q_U[which(sim_dat$trial == 1)]
json_dat <- rjson::fromJSON(file = dat_path)
json_dat[["initQF"]] <- initQFs
json_dat[["initQU"]] <- initQUs
cmdstanr::write_stan_json(json_dat, file = dat_path)

plot$sim_plots(sim_dat, params)

# FIT
model_path <- paste0(mod_dir, mod_name, "_simple.stan")
model <- cmdstan_model(model_path)
# draws <- fitting$fit(model, dat_path, return = "draws", show_iteration_progress = TRUE)
# saveRDS(draws, paste0(dat_dir, "draws.rds"))
draws <- readRDS(paste0(dat_dir, "draws.rds"))
free_params <- c("LR_disconf_group", "LR_diff_group", "inv_temp_group")
draws <- draws %>% 
  rename(setNames(paste0("means[", seq_along(free_params), "]"),
                  free_params))

# INSPECT
to_plot <- list(c("LR_disconf_group", "LR_diff_group"), "inv_temp_group")
plot$posterior_densities(draws, to_plot, params)

# ------------------------------------------------------------
# SIM + FIT MANY ---------------------------------------------
# ------------------------------------------------------------
n_runs <- 100
dat_dir <- paste0(current_dir, "100_runs/")
sim$run_many(params, dat_dir, n_runs)
# running this bit below takes at least an hour
# fitting$fit_many(dat_dir, model_path, fit_data_type = "draws", dat_dir, n_runs)

# INSPECT
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
    rename(setNames(paste0("means[", seq_along(free_params), "]"),
                    free_params))

  for (p in free_params) {
    sim_params[[p]][k] <- sim_dat[[p]]
    fit_params[[p]][k] <- median(fit_dat[[p]])
  }
}

plot$many_runs_param_fit(sim_params, fit_params, free_params)
