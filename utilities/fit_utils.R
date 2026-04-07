library(cmdstanr)
options(mc.cores = parallel::detectCores())

# -----------------------------
#        SET DIRECTORIES
# -----------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "utilities/")
sim <- new.env()
source(paste0(util_dir, "sim.R"), local = sim)
mod_dir <- paste0(main_dir, "stan_models/dat/")

# ---------------------------
#        MODEL FITTING
# ---------------------------

# === get_draws() =======================
# fits the model once, prints progress (Completed run k of n_runs), returns data frame of fit draws
# arguments: 
# - model: the CmdStanModel object
# - dat_file: name of data file to feed to Stan
# - k: run number (for progress print)
# - n_runs: total number of runs (for progress print)
# - show_iteration_progress: whether to show the progress of the individual iteration (i.e., chain updates)
# 
# returns: data frame of fit draws
get_draws <- function(model, dat_file, k = 1, n_runs = 1, show_iteration_progress = FALSE) {
  it <- 1000
  fit <- model$sample(
    data = dat_file,
    iter_sampling = it,
    chains = 1,
    thin = 1,
    iter_warmup = it / 2,
    refresh = it / 5,
    seed = 1234,
    show_messages = show_iteration_progress
  )
  draws <- posterior::as_draws_df(fit$draws()) # df makes it easier to handle
  draws <- draws %>%
    dplyr::rename(
      LR_group = `means[1]`,
      inv_temp_group = `means[2]`,
      initQF_group = `means[3]`,
      initQU_group = `means[4]`
    )
  message("Completed run ", k, " of ", n_runs, " in ", round(fit$time()$total, 1), " seconds.")
  return(draws)
}

# === randomize_free_params() =======================
# randomizes given free parameters: uniform distribution over their bounds
# arguments: 
# - param_settings: list of parameter settings
# - free_params: list of names of free parameters to randomize
# 
# returns: list of parameter settings with free_params randomized
randomize_free_params <- function(param_settings, free_params) {
  for (p in free_params) {
    bounds <- sim$param_bounds[[p]]
    param_settings[[p]] <- runif(1, min = bounds[1], max = bounds[2])
  }
  return(param_settings)
}

# === sim_fit_many() =======================
# runs simulation and model and saves results to json and rds files, respectively, in a directory called [n_runs]_runs
# arguments: 
# - param_settings: list of parameter settings
# - free_params: list of names of free parameters
# - model_file: name of Stan file
# - save_dat_to: directory to save data to; will create a new folder [n_runs]_runs/
# - n_runs: number of times to run the simulation and model
# 
# returns: nothing
sim_fit_many <- function(param_settings, free_params, model_file, save_dat_to, n_runs = 1) {
  dat_dir <- paste0(save_dat_to, n_runs, "_runs/")
  if (!dir.exists(dat_dir)) dir.create(dat_dir)
  model <- cmdstan_model(model_file)
  for (k in 1:n_runs) {
    # simulate
    params <- randomize_free_params(param_settings, free_params)
    sim_dat <- sim$run_std(params)
    dat_file <- paste0(dat_dir, "sim_dat_", sprintf("%03d", k), ".json")
    sim$save_sim_dat(params, sim_dat, dat_file)

    # fit
    fit_file <- paste0(dat_dir, "fit_", sprintf("%03d", k), ".rds")
    fit <- get_draws(model, dat_file, k, n_runs)
    saveRDS(fit, file = fit_file)
  }
}
