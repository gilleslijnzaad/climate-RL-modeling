library(cmdstanr)
options(mc.cores = parallel::detectCores())

# -----------------------------
#        SET DIRECTORIES
# -----------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "utilities/")
sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils)
mod_dir <- paste0(main_dir, "stan_models/dat/")

# ---------------------------
#        MODEL FITTING
# ---------------------------
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
sim_fit_many <- function(param_settings, free_params, model_file, save_draws_to, n_runs = 1) {
  dat_dir <- paste0(save_draws_to, n_runs, "_runs/")
  if (!dir.exists(dat_dir)) dir.create(dat_dir)
  model <- cmdstan_model(model_file)
  for (k in 1:n_runs) {
    # simulate
    params <- sim_utils$randomize_free_params(param_settings, free_params)
    sim_dat <- sim_utils$run_std(params)
    dat_file <- paste0(dat_dir, "sim_dat_", sprintf("%03d", k), ".json")
    sim$save_sim_dat(params, sim_dat, dat_file)

    # fit
    draws_file <- paste0(dat_dir, "draws_", sprintf("%03d", k), ".rds")
    draws <- fit(model, dat_file, return = "draws", k, n_runs)
    saveRDS(draws, file = draws_file)
  }
}

# === fit() =======================
# fits the model once, prints progress (Completed run k of n_runs), returns data frame of fit draws
# arguments: 
# - model: the CmdStanModel object
# - dat_file: name of data file to feed to Stan
# - return: whether to return the fit object, or a draws dataframe
# - k: run number (for progress print)
# - n_runs: total number of runs (for progress print)
# - show_iteration_progress: whether to show the progress of the individual iteration (i.e., chain updates)
# 
# returns: data frame of fit draws
fit <- function(model, dat_file, return, k = 1, n_runs = 1, show_iteration_progress = FALSE) {
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
  message("Completed run ", k, " of ", n_runs, " in ", round(fit$time()$total, 1), " seconds.")
  if (return == "draws") {
    draws <- posterior::as_draws_df(fit$draws()) # df makes it easier to handle
    return(draws)
  } else {
    return(fit)
  }
}

# === fit_many() =======================
# runs model [n_runs] times and saves fit objects as rds files
# arguments: 
# - TODO
# 
# returns: nothing
fit_many <- function(sim_dat_dir, model_path, fit_dir, n_runs, free_params = NULL) {
  model <- cmdstan_model(model_path)
  for (k in 1:n_runs) {
    k_code <- sprintf("%03d", k)
    sim_dat_path <- paste0(sim_dat_dir, "dat_", k_code, ".json")
    fit_path <- paste0(fit_dir, "fit_", k_code, ".rds")
    fit <- fit(model, sim_dat_path, return = "fit", k, n_runs)
    saveRDS(fit, file = fit_path)
  }
}