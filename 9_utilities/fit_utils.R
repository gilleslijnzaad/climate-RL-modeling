library(cmdstanr)
options(mc.cores = parallel::detectCores())

# -----------------------------
#        SET DIRECTORIES
# -----------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils)

# ---------------------------
#        MODEL FITTING
# ---------------------------

#' Fits the model once, prints progress (Completed run k of n_runs),
#' returns data frame of fit draws
#' 
#' @param model: the CmdStanModel object
#' 
#' @param dat_file: name of data file to feed to Stan
#' 
#' @param return: whether to return the fit object, or a draws
#' dataframe
#' 
#' @param k: run number (for progress print)
#' 
#' @param n_runs: total number of runs (for progress print)
#' 
#' @param show_iteration_progress: whether to show the progress of the
#' individual iteration (i.e., chain updates)
#' 
#' @return data frame of fit draws
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

#' Runs model [n_runs] times and saves fit objects as rds files in the
#' given directory
#' 
#' @param sim_dat_dir directory where simulated data are saved
#' 
#' @param model_path path to the Stan model
#' 
#' @param fit_data_type either "fit" for the fit object or or "draws"
#' for a draws dataframe
#' 
#' @param fit_dir directory where fit data should be saved
#' 
#' @param n_runs how often to fit the model
#' 
#' @return nothing
fit_many <- function(sim_dat_dir, model_path, fit_data_type, fit_dir, n_runs) {
  model <- cmdstan_model(model_path)
  for (k in 1:n_runs) {
    k_code <- sprintf("%03d", k)
    sim_dat_path <- paste0(sim_dat_dir, "dat_", k_code, ".json")
    fit_path <- paste0(fit_dir, fit_data_type, "_", k_code, ".rds")
    fit <- fit(model, sim_dat_path, return = fit_data_type, k, n_runs)
    saveRDS(fit, file = fit_path)
  }
}