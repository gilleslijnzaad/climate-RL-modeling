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
#' returns fit object or a draws dataframe
#' 
#' @param model: the CmdStanModel object
#' 
#' @param dat_file: name of data file to feed to Stan
#' 
#' @param return: whether to return the fit object, or a draws
#' dataframe
#' 
#' @param k: run number (for random seed and progress print)
#' 
#' @param n_runs: total number of runs (for progress print)
#' 
#' @param show_iteration_progress: whether to show the progress of the
#' individual iteration (i.e., chain updates)
#' 
#' @return data frame of the type specified in return
fit <- function(model, dat_file, return, k = 1, n_runs = 1, free_params = NULL, show_iteration_progress = FALSE) {
  it <- 5000
  fit <- model$sample(
    data = dat_file,
    iter_sampling = it,
    chains = 1,
    thin = 1,
    iter_warmup = it / 2,
    refresh = it / 5,
    seed = k,
    show_messages = show_iteration_progress
  )
  if (return == "draws") {
    draws <- posterior::as_draws_df(fit$draws()) %>%
      rename(setNames(paste0("means[", seq_along(free_params), "]"),
                      free_params))
    return(draws)
  } else if (return == "bridge") {
    return(bridgesampling::bridge_sampler(fit))
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
#' @param free_params names of free parameters (to rename )
#' 
#' @return nothing
fit_many <- function(sim_dat_dir, model_path, fit_data_type, fit_dir, n_runs, model_nums = NULL, free_params = NULL) {
  model <- cmdstan_model(model_path, force_recompile = TRUE)
  for (k in 1:n_runs) {
    sim_dat_path <- sprintf("%sdat_%03d.json", sim_dat_dir, k)
    fit_path <- sprintf("%s%s_%03d.rds", fit_dir, fit_data_type, k)
    fit <- fit(model, sim_dat_path, return = fit_data_type, k, n_runs, free_params = free_params)
    message(format(Sys.time(), "[%d%b%Y, %H:%M] "), 
            sprintf("run %d of %d ✅ ", k, n_runs),
            sprintf("(sim%d_fit%d)", model_nums[["sim"]], model_nums[["fit"]]))
    saveRDS(fit, file = fit_path)
  }
}