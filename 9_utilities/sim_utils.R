set.seed(1234)
library(dplyr)
library(stringr)

#' A list of standard deviations for group-level means of parameters
param_stddevs <- list(
  LR_group = 0.2,
  LR_max_group = 0.2,
  sigma_LR_group = 0.8,
  inv_temp_group = 0.3,
  initQ_dev_group = 0.3,
  mu_R_group = 2,
  sigma_R_group = 2
)

#' A list of theoretical bounds for parameters; same as in Stan
# TODO: check which of these are used
param_bounds <- list(
  LR_group = c(0, 1),
  LR_max_group = c(0, 1),
  sigma_LR_group = c(0.01, 10),
  LR = c(0, 1),
  inv_temp_group = c(0, 5),
  inv_temp = c(0, 5),
  initQ_dev_group = c(0, 5),
  initQ_dev = c(0, 9),
  mu_R_group = c(1, 10),
  sigma_R_group = c(0, 10)
)

#' Randomizes given free parameters according to a uniform
#' distribution
#' 
#' @param param_settings list of parameter settings
#' 
#' @param free_params vector of names of free parameters to randomize
#'
#' @return A list of parameter settings with `free_params` randomized
randomize_free_params <- function(param_settings, free_params, seed) {
  set.seed(seed)
  for (p in free_params) {
    bounds <- param_bounds[[p]]
    draw <- runif(n = length(param_settings[[p]]), 
                  min = bounds[1], 
                  max = bounds[2])
    param_settings[[p]] <- draw
  }

  return(param_settings)
}

#' Draws participant-level parameters from the group means for each
#' participant and returns them in one list
#' 
#' @param group_param_settings named list of settings for group parameters
#' 
#' @param n_part number of participants to draw parameters for
#' 
#' @return list of arrays
draw_pp_params <- function(group_param_settings, n_part) {
  pp_params <- list()

  for (p in names(group_param_settings)) {
    group_mean <- group_param_settings[[p]]
    pp_level_name <- str_remove(p, "_group")
    pp_params[[pp_level_name]] <- replicate(n = n_part,
                                            draw_from_group_mean(group_mean, p))
  }
  return(pp_params)
}

#' Draws a value from a normal distribution with given group mean
#' 
#' @details uses standard deviation as defined in `param_stddevs`, and
#' bounds to the distribution as defined in `param_bounds`
#' 
#' @param group_mean the group mean for this parameter
#' 
#' @param p the parameter
#' 
#' @return A value from the distribution of parameter `p`
draw_from_group_mean <- function(group_mean, p) {
  draw <- truncnorm::rtruncnorm(n = 1, 
                     a = param_bounds[[p]][1],
                     b = param_bounds[[p]][2],
                     mean = group_mean,
                     sd = param_stddevs[[p]])

  return(draw)
}

#' Saves parameter settings to file `sim_param_settings.json` and
#' saves simulated data to file `sim_dat.json`
#' 
#' @param params: vector of parameter settings
#' 
#' @param sim_dat: data frame of simulated data
#' 
#' @param dat_file_name: file to save data to
#' 
#' @param free_params_pp: names of participant-level free parameters
#' 
#' @return nothing
save_sim_dat <- function(params, sim_dat, dat_file_name, free_params_pp) {

  # parameter settings -> group means are already in params, now we
  # add participant-level settings 
  for (p in free_params_pp) {
    params[[p]] <- round(sim_dat[[p]][which(sim_dat$trial == 1)], 4)
  }
  param_file_name <- stringr::str_replace(dat_file_name, "dat_", "param_settings_")
  cmdstanr::write_stan_json(params, file = param_file_name)

  # data
  n_part <- params$n_part
  n_trials <- params$n_trials
  choice_c <- matrix(sim_dat$choice_c,
                   nrow = n_part,
                   ncol = n_trials,
                   byrow = TRUE)
  R <- matrix(sim_dat$R,
              nrow = n_part,
              ncol = n_trials,
              byrow = TRUE)
  mu_R <- round(sim_dat[["mu_R"]][which(sim_dat$trial == 1)], 4)
  dat_names <- c("n_part", "n_trials", "choice_c", "R", "mu_R")
  list_dat <- setNames(mget(dat_names), dat_names)
  cmdstanr::write_stan_json(list_dat, file = dat_file_name)
}

#' Checks if given simulated data changed compared to the data in the
#' given JSON file
#' 
#' @param data_file: file path for the JSON file
#' 
#' @param sim_dat: data frame to compare to file
#' 
#' @return TRUE if `sim_dat` changed compared to saved JSON file, else
#' FALSE
did_sim_dat_change <- function(data_file, sim_dat) {
  json_data <- rjson::fromJSON(file = data_file)
  json_choice <- unlist(json_data$choice)
  json_R <- unlist(json_data$R)
  sim_choice <- sim_dat$choice
  sim_R <- sim_dat$R
  if (identical(json_choice, sim_choice) & 
      identical(json_R, sim_R)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
