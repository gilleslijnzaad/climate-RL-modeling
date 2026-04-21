set.seed(1234)
library(dplyr)

#' A list of standard deviations for group-level means of parameters
param_stddevs <- list(
  LR_group = 0.2,
  LR_conf_group = 0.2,
  LR_disconf_group = 0.2,
  inv_temp_group = 0.3,
  initQ_group = 2,
  initQF_group = 2,
  initQU_group = 2,
  mu_R_group = 2,
  sigma_R_group = 2,
  margin_group = 2
)

#' A list of theoretical bounds for parameters; same as in Stan
param_bounds <- list(
  LR_group = c(0, 1),
  LR_conf_group = c(0, 1),
  LR_disconf_group = c(0, 1),
  LR = c(0, 1),
  inv_temp_group = c(0, 5),
  inv_temp = c(0, 5),
  initQ_group = c(1, 10),
  initQF_group = c(1, 10),
  initQU_group = c(1, 10),
  initQF = c(1, 10),
  initQU = c(1, 10),
  mu_R_group = c(1, 10),
  sigma_R_group = c(0, 10),
  margin_group = c(0, 10)
)

#' Randomizes given free parameters according to a uniform
#' distribution
#' 
#' @param param_settings list of parameter settings
#' 
#' @param free_params vector of names of free parameters to randomize
#'
#' @return A list of parameter settings with `free_params` randomized
randomize_free_params <- function(param_settings, free_params) {
  for (p in free_params) {
    bounds <- param_bounds[[p]]
    param_settings[[p]] <- runif(1, min = bounds[1], max = bounds[2])
  }
  return(param_settings)
}

#' Randomly draws a value based on given group mean
#' 
#' @details uses standard deviation as defined in `param_stddevs`, and
#' bounds to the distribution as defined in `param_bounds`
#' 
#' @param param_settings: the list of parameter settings (i.e., group
#' means)
#' 
#' @param p: the parameter for which we want to draw from the group
#' mean
#' 
#' @return A value from the distribution of parameter `p`
draw_from_group_mean <- function(param_settings, p) {
  draw <- truncnorm::rtruncnorm(n = 1, 
                     a = param_bounds[[p]][1],
                     b = param_bounds[[p]][2],
                     mean = param_settings[[p]],
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
#' @return nothing
save_sim_dat <- function(params, sim_dat, dat_file_name, 
                         free_params = c("LR", "inv_temp", "initQF", "initQU")) {

  # parameter settings -> group means are already in params, now we
  # add participant-level settings 
  for (p in free_params) {
    sim_name <- case_when(
      (p == "initQF") ~ "Q_F",
      (p == "initQU") ~ "Q_U",
      TRUE ~ p
    )
    params[[p]] <- round(sim_dat[[sim_name]][which(sim_dat$trial == 1)], 4)
  }
  param_file_name <- stringr::str_replace(dat_file_name, "dat_", "param_settings_")
  cmdstanr::write_stan_json(params, file = param_file_name)

  # data
  n_part <- params$n_part
  n_trials <- params$n_trials
  choice <- matrix(sim_dat$choice,
                   nrow = n_part,
                   ncol = n_trials,
                   byrow = TRUE)
  R <- matrix(sim_dat$R,
              nrow = n_part,
              ncol = n_trials,
              byrow = TRUE)
  dat_names <- c("n_part", "n_trials", "choice", "R")
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

# BELOW: WILL BE DEPRECATED! IS ONLY THERE FOR SIM_SHOWCASE NOW

# === run_LRN_cont() =================
# arguments: vector of parameter settings; whether belief is stat or dyn
# returns: data frame of simulated data
run_LRN_cont <- function(params, belief_type) {
  dat <- data.frame()

  n_part <- params$n_part
  n_trials <- params$n_trials

  for (j in 1:n_part) {

    # ------ init data frames & vectors -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    LR <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    w_LR <- params$w_LR
    inv_temp <- params$inv_temp
    Q$F[1] <- params$initQ$F
    Q$U[1] <- params$initQ$U
    mu_R <- params$mu_R
    sigma_R <- params$sigma_R

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- sample(c(1, 2), 
                          size = 1,
                          prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[t] <- round(truncnorm::rtruncnorm(n = 1, a = 1, b = 10,
                               mean = mu_R[[choice[t]]], 
                               sd = sigma_R),
                    0)

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[t] == 1) {                                 # since t = 0 doesn't exist
          belief <- if (belief_type == "stat") Q[1, 1] else Q[max(t-1, 1), 1]
          LR[t] <- LR_cont(R[t], belief, w_LR)
          Q[t+1, 1] <- Q[t, 1] + LR[t] * pred_err[t]
          Q[t+1, 2] <- Q[t, 2]
        } else {
          belief <- if (belief_type == "stat") Q[1, 2] else Q[max(t-1, 1), 2]
          LR[t] <- LR_cont(R[t], belief, w_LR)
          Q[t+1, 2] <- Q[t, 2] + LR[t] * pred_err[t]
          Q[t+1, 1] <- Q[t, 1]
        }
      }
    }

    dat_p <- data.frame(
      participant = rep(j, n_trials),
      trial =       1:n_trials,
      Q_F =         Q$F,
      Q_U =         Q$U,
      P_F =         P_F,
      LR =          c(LR, NA),
      choice =      choice,
      R =           R,
      pred_err =    pred_err
    )

    dat <- rbind(dat, dat_p)
  }
  return(dat)
}

# === LR_cont() =================
# arguments: the rating of this trial; the belief to compare it to; the learning rate weight
# returns: learning rate for this trial
LR_cont <- function(R, belief, w_LR) {
  diff <- R - belief
  LR_prime <- min(1, 
                  1/9 * diff + 1)
  return(w_LR * LR_prime)
}
