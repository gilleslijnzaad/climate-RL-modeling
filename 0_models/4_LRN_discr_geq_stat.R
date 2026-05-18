sim_utils <- new.env()
source("~/research/climate-RL-mod/9_utilities/sim_utils.R", local = sim_utils)

#' Runs this simulation model once
#' 
#' @param params named list of parameter settings
#' 
#' @return data frame of simulated data
run <- function(params) {
  # ------ initialize ------
  n_part <- params$n_part
  n_trials <- params$n_trials

  Q_pref <- matrix(ncol = n_trials, nrow = n_part)
  Q_nonpref <- matrix(ncol = n_trials, nrow = n_part)
  choice_c <- matrix(ncol = n_trials, nrow = n_part)
  R <- matrix(ncol = n_trials, nrow = n_part)

  group_params <- params[str_detect(names(params), "_group")]
  pp_params <- sim_utils$draw_pp_params(group_params, n_part)

  # the next line attaches pp_params to the environment of this
  # function so we can use (e.g.) LR instead of pp_params$LR
  list2env(pp_params, envir = environment())
  margin <- params[["margin_group"]]

  for (j in 1:n_part) {
    # set initQs
    Q_pref[j, 1]    <- min(mu_R[j] + initQ_dev[j], 10)
    Q_nonpref[j, 1] <- max(mu_R[j] - initQ_dev[j], 1)
    
    # --------- run trials ------------
    for (t in 1:n_trials) {
      # choose
      if (t == 1) {
        choice_c[j, t] <- 1
      } else {
        P_pref <- 1 / (1 + exp(-inv_temp[j] * (Q_pref[j, t] - Q_nonpref[j, t])))
        choice_c[j, t] <- sample(c(1, 2), 
                              size = 1,
                              prob = c(P_pref, 1 - P_pref))
      }

      # rate
      R[j, t] <- round(truncnorm::rtruncnorm(n = 1, a = 1, b = 10,
                                  mean = mu_R[j], 
                                  sd = sigma_R[j]),
                       0)

          # learn
      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice_c[j, t] == 1) {    # choice was preferred option
          pred_err <- R[j, t] - Q_pref[j, t]
          belief <- Q_pref[j, 1]
          LR <- LR_geq(LRs[, j], R[j, t], belief, margin)
          Q_pref[j, t+1] <- Q_pref[j, t] + LR * pred_err
          Q_nonpref[j, t+1] <- Q_nonpref[j, t]
        } else {                      # choice was nonpreferred option
          pred_err <- R[j, t] - Q_nonpref[j, t]
          belief <- Q_nonpref[j, 1]
          LR <- LR_geq(LRs[, j], R[j, t], belief, margin)
          Q_nonpref[j, t+1] <- Q_nonpref[j, t] + LR * pred_err
          Q_pref[j, t+1] <- Q_pref[j, t]
        }
      }
    }
  }

  dat <- data.frame(
    participant =   rep(seq_len(n_part), each = n_trials),
    trial =         rep(seq_len(n_trials), n_part),
    Q_pref =        array(t(Q_pref)),
    Q_nonpref =     array(t(Q_nonpref)),
    choice_c =      array(t(choice_c)),
    R =             array(t(R)),
    mu_R =          rep(mu_R, each = n_trials),
    LR_disconf =    rep(LRs[1, ], each = n_trials),
    LR_diff =       rep(LRs[2, ], each = n_trials),
    inv_temp =      rep(inv_temp, each = n_trials),
    initQ_dev =     rep(initQ_dev, each = n_trials)
  )  
  return(dat)
}

#' Calculates whether to use confirmatory or disconfirmatory LR
#' 
#' @param LRs vector containing `LR_disconf` and `LR_diff`, respectively
#' 
#' @param R the rating of the current trial
#' 
#' @param belief the belief to compare the rating to
#' 
#' @param margin
#' 
#' @return either the confirmatory LR or the disconfirmatory LR
LR_geq <- function(LRs, R, belief, margin) {
  if (R + margin >= belief) {
    return(LRs[1] + LRs[2]) # confirmatory
  } else {
    return(LRs[1])
  }
}

#' Runs this simulation model many times
#' 
#' @param settings named list of experiment settings
#' 
#' @param save_dir directory to save the simulated data to
#' 
#' @param n_runs how many times to run the simulation
#' 
#' @return nothing
run_many <- function(settings, save_dir, n_runs) {
  free_params_group <- c("LRs_group", "inv_temp_group", "initQF_group", "initQU_group")
  free_params <- c("LR_disconf", "LR_diff", "inv_temp", "initQF", "initQU")

  for (k in 1:n_runs) {
    save_path <- paste0(save_dir, "dat_", sprintf("%03d", k), ".json")
    params <- sim_utils$randomize_free_params(settings, free_params_group, k)

    dat <- run(params, seed = k)

    sim_utils$save_sim_dat(params, dat, save_path, free_params)
  }
  message(paste0("Finished simulating ", n_runs, " runs."))
}
