sim_utils <- new.env()
source("~/research/climate-RL-mod/utilities/sim_utils.R", local = sim_utils)

# === run_std() =============================
# runs the standard simulation (no confirmation bias)
# arguments: 
# - params: vector of parameter settings
# 
# returns: 
# - data frame of simulated data
run <- function(params) {
  library(truncnorm) # for drawing from truncated distribution

  # ------ initialize ------
  n_part <- params$n_part
  n_trials <- params$n_trials
  Q_F <- matrix(ncol = n_trials, nrow = n_part)
  Q_U <- matrix(ncol = n_trials, nrow = n_part)
  choice <- matrix(ncol = n_trials, nrow = n_part)
  R <- matrix(ncol = n_trials, nrow = n_part)
  pred_err <- matrix(ncol = n_trials, nrow = n_part)
  LR <- c()
  inv_temp <- c()

  for (j in 1:n_part) {
    P_F <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    # TODO: possibly replace this with a loop over params (more elegant)
    LR[j] <- sim_utils$draw_from_group_mean(params, "LR_group")
    inv_temp[j] <- sim_utils$draw_from_group_mean(params, "inv_temp_group")
    Q_F[j, 1] <- sim_utils$draw_from_group_mean(params, "initQF_group")
    Q_U[j, 1] <- sim_utils$draw_from_group_mean(params, "initQU_group")
    mu_R <- sim_utils$draw_from_group_mean(params, "mu_R_group")
    sigma_R <- sim_utils$draw_from_group_mean(params, "sigma_R_group")

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp[j] * (Q_F[j, t] - Q_U[j, t])))
      choice[j, t] <- sample(c(1, 2), 
                             size = 1,
                             prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[j, t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
                                  mean = mu_R[[choice[j, t]]], 
                                  sd = sigma_R),
                       0)

      # learn
      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[j, t] == 1) {
          pred_err[t] <- R[j, t] - Q_F[j, t]
          Q_F[j, t+1] <- Q_F[j, t] + LR[j] * pred_err[t]
          Q_U[j, t+1] <- Q_U[j, t]
        } else {
          pred_err[t] <- R[j, t] - Q_U[j, t]
          Q_U[j, t+1] <- Q_U[j, t] + LR[j] * pred_err[t]
          Q_F[j, t+1] <- Q_F[j, t]
        }
      }
    }
  }

  dat <- data.frame(
    participant =   rep(seq_len(n_part), each = n_trials),
    trial =         rep(seq_len(n_trials), n_part),
    Q_F =           array(t(Q_F)),
    Q_U =           array(t(Q_U)),
    choice =        array(t(choice)),
    R =             array(t(R)),
    LR =            rep(LR, each = n_trials),
    inv_temp =      rep(inv_temp, each = n_trials)
  )
  return(dat)
}

# description goes here
run_and_save <- function(settings, save_dir, n_runs) {
  library(truncnorm) # for drawing from truncated distribution
  free_params <- c("LR_group", "inv_temp_group", "initQF_group", "initQU_group")

  for (k in 1:n_runs) {
    save_path <- paste0(save_dir, "dat_", sprintf("%03d", k), ".json")
    params <- sim_utils$randomize_free_params(settings, free_params)

    # ------ initialize experiment settings and data frames ------
    n_part <- settings$n_part
    n_trials <- settings$n_trials
    Q_F <- matrix(ncol = n_trials, nrow = n_part)
    Q_U <- matrix(ncol = n_trials, nrow = n_part)
    choice <- matrix(ncol = n_trials, nrow = n_part)
    R <- matrix(ncol = n_trials, nrow = n_part)
    pred_err <- matrix(ncol = n_trials, nrow = n_part)
    LR <- c()
    inv_temp <- c()

    # --------- participant loop ------------
    for (j in 1:n_part) {
      P_F <- c()
      pred_err <- c()

      # ----- initialize parameters -----
      # TODO: possibly replace this with a loop over params (more elegant)
      LR[j] <- sim_utils$sim_utils$draw_from_group_mean(params, "LR_group")
      inv_temp[j] <- sim_utils$sim_utils$draw_from_group_mean(params, "inv_temp_group")
      Q_F[j, 1] <- sim_utils$sim_utils$draw_from_group_mean(params, "initQF_group")
      Q_U[j, 1] <- sim_utils$sim_utils$draw_from_group_mean(params, "initQU_group")
      mu_R <- sim_utils$sim_utils$draw_from_group_mean(params, "mu_R_group")
      sigma_R <- sim_utils$sim_utils$draw_from_group_mean(params, "sigma_R_group")

      # --------- trial loop ------------
      for (t in 1:n_trials) {

        # choose
        P_F[t] <- 1 / (1 + exp(-inv_temp[j] * (Q_F[j, t] - Q_U[j, t])))
        choice[j, t] <- sample(c(1, 2), 
                              size = 1,
                              prob = c(P_F[t], 1 - P_F[t]))

        # rate
        R[j, t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
                                    mean = mu_R[[choice[j, t]]], 
                                    sd = sigma_R),
                        0)

        # learn
        if (t < n_trials) {   # no updating Qs in the very last trial
          if (choice[j, t] == 1) {
            pred_err[t] <- R[j, t] - Q_F[j, t]
            Q_F[j, t+1] <- Q_F[j, t] + LR[j] * pred_err[t]
            Q_U[j, t+1] <- Q_U[j, t]
          } else {
            pred_err[t] <- R[j, t] - Q_U[j, t]
            Q_U[j, t+1] <- Q_U[j, t] + LR[j] * pred_err[t]
            Q_F[j, t+1] <- Q_F[j, t]
          }
        }
      }
    }

    # --------- save ------------
    dat <- data.frame(
      participant =   rep(seq_len(n_part), each = n_trials),
      trial =         rep(seq_len(n_trials), n_part),
      Q_F =           array(t(Q_F)),
      Q_U =           array(t(Q_U)),
      choice =        array(t(choice)),
      R =             array(t(R)),
      LR =            rep(LR, each = n_trials),
      inv_temp =      rep(inv_temp, each = n_trials)
    )

    sim_utils$save_sim_dat(params, dat, save_path)
  }
}