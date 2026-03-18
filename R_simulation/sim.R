# === run_std() =============================
# arguments: vector of parameter settings
# returns: data frame of simulated data
run_std <- function(params) {
  library(truncnorm) # for drawing from truncated distribution
  dat <- data.frame()

  n_part <- params$n_part
  n_trials <- params$n_trials

  for (j in 1:n_part) {
    set.seed(j)

    # ------ init data frames & vectors -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LR <- params$LR
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
      R[t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
                               mean = mu_R[[choice[t]]], 
                               sd = sigma_R),
                    0)

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[t] == 1) {
          Q[t+1, 1] <- Q[t, 1] + LR * pred_err[t]
          Q[t+1, 2] <- Q[t, 2]
        } else {
          Q[t+1, 2] <- Q[t, 2] + LR * pred_err[t]
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
      choice =      choice,
      R =           R,
      pred_err =    pred_err
    )

    dat <- rbind(dat, dat_p)
  }
  return(dat)
}
# === end of run_std()

run_std_hrch <- function(params) {
  library(truncnorm) # for drawing from truncated distribution
  dat <- data.frame()

  n_part <- params$n_part
  n_trials <- params$n_trials

  for (j in 1:n_part) {
    set.seed(j)

    # ------ init data frames & vectors -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    attach(params, warn.conflicts = FALSE)
    LR <-       rtruncnorm(n = 1, a = 0, b = 1, mean = LR_group, sd = 0.2)
    inv_temp <- rtruncnorm(n = 1, a = 0, b = Inf, mean = inv_temp_group, sd = 0.3)
    Q$F[1] <-   rtruncnorm(n = 1, a = 1, b = 10, mean = initQ_group$F, sd = 2)
    Q$U[1] <-   rtruncnorm(n = 1, a = 1, b = 10, mean = initQ_group$U, sd = 2)
    mu_R <-   c(rtruncnorm(n = 1, a = 1, b = 10, mean = mu_R_group[1], sd = 2),
                rtruncnorm(n = 1, a = 1, b = 10, mean = mu_R_group[2], sd = 2))
    sigma_R <-  rtruncnorm(n = 1, a = 0, b = 10, mean = sigma_R_group, sd = 2)
    detach(params)
    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- sample(c(1, 2), 
                          size = 1,
                          prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
                               mean = mu_R[[choice[t]]], 
                               sd = sigma_R),
                    0)

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[t] == 1) {
          Q[t+1, 1] <- Q[t, 1] + LR * pred_err[t]
          Q[t+1, 2] <- Q[t, 2]
        } else {
          Q[t+1, 2] <- Q[t, 2] + LR * pred_err[t]
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
      choice =      choice,
      R =           R,
      pred_err =    pred_err
    )

    dat <- rbind(dat, dat_p)
  }
  return(dat)
}


# === save_sim_dat() =======================
# arguments: vector of parameter settings; data frame of simulated data
# returns: nothing
save_sim_dat <- function(params, sim_dat) {
  library(cmdstanr) # contains function write_stan_json()

  # parameter settings
  write_stan_json(params, file = paste0(sim_dir, "sim_param_settings.json"))

  # data
  n_part <- params$n_part
  n_trials <- params$n_trials
  initQF <- params$initQF
  initQU <- params$initQU
  choice <- matrix(sim_dat$choice,
                   nrow = n_part,
                   ncol = n_trials,
                   byrow = TRUE)
  R <- matrix(sim_dat$R,
              nrow = n_part,
              ncol = n_trials,
              byrow = TRUE)
  dat_names <- c("n_part", "n_trials", "initQF", "initQU", "choice", "R")
  list_dat <- setNames(mget(dat_names), dat_names)
  write_stan_json(list_dat, file = paste0(sim_dir, "sim_dat.json"))
}

# === did_sim_dat_change() =================
# arguments: file path for the JSON file, data frame of simulated data
# returns: TRUE if sim_dat changed compared to saved JSON file, else FALSE
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

# === run_LRN_discr() =================
# arguments: vector of parameter settings; which LR function to use; whether belief is stat or dyn
# returns: data frame of simulated data
run_LRN_discr <- function(params, LR_function, belief_type) {
  library(truncnorm) # for drawing from truncated distribution
  dat <- data.frame()

  n_part <- params$n_part
  n_trials <- params$n_trials

  for (j in 1:n_part) {
    set.seed(j)

    # ------ init data frames & vectors -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LRs <- params$LRs
    inv_temp <- params$inv_temp
    Q$F[1] <- params$initQ$F
    Q$U[1] <- params$initQ$U
    mu_R <- params$mu_R
    sigma_R <- params$sigma_R
    margin <- params$margin

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- sample(c(1, 2), 
                          size = 1,
                          prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
                               mean = mu_R[[choice[t]]], 
                               sd = sigma_R),
                    0)

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[t] == 1) {
          belief <- if (belief_type == "stat") Q[1, 1] else  Q[max(t-1, 1), 1]
          LR <- LRs[[ LR_function(R[t], belief, margin) ]]
          Q[t+1, 1] <- Q[t, 1] + LR * pred_err[t]
          Q[t+1, 2] <- Q[t, 2]
        } else {
          belief <- if (belief_type == "stat") Q[1, 2] else Q[max(t-1, 1), 2]
          LR <- LRs[[ LR_function(R[t], belief, margin) ]]
          Q[t+1, 2] <- Q[t, 2] + LR * pred_err[t]
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
      LR =          LR,
      choice =      choice,
      R =           R,
      pred_err =    pred_err
    )

    dat <- rbind(dat, dat_p)
  }
  return(dat)
}

# === LR_approx() =================
# arguments: the rating of this trial; the belief to compare it to; the margin
# returns: "conf" or "disconf"
LR_approx <- function(R, belief, margin) {
  if (abs(R - belief) <= margin) {
    return("conf")
  } else {
    return("disconf")
  }
}

# === LR_geq() =================
# arguments: the rating of this trial; the belief to compare it to; the margin
# returns: "conf" or "disconf"
LR_geq <- function(R, belief, margin) {
  if (R + margin >= belief) {
    return("conf")
  } else {
    return("disconf")
  }
}

# === run_LRN_cont() =================
# arguments: vector of parameter settings; whether belief is stat or dyn
# returns: data frame of simulated data
run_LRN_cont <- function(params, belief_type) {
  library(truncnorm) # for drawing from truncated distribution
  dat <- data.frame()

  n_part <- params$n_part
  n_trials <- params$n_trials

  for (j in 1:n_part) {
    set.seed(j)

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
      R[t] <- round(rtruncnorm(n = 1, a = 1, b = 10,
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
