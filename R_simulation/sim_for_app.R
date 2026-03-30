# === run_std() =============================
# runs the standard simulation (no confirmation bias)
# arguments: 
# - params: vector of parameter settings
# 
# returns: 
# - data frame of simulated data
run_std <- function(params) {
  set.seed(1234)

  library(truncnorm) # for drawing from truncated distribution
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
    pred_err <- c()

    # ----- initialize parameters -----
    LR <- params$LR
    inv_temp <- params$inv_temp
    Q$F[1] <- params$initQ[["F"]]
    Q$U[1] <- params$initQ[["U"]]
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

# === run_LRN_discr() =================
# arguments: 
# - params: vector of parameter settings; 
# - LR_function: which LR function to use (LR_approx or LR_geq)
# - belief_type: whether belief is "stat" or "dyn"
# 
# returns: 
# - data frame of simulated data
run_LRN_discr <- function(params, LR_function, belief_type) {
  set.seed(1234)

  library(truncnorm) # for drawing from truncated distribution
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
    pred_err <- c()

    # ----- initialize parameters -----
    # TODO: possibly replace this with a loop over params (more elegant)
    LRs <- params$LRs
    names(LRs) <- c("conf", "disconf")
    inv_temp <- params$inv_temp
    Q$F[1] <- params$initQ[["F"]]
    Q$U[1] <- params$initQ[["U"]]
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
  set.seed(1234)

  library(truncnorm) # for drawing from truncated distribution
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
