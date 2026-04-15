sim_utils <- new.env()
source("~/research/climate-RL-mod/utilities/sim_utils.R", local = sim_utils)

# TODO: vectorize

# === run() =================
# arguments: 
# - params: vector of parameter settings
# 
# returns: 
# - data frame of simulated data
run <- function(params) {
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
    LRs <- sim_utils$draw_from_group_mean(params, "LRs_group")
    names(LRs) <- c("conf", "disconf")
    inv_temp <- sim_utils$draw_from_group_mean(params, "inv_temp_group")
    initQ <- sim_utils$draw_from_group_mean(params, "initQ_group")
    Q$F[1] <- initQ[1]
    Q$U[1] <- initQ[2]
    mu_R <- sim_utils$draw_from_group_mean(params, "mu_R_group")
    sigma_R <- sim_utils$draw_from_group_mean(params, "sigma_R_group")
    margin <- sim_utils$draw_from_group_mean(params, "margin_group")

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
          belief <- Q[1, 1]
          LR <- LRs[[ LR_geq(R[t], belief, margin) ]]
          Q[t+1, 1] <- Q[t, 1] + LR * pred_err[t]
          Q[t+1, 2] <- Q[t, 2]
        } else {
          belief <- Q[1, 2]
          LR <- LRs[[ LR_geq(R[t], belief, margin) ]]
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
