sim_utils <- new.env()
source("~/research/climate-RL-mod/9_utilities/sim_utils.R", local = sim_utils)

# === run() =================
# arguments: 
# - params: vector of parameter settings
# 
# returns: 
# - data frame of simulated data
run <- function(params) {
  # ------ initialize ------
  n_part <- params$n_part
  n_trials <- params$n_trials
  Q_F <- matrix(ncol = n_trials, nrow = n_part)
  Q_U <- matrix(ncol = n_trials, nrow = n_part)
  choice <- matrix(ncol = n_trials, nrow = n_part)
  R <- matrix(ncol = n_trials, nrow = n_part)
  pred_err <- matrix(ncol = n_trials, nrow = n_part)
  LR_conf <- c()
  LR_disconf <- c()
  inv_temp <- c()

  for (j in 1:n_part) {
    P_F <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    # TODO: possibly replace this with a loop over params (more elegant)
    LR_conf[j] <- sim_utils$draw_from_group_mean(params, "LR_conf_group")
    LR_disconf[j] <- sim_utils$draw_from_group_mean(params, "LR_disconf_group")
    inv_temp[j] <- sim_utils$draw_from_group_mean(params, "inv_temp_group")
    Q_F[j, 1] <- sim_utils$draw_from_group_mean(params, "initQF_group")
    Q_U[j, 1] <- sim_utils$draw_from_group_mean(params, "initQU_group")
    mu_R <- sim_utils$draw_from_group_mean(params, "mu_R_group")
    sigma_R <- sim_utils$draw_from_group_mean(params, "sigma_R_group")
    margin <- sim_utils$draw_from_group_mean(params, "margin_group")

    LRs <- c(LR_conf[j], LR_disconf[j])
    # --------- run trials ------------
    for (t in 1:n_trials) {
      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp[j] * (Q_F[j, t] - Q_U[j, t])))
      choice[j, t] <- sample(c(1, 2), 
                          size = 1,
                          prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[j, t] <- round(truncnorm::rtruncnorm(n = 1, a = 1, b = 10,
                                  mean = mu_R[[choice[j, t]]], 
                                  sd = sigma_R),
                       0)

      # learn
      if (t < n_trials) {   # no updating Qs in the very last trial
        if (choice[j, t] == 1) {
          pred_err[t] <- R[j, t] - Q_F[j, t]
          belief <- Q_F[j, max(t-1, 1)]
          LR <- LR_geq(LRs, R[j, t], belief, margin)
          Q_F[j, t+1] <- Q_F[j, t] + LR * pred_err[t]
          Q_U[j, t+1] <- Q_U[j, t]
        } else {
          pred_err[t] <- R[j, t] - Q_U[j, t]
          belief <- Q_U[j, max(t-1, 1)]
          LR <- LR_geq(LRs, R[j, t], belief, margin)
          Q_U[j, t+1] <- Q_U[j, t] + LR * pred_err[t]
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
    LR_conf =       rep(LR_conf, each = n_trials),
    LR_disconf =    rep(LR_disconf, each = n_trials),
    inv_temp =      rep(inv_temp, each = n_trials)
  )  
  return(dat)
}

# === LR_geq() =================
# arguments: 
# - LRs: vector of the two learning rates
# - R: the rating of this trial
# - belief: the belief to compare the rating to
# - margin
# 
# returns: 
# - either the confirmatory LR or disconfirmatory LR
LR_geq <- function(LRs, R, belief, margin) {
  if (R + margin >= belief) {
    return(LRs[1]) # confirmatory
  } else {
    return(LRs[2])
  }
}
