sim_utils <- new.env()
source("~/research/climate-RL-mod/9_utilities/sim_utils.R", local = sim_utils)

# === run() =============================
# runs this simulation model once
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

  group_params <- params[str_detect(names(params), "_group")]
  pp_params <- sim_utils$draw_pp_params(group_params, n_part)
  
  # the next line attaches pp_params to the environment of this
  # function so we can use (e.g.) LR instead of pp_params$LR
  list2env(pp_params, envir = environment())

  Q_F[, 1] <- initQF
  Q_U[, 1] <- initQU

  for (j in 1:n_part) {
    P_F <- c()
    pred_err <- c()

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp[j] * (Q_F[j, t] - Q_U[j, t])))
      choice[j, t] <- sample(c(1, 2), 
                             size = 1,
                             prob = c(P_F[t], 1 - P_F[t]))

      # rate
      R[j, t] <- round(truncnorm::rtruncnorm(n = 1, a = 1, b = 10,
                                  mean = mu_R[choice[j, t], j], 
                                  sd = sigma_R[j]),
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

# === run_many() =============================
# runs this simulation model many times
# arguments: 
# - settings: vector of experiment settings
# - save_dir: directory to save the simulated data to
# - n_runs: how many times to run the simulation
# 
# returns: 
# - nothing
run_many <- function(settings, save_dir, n_runs) {
  free_params <- c("LR_group", "inv_temp_group", "initQF_group", "initQU_group")

  for (k in 1:n_runs) {
    save_path <- paste0(save_dir, "dat_", sprintf("%03d", k), ".json")
    params <- sim_utils$randomize_free_params(settings, free_params)

    dat <- run(params)
  
    sim_utils$save_sim_dat(params, dat, save_path)
  }
  message(paste0("Finished simulating ", n_runs, " runs."))
}