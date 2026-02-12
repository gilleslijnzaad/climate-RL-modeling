library(dplyr)

# === run_sim() =============================
# arguments: vector of parameter settings; whether or not to save data to JSON
# returns: data frame of simulated data
run_sim <- function(params, save_to_JSON = FALSE) {
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
    Q$F[1] <- params$initQF
    Q$U[1] <- params$initQU
    mu_R <- params$mu_R
    names(mu_R) <- c("F", "U")
    sigma_R <- params$sigma_R

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- sample(c("F", "U"), 
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
        if (choice[t] == "F") {
          Q[t+1, "F"] <- Q[t, "F"] + LR * pred_err[t]
          Q[t+1, "U"] <- Q[t, "U"]
        } else {
          Q[t+1, "U"] <- Q[t, "U"] + LR * pred_err[t]
          Q[t+1, "F"] <- Q[t, "F"]
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
  if (save_to_JSON) {
    save_sim_dat(params, dat)
  }
  return(dat)
}
# === end of run_sim

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
  choice <- matrix(as.numeric(sim_dat$choice == "U") + 1,
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
  sim_choice <- as.numeric(sim_dat$choice == "U") + 1
  sim_R <- sim_dat$R
  if (identical(json_choice, sim_choice) & 
      identical(json_R, sim_R)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

