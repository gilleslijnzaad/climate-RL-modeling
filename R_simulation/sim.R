util <- new.env()

main_dir <- "~/research/climate-RL/"
source(paste0(main_dir, "plot_utils.R"))
sim_dir <- paste0(main_dir, "R_simulation/")

library(dplyr)

# === run_sim =============================
# arguments: vector of parameter settings; whether or not to save data to JSON
# returns: data frame of simulated data
run_sim <- function(params, save_to_JSON = FALSE) {
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

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                           "F",
                           "U")

      # rate
      # 80% of the time, get R that belongs to the choice.
      # other 20%, get R that belongs to the other choice.
      if (runif(1) < 0.8) {
        R[t] <- mu_R[[choice[t]]]
      } else {
        R[t] <- mu_R[[names(mu_R)[names(mu_R) != choice[t]]]]
      }

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        Q[t+1, choice[t]] <- Q[t, choice[t]] + LR * pred_err[t]

        not_chosen <- colnames(Q[which(colnames(Q) != choice[t])])
        Q[t+1, not_chosen] <- Q[t, not_chosen]
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

# === PLOT UTILS ====================
  # === plot_Q ========================
  # arguments: data frame of simulated data
  # returns: ggplot object: smooth plot of Q values over time
  plot_Q <- function(dat) {
    # data to long format
    dat <- dat %>%
      pivot_longer(c(Q_F, Q_U), names_prefix = "Q_", names_to = "option", values_to = "Q") %>%
      mutate(option = factor(option),
            choice = factor(choice))

    p <- ggplot(dat, aes(x = trial,
                        y = Q,
                        color = option)) +
      geom_smooth(aes(fill = option)) +
      ylim(c(1, 10)) +
      labs(x = "Trial") +
      scale_color_manual(values = c(my_teal, my_pink),
                        labels = c("Friendly", "Unfriendly")) +  
      scale_fill_manual(values = c(my_teal, my_pink),
                        labels = c("Friendly", "Unfriendly")) +
      my_theme +
      theme(legend.position = "inside",
            legend.position.inside = c(0.83, 0.91))

    return(p)
  }

  # === plot_choice ========================
  # arguments: data frame of simulated data
  # returns: ggplot object: smooth plot of choices over time
  plot_choice <- function(dat) {
    # data to long format
    dat <- dat %>%
      pivot_longer(c(Q_F, Q_U), names_prefix = "Q_", names_to = "option", values_to = "Q") %>%
      mutate(option = factor(option),
            choice = factor(choice)) %>%
      mutate(choice_is_F = as.numeric(choice == "F"),
            choice_is_U = 1 - choice_is_F)
      
    p <- ggplot(dat, aes(x = trial)) +
      geom_smooth(aes(y = choice_is_F),
                  color = paste0(my_teal, "30"),
                  fill = paste0(my_teal, "30")) +
      geom_smooth(aes(y = choice_is_U),
                  color = paste0(my_pink, "30"),
                  fill = paste0(my_pink, "30")) +
      ylim(c(0, 1)) +
      labs(x = "Trial",
          y = "Proportion chosen") +
      my_theme
    return(p)
  }

  # === my_annotation ======================
# arguments: vector of parameter settings
# returns: nothing
my_annotation <- function(params) {
  library(grid)
  text <- paste0("LR = ", params$LR,
                 "\ninv_temp = ", params$inv_temp,
                 "\ninitQF = ", params$initQF,
                 "\ninitQU = ", params$initQU,
                 "\nmu_R_F = ", params$mu_R[1],
                 "\nmu_R_U = ", params$mu_R[2]
                #  ,"\nsigma_R = ", params$sigma_R
                 )

  grid.text(text, x = unit(0.98, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

# === save_sim_dat =======================
# arguments: vector of parameter settings; data frame of simulated data
# returns: nothing
save_sim_dat <- function(params, sim_dat) {
  library(cmdstanr) # contains function write_stan_json()

  # parameter settings
  write_stan_json(params, file = paste0(sim_dir,"sim_param_settings.json"))

  # data
  n_part <- params$n_part
  n_trials <- params$n_trials
  initQF <- params$initQF
  initQU <- params$initQU
  choice <- matrix(as.numeric(sim_dat$choice == "U") + 1,
                   nrow = n_part,
                   ncol = n_trials)
  R <- matrix(sim_dat$R,
              nrow = n_part,
              ncol = n_trials)
  dat_names <- c("n_part", "n_trials", "initQF", "initQU", "choice", "R")
  list_dat <- setNames(mget(dat_names), dat_names)
  write_stan_json(list_dat, file = paste0(sim_dir, "sim_dat.json"))
}

# ---------------------------------------------

while ("util" %in% search())
  detach("util")

attach(util)
