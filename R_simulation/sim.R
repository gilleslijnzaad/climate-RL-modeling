## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----prep---------------------------------------------------------------------
n_participants <- 1
n_trials <- 40
params_std <- c(0.5, 0.5, 5, 5) # LR, inv_temp, Q_F_1, Q_U_1
set.seed(1234)

## ----fun-rating---------------------------------------------------------------
library(truncnorm)
mu_R <- 7
sigma_R <- 3
rating <- function() {
  R <- round(rtruncnorm(n = 1, a = 1, b = 10, 
                        mean = mu_R, 
                        sd = sigma_R), 
             0)
  return(R)
}

## ----model-std----------------------------------------------------------------
run_model <- function(params = params_std) {
  dat <- data.frame()

  for (j in 1:n_participants) {

    # ------ init data frames etc -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LR <- params[1]
    inv_temp <- params[2]
    Q$F[1] <- params[3]
    Q$U[1] <- params[4]

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                          "F",
                          "U")

      # rate
      R[t] <- rating()

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

  return(dat)
}

## ----plot-util----------------------------------------------------------------
library(tidyverse)
my_teal <- "#008080"
my_pink <- "#ff00dd"

my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))

## ----helper-fun---------------------------------------------------------------
library(grid)
library(gridExtra)

to_long <- function(dat) {
  long_dat <- dat %>%
    pivot_longer(c(Q_F, Q_U), names_prefix = "Q_", names_to = "option", values_to = "Q") %>%
    mutate(option = factor(option),
           choice = factor(choice))

  return(long_dat)
}

plot_Q <- function(dat) {
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
    my_theme
  return(p)
}

plot_choice <- function(dat) {
  dat <- dat %>%
    mutate(choice_is_F = if_else(choice == "F", 1, 0),
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

annotation_single <- function(params, x = 0.95) {
    text <- paste0("LR = ", params[1],
                  "\ninv_temp = ", params[2],
                  "\nQ_F[1] = ", params[3],
                  "\nQ_U[1] = ", params[4])

    grid.text(text, x = unit(x, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

annotation_double <- function(params_left, params_right) {
  annotation_single(params_left, 0.45)
  annotation_single(params_right, 0.95)
}

## ----dat-to-JSON--------------------------------------------------------------
library(rjson)
model_dat_JSON <- function(params, model_dat) {
  LR <- params[1]
  inv_temp <- params[2]
  initQF <- params[3]
  initQU <- params[4]
  T <- n_trials
  choice <- as.numeric(model_dat$choice == "U") + 1
  R <- model_dat$R
  my_list <- list(LR, inv_temp, initQF, initQU, mu_R, sigma_R, T, choice, R)
  names(my_list) <- c("LR", "inv_temp", "initQF", "initQU", "mu_R", "sigma_R", "T", "choice", "R")
  return(toJSON(my_list))
}

## ----run-std, warning = FALSE-------------------------------------------------
dat_std <- run_model()
write(model_dat_JSON(params_std, dat_std), "sim_dat.json")
dat_std <- dat_std %>% to_long()
p_left <- plot_Q(dat_std)
p_right <- plot_choice(dat_std)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params_std)

## ----run-std-init-val, warning = FALSE----------------------------------------
params <- c(0.5, 0.5, 8, 3)
dat <- run_model(params)
write(model_dat_JSON(params, dat), "sim_dat.json")
dat <- dat %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----run-std-LR, echo = FALSE, warning = FALSE--------------------------------
params <- c(0.2, 0.5, 5, 5)
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

params <- c(0.8, 0.5, 5, 5)
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----run-std-inv-temp, echo = FALSE-------------------------------------------
params <- c(0.5, 0, 5, 5)
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

params <- c(0.5, 1.5, 5, 5)
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----fun-conf-----------------------------------------------------------------
threshold <- 2

is_confirm <- function(rating, init_value) {
  return(abs(rating - init_value) <= threshold)
}

## ----model-dualLR-------------------------------------------------------------
params_std <- c(0.7, 0.3, 0.5, 8, 4) # LR_conf, LR_disconf, inv_temp, Q_F_1, Q_U_1

run_model_dualLR <- function(params = params_std) {
  dat <- data.frame()

  for (j in 1:n_participants) {

    # ------ init data frames etc -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LR_conf <- params[1]
    LR_disconf <- params[2]
    inv_temp <- params[3]
    Q$F[1] <- params[4]
    Q$U[1] <- params[5]

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                          "F",
                          "U")

      # rate
      R[t] <- rating()

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      # which learning rate?
      LR <- if_else(is_confirm(R[t], Q[1, choice[t]]),
                    LR_conf,
                    LR_disconf)

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

  return(dat)
}

## ----helper-fun-2, echo = FALSE-----------------------------------------------
annotation_single <- function(params, x = 0.95) {
  text <- paste0("LR_conf = ", params[1],
               "\nLR_disconf = ", params[2], 
               "\ninv_temp = ", params[3],
               "\nQ_F[1] = ", params[4],
               "\nQ_U[1] = ", params[5])

  grid.text(text, x = unit(x, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

## ----run-dualLR---------------------------------------------------------------
dat <- run_model_dualLR() %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params_std)

## ----fun-rating-bonus---------------------------------------------------------
bonus <- 3

rating <- function(init_value_chosen, init_value_not_chosen) {
  R <- round(rtruncnorm(n = 1, a = 1, b = 10, 
                        mean = 5, 
                        sd = 3), 
             0)

  if (init_value_chosen > init_value_not_chosen) {
    bonus_polarity <- 1
  } else if (init_value_chosen < init_value_not_chosen) {
    bonus_polarity <- -1
  } else {
    bonus_polarity <- 0
  }

  R <- R + 
    is_confirm(R, init_value_chosen) * bonus * bonus_polarity
  return(min(R, 10))    # max rating is still 10
}

## ----model-rating-bonus, echo = FALSE-----------------------------------------
run_model <- function(params = params_std) {
  dat <- data.frame()

  for (j in 1:n_participants) {

    # ------ init data frames etc -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LR <- params[1]
    inv_temp <- params[2]
    Q$F[1] <- params[3]
    Q$U[1] <- params[4]

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                          "F",
                          "U")
      not_chosen <- colnames(Q[which(colnames(Q) != choice[t])])

      # rate
      R[t] <- rating(Q[1, choice[t]],
                     Q[1, not_chosen])

      # learn
      pred_err[t] <- R[t] - Q[t, choice[t]]

      if (t < n_trials) {   # no updating Qs in the very last trial
        Q[t+1, choice[t]] <- Q[t, choice[t]] + LR * pred_err[t]

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

  return(dat)
}

annotation_single <- function(params, x = 0.95) {
    text <- paste0("LR = ", params[1],
                  "\ninv_temp = ", params[2],
                  "\nQ_F[1] = ", params[3],
                  "\nQ_U[1] = ", params[4])

    grid.text(text, x = unit(x, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

## ----run-rating-bonus, warning = FALSE----------------------------------------
params <- c(0.5, 0.5, 6, 4) # LR, inv_temp, Q_F_1, Q_U_1
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----fun-rating-diffdistr-----------------------------------------------------
rating <- function(init_value) {
  R <- round(rtruncnorm(n = 1, a = 1, b = 10, 
                        mean = init_value, 
                        sd = 3), 
             0)
  return(R)
}

## ----model-rating-diffdistr, echo = FALSE-------------------------------------
run_model <- function(params = params_std) {
  dat <- data.frame()

  for (j in 1:n_participants) {

    # ------ init data frames etc -----
    Q <- data.frame(
      F = rep(NA, n_trials),
      U = rep(NA, n_trials)
    )
    P_F <- c()
    choice <- c()
    R <- c()
    pred_err <- c()

    # ----- initialize parameters -----
    LR <- params[1]
    inv_temp <- params[2]
    Q$F[1] <- params[3]
    Q$U[1] <- params[4]

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                          "F",
                          "U")

      # rate
      R[t] <- rating(Q[1, choice[t]])

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

  return(dat)
}

annotation_single <- function(params, x = 0.95) {
    text <- paste0("LR = ", params[1],
                  "\ninv_temp = ", params[2],
                  "\nQ_F[1] = ", params[3],
                  "\nQ_U[1] = ", params[4])

    grid.text(text, x = unit(x, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

## ----run-rating-diffdistr, warning = FALSE------------------------------------
params <- c(0.5, 0.5, 7, 3) # LR, inv_temp, Q_F_1, Q_U_1
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

