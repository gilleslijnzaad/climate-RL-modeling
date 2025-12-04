## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----prep---------------------------------------------------------------------
n_participants <- 20
n_trials <- 40
params_std <- c(0.5, 0.5, 6, 3, 5, 3) # LR, inv_temp, Q_F_1, Q_U_1, mu_R, sigma_R
set.seed(1234)

## ----model-std----------------------------------------------------------------
library(truncnorm) # draw from a truncated normal distribution (for rating)

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
    mu_R <- params[5]
    sigma_R <- params[6]

    # --------- run trials ------------
    for (t in 1:n_trials) {

      # choose
      P_F[t] <- 1 / (1 + exp(-inv_temp * (Q$F[t] - Q$U[t])))
      choice[t] <- if_else(runif(1) < P_F[t],
                           "F",
                           "U")

      R[t] <- round(rtruncnorm(n = 1, a = 1, b = 10, 
                        mean = mu_R, 
                        sd = sigma_R),
                    0)

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
                  "\ninitQF = ", params[3],
                  "\ninitQU = ", params[4],
                  "\nmu_R = ", params[5],
                  "\nsigma_R = ", params[6])

    grid.text(text, x = unit(x, "npc"), y = unit(0.95, "npc"), hjust = 1, vjust = 1)
}

annotation_double <- function(params_left, params_right) {
  annotation_single(params_left, 0.45)
  annotation_single(params_right, 0.95)
}

## ----dat-to-JSON--------------------------------------------------------------
library(cmdstanr) # contains function write_stan_json()
dir <- "~/research/climate-RL/R_simulation/"

write_sim_dat_JSON <- function(params, model_dat) {
  # parameter settings
  LR <- params[1]
  inv_temp <- params[2]
  initQF <- params[3]
  initQU <- params[4]
  mu_R <- params[5]
  sigma_R <- params[6]
  T <- n_trials
  n_part <- n_participants
  list_param_settings <- list(LR, inv_temp, initQF, initQU, mu_R, sigma_R, T, n_part)
  names(list_param_settings) <- c("LR", "inv_temp", "initQF", "initQU", "mu_R", "sigma_R", "T", "n_part")
  write_stan_json(list_param_settings, file = paste0(dir, "sim_param_settings.json"))

  # data
  choice <- matrix(as.numeric(model_dat$choice == "U") + 1,
                   nrow = n_part,
                   ncol = T)
  R <- matrix(model_dat$R,
              nrow = n_part,
              ncol = T)
  list_dat <- list(n_part, T ,choice, R)
  names(list_dat) <- c("n_part", "T", "choice", "R")
  write_stan_json(list_dat, file = paste0(dir, "sim_dat.json"))
}

## ----run-std------------------------------------------------------------------
dat_std <- run_model()
write_sim_dat_JSON(params_std, dat_std)
dat_std <- dat_std %>% to_long()
p_left <- plot_Q(dat_std)
p_right <- plot_choice(dat_std)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params_std)

## ----run-std-init-val---------------------------------------------------------
params <- c(params_std[1:2], 9, 2, params_std[5:6])
dat <- run_model(params)
dat <- dat %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----run-std-LR, echo = FALSE-------------------------------------------------
params <- c(0.2, params_std[2:6])
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

params <- c(0.8, params_std[2:6])
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

## ----run-std-inv-temp, echo = FALSE-------------------------------------------
params <- c(params_std[1], 0, params_std[3:6])
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

params <- c(params_std[1], 1.5, params_std[3:6])
dat <- run_model(params) %>% to_long()
p_left <- plot_Q(dat)
p_right <- plot_choice(dat)
grid.arrange(p_left, p_right, nrow = 1)
annotation_single(params)

