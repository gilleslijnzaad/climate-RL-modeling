library(dplyr)
library(ggplot2)
library(tidyr)

#' Runs the standard simulation model
#' 
#' @param params named list of parameter settings
#'
#' @return data frame of simulated data
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

#' Runs the discrete learning rate model
#' 
#' @param params named list of parameter settings
#' 
#' @param LR_function which LR function to use (`LR_approx` or `LR_geq`)
#' 
#' @param belief_type whether belief is "stat" or "dyn" 

#' @return data frame of simulated data
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

#' Calculates whether to use confirmatory or disconfirmatory LR
#' 
#' @param LRs vector containing `LR_conf` and `LR_disconf`, respectively
#' 
#' @param R the rating of the current trial
#' 
#' @param belief the belief to compare the rating to
#' 
#' @param margin
#' 
#' @return "conf" or "disconf"
LR_approx <- function(LRs, R, belief, margin) {
  if (abs(R - belief) <= margin) {
    return("conf")
  } else {
    return("disconf")
  }
}

#' Calculates whether to use confirmatory or disconfirmatory LR
#' 
#' @param LRs vector containing `LR_conf` and `LR_disconf`, respectively
#' 
#' @param R the rating of the current trial
#' 
#' @param belief the belief to compare the rating to
#' 
#' @param margin
#' 
#' @return "conf" or "disconf"
LR_geq <- function(R, belief, margin) {
  if (R + margin >= belief) {
    return("conf")
  } else {
    return("disconf")
  }
}

#' Creates a plot of Q-values over time (smooth)
#' 
#' @param   sim_dat: data frame of simulated data
#' 
#' @return  ggplot2 object
Q <- function(sim_dat) {
  # data to long format
  sim_dat <- sim_dat %>%
    pivot_longer(c(Q_F, Q_U), names_prefix = "Q_", names_to = "option", values_to = "Q") %>%
    mutate(option = factor(option),
           choice = factor(choice))

  p <- ggplot(sim_dat, aes(x = trial,
                           y = Q,
                           color = option)) +
    geom_smooth(aes(fill = option)) +
    ylim(c(1, 10)) +
    labs(x = "Trial") +
    scale_color_grey() +  
    scale_fill_grey() +
    my_theme +
    theme(legend.position = "none")

  return(p)
}

#' Creates a plot of choices over time (smooth)
#' 
#' @param   sim_dat: data frame of simulated data
#' 
#' @return  ggplot2 object
choice <- function(sim_dat) {
  plot_dat <- sim_dat %>%
    mutate(choice_F = as.numeric(choice == 1),
           choice_U = as.numeric(choice == 2)) %>%
    pivot_longer(c(choice_F, choice_U), names_prefix = "choice_", names_to = "option", values_to = "choice_prop") %>%
    mutate(option = factor(option))

  p <- ggplot(plot_dat) +
    geom_smooth(aes(x = trial, y = choice_prop, color = option, fill = option)) +
    ylim(c(0, 1)) +
    labs(x = "Trial",
         y = "Proportion chosen") +
    scale_color_grey() +  
    scale_fill_grey() +
    my_theme +
    theme(legend.position = "inside",
          legend.position.inside = c(0.83, 0.91))
  return(p)
}

#' Creates an annotation listing the parameter settings
#' 
#' @param   params named list of parameter settings
#' 
#' @return `textGrob` detailing the parameter settings
param_annotation <- function(params) {
  library(grid)
  full_text <- c()
  for (p in names(params)) {
    if (length(params[[p]]) == 1) {
      add_text <- paste0(p, " = ", params[[p]])
      full_text <- paste(full_text, add_text, sep = "\n")
    } else {
      for (i in 1:length(params[[p]])) {
        param_name <- paste0(p, "$", names(params[[p]][i]))
        add_text <- paste0(param_name, " = ", params[[p]][i])
        full_text <- paste(full_text, add_text, sep = "\n")
      }
    }
  }
  g <- textGrob(label = full_text, x = unit(1, "npc"), y = unit(0.98, "npc"), just = c("right", "top"))
  return(g)
}

#' Combines `Q()` and `choice()` plots with the annotation
#' 
#' @param sim_dat     data frame of simulated data
#' 
#' @param params      named list of parameter settings
#' 
#' @param plot_title  string for plot title; if `NA`, plot will not
#' have a title
#' 
#' @return nothing
sim_plots <- function(sim_dat, params, plot_title = NA) {
  annotation <- param_annotation(params)
  if (is.na(plot_title)) {
    title <- NA
  } else {
    title <- textGrob(plot_title, gp = gpar(fontsize = 20, font = 2))
  }
  
  gridExtra::grid.arrange(
    Q(sim_dat), 
    choice(sim_dat), 
    annotation,
    ncol = 3,
    widths = unit.c(unit(1, "null"), # fill space evenly
                    unit(1, "null"),
                    grobWidth(annotation) + unit(2, "mm")),
    top = title
  )
}

my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 18, face = "bold"))
