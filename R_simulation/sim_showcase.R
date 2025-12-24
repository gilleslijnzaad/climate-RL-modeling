## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----sim-code-----------------------------------------------------------------
rm(list = ls())
setwd("~/research/climate-RL/R_simulation")
source("sim.R")
run_sim

## ----run-std------------------------------------------------------------------
params_std <- list(
  n_part = 10,
  n_trials = 100,
  LR = 0.4,
  inv_temp = 1,
  initQF = 5,
  initQU = 5,
  mu_R = c(8, 3), # F and U
  sigma_R = 3
)

dat <- run_sim(params_std)
gridExtra::grid.arrange(plot_Q(dat), plot_choice(dat), nrow = 1)
my_annotation(params_std)

## ----run-std-LR, echo = FALSE-------------------------------------------------
params <- modifyList(params_std, list(LR = 0.2))
dat <- run_sim(params)
gridExtra::grid.arrange(plot_Q(dat), plot_choice(dat), nrow = 1)
my_annotation(params)

params <- modifyList(params_std, list(LR = 0.8))
dat <- run_sim(params)
gridExtra::grid.arrange(plot_Q(dat), plot_choice(dat), nrow = 1)
my_annotation(params)

## ----run-std-inv-temp, echo = FALSE-------------------------------------------
params <- modifyList(params_std, list(inv_temp = 0))
dat <- run_sim(params)
gridExtra::grid.arrange(plot_Q(dat), plot_choice(dat), nrow = 1)
my_annotation(params)

params <- modifyList(params_std, list(inv_temp = 1.5))
dat <- run_sim(params)
gridExtra::grid.arrange(plot_Q(dat), plot_choice(dat), nrow = 1)
my_annotation(params)

