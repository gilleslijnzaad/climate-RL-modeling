## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----sim-code, comment = NA---------------------------------------------------
rm(list = ls())
# setwd("R_simulation/")

sim <- new.env()
source("sim.R", local = sim)  # access functions using sim$fun()

temp <- readLines("sim.R")
start <- grep("# === run_std", temp)
end <- grep("# === end of run_std()", temp) - 1
cat(temp[start:end], sep = "\n")

## ----run-std-hrch-------------------------------------------------------------
params_std_hrch <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 0.5,
  initQ_group = list(F = 8, U = 2),
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)
plot <- new.env()
source("../plot_utils.R", local = plot)  # access functions using plot$fun()

dat <- sim$run_std_hrch(params_std_hrch)
plot$sim_plots(dat, params_std_hrch)

## ----run-std------------------------------------------------------------------
plot <- new.env()
source("../plot_utils.R", local = plot)  # access functions using plot$fun()

params_std <- list(
  n_part = 50,
  n_trials = 30,
  LR = 0.4,
  inv_temp = 0.5,
  initQ = list(F = 8, U = 2),
  mu_R = list(F = 5, U = 5),
  sigma_R = 2
)

dat <- sim$run_std(params_std)
plot$sim_plots(dat, params_std)

## ----run-std-LR---------------------------------------------------------------
params <- modifyList(params_std, list(LR = 0.2))
dat <- sim$run_std(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(LR = 0.8))
dat <- sim$run_std(params)
plot$sim_plots(dat, params)

## ----run-std-inv-temp---------------------------------------------------------
params <- modifyList(params_std, list(inv_temp = 0))
dat <- sim$run_std(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(inv_temp = 1.5))
dat <- sim$run_std(params)
plot$sim_plots(dat, params)

## ----params-discr-approx------------------------------------------------------
params_LRN_discr <- list(
  n_part = 50,
  n_trials = 30,
  LRs = list(conf = 0.8, disconf = 0.2),
  inv_temp = 0.5,
  initQ = list(F = 8, U = 2),
  mu_R = list(F = 5, U = 5),
  sigma_R = 2,
  margin = 2
)

## ----run-LRN-discr-approx-stat------------------------------------------------
dat <- sim$run_LRN_discr(params_LRN_discr, sim$LR_approx, "stat")
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-approx-dyn-------------------------------------------------
dat <- sim$run_LRN_discr(params_LRN_discr, sim$LR_approx, "dyn")
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-geq-stat---------------------------------------------------
dat <- sim$run_LRN_discr(params_LRN_discr, sim$LR_geq, "stat")
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-geq-dyn----------------------------------------------------
dat <- sim$run_LRN_discr(params_LRN_discr, sim$LR_geq, "dyn")
plot$sim_plots(dat, params_LRN_discr)

## ----illustrate-rel-LR, fig.width = 5, fig.height = 3-------------------------
dummy <- data.frame(
  diff = c(-9, 0, 9),
  LR_prime = c(0, 1, 1)
)
ggplot(dummy) + 
  geom_line(aes(x = diff, y = LR_prime)) +
  geom_vline(aes(xintercept = 0), lty = 2) +
  scale_x_continuous(breaks = seq(-9, 9, 3)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("0", expression(w[LR]))) +
  labs(x = "R - belief", y = "LR") +
  plot$my_theme_classic

## ----run-LRN-cont-stat--------------------------------------------------------
params_LRN_cont <- list(
  n_part = 50,
  n_trials = 30,
  w_LR = 0.8,
  inv_temp = 0.5,
  initQ = list(F = 8, U = 2),
  mu_R = list(F = 5, U = 5),
  sigma_R = 2
)

dat <- sim$run_LRN_cont(params_LRN_cont, "stat")
plot$sim_plots(dat, params_LRN_cont)

## ----run-LRN-cont-dyn---------------------------------------------------------
dat <- sim$run_LRN_cont(params_LRN_cont, "dyn")
plot$sim_plots(dat, params_LRN_cont)

