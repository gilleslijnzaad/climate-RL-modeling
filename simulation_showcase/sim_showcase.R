## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----run-std------------------------------------------------------------------
rm(list = ls())
# setwd("~/research/climate-RL-mod/simulation_showcase")
main_dir <- "~/research/climate-RL-mod/"
model_dir <- paste0(main_dir, "models/")
util_dir <- paste0(main_dir, "utilities/")

plot <- new.env()
source(paste0(util_dir,"plot_utils.R"), local = plot)  # access functions using plot$fun()

params_std <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)

sim <- new.env()
source(paste0(model_dir, "1_std.R"), local = sim)  # access functions using sim$fun()

dat <- sim$run(params_std)
plot$sim_plots(dat, params_std)

## ----run-std-LR---------------------------------------------------------------
params <- modifyList(params_std, list(LR_group = 0.2))
dat <- sim$run(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(LR_group = 0.8))
dat <- sim$run(params)
plot$sim_plots(dat, params)

## ----run-std-inv-temp---------------------------------------------------------
params <- modifyList(params_std, list(inv_temp_group = 0))
dat <- sim$run(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(inv_temp_group = 1.5))
dat <- sim$run(params)
plot$sim_plots(dat, params)

## ----params-discr-approx------------------------------------------------------
params_LRN_discr <- list(
  n_part = 50,
  n_trials = 30,
  LR_conf_group = 0.8,
  LR_disconf_group = 0.2,
  inv_temp_group = 0.5,
  initQF_group = 8,
  initQU_group = 2,
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2,
  margin_group = 2
)

## ----run-LRN-discr-approx-stat------------------------------------------------
sim <- new.env()
source(paste0(model_dir, "2_LRN_discr_approx_stat.R"), local = sim)

dat <- sim$run(params_LRN_discr)
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-approx-dyn-------------------------------------------------
sim <- new.env()
source(paste0(model_dir, "3_LRN_discr_approx_dyn.R"), local = sim)

dat <- sim$run(params_LRN_discr)
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-geq-stat---------------------------------------------------
sim <- new.env()
source(paste0(model_dir, "4_LRN_discr_geq_stat.R"), local = sim)

dat <- sim$run(params_LRN_discr)
plot$sim_plots(dat, params_LRN_discr)

## ----run-LRN-discr-geq-dyn----------------------------------------------------
sim <- new.env()
source(paste0(model_dir, "5_LRN_discr_geq_dyn.R"), local = sim)

dat <- sim$run(params_LRN_discr)
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
sim <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim)

params_LRN_cont <- list(
  n_part = 50,
  n_trials = 30,
  w_LR_group = 0.8,
  inv_temp_group = 0.5,
  initQ_group = list(F = 8, U = 2),
  mu_R_group = list(F = 5, U = 5),
  sigma_R_group = 2
)

dat <- sim$run_LRN_cont(params_LRN_cont, "stat")
plot$sim_plots(dat, params_LRN_cont)

## ----run-LRN-cont-dyn---------------------------------------------------------
dat <- sim$run_LRN_cont(params_LRN_cont, "dyn")
plot$sim_plots(dat, params_LRN_cont)

