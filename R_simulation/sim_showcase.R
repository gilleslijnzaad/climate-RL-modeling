## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----sim-code, comment = NA---------------------------------------------------
rm(list = ls())
sim <- new.env()
source("sim.R", local = sim)  # access functions using sim$fun()
temp <- readLines("sim.R")
start <- grep("# === run_std", temp)
end <- grep("# === end of run_std", temp) - 1
cat(temp[start:end], sep = "\n")

## ----run-std------------------------------------------------------------------
params_std <- list(
  n_part = 50,
  n_trials = 30,
  LR = 0.4,
  inv_temp = 0.5,
  initQF = 5,
  initQU = 5,
  mu_R = c(8, 2), # F and U
  sigma_R = 2
)

dat <- sim$run_std(params_std)

plot <- new.env()
source("../plot_utils.R", local = plot)  # access functions using plot$fun()

gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params_std)

## ----run-std-sigma-init-------------------------------------------------------
params <- modifyList(params_std, list(sigma_R = 3))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

params <- modifyList(params_std, list(initQF = 2, initQU = 8))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

## ----run-std-LR---------------------------------------------------------------
params <- modifyList(params_std, list(LR = 0.2))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

params <- modifyList(params_std, list(LR = 0.8))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

## ----run-std-inv-temp---------------------------------------------------------
params <- modifyList(params_std, list(inv_temp = 0))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

params <- modifyList(params_std, list(inv_temp = 1.5))
dat <- sim$run_std(params)
gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params)

## -----------------------------------------------------------------------------
params_LRN_discr <- list(
  n_part = 50,
  n_trials = 30,
  LRs = list(conf = 0.5, disconf = 0.2),
  inv_temp = 0.5,
  initQF = 8,
  initQU = 2,
  mu_R = c(5, 5), # F and U
  sigma_R = 2,
  margin = 2
)

dat <- sim$run_LRN(params_LRN_discr, sim$LR_approx)

gridExtra::grid.arrange(plot$Q(dat), plot$choice(dat), nrow = 1)
plot$param_annotation(params_LRN_discr)

