## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----run-std------------------------------------------------------------------
rm(list = ls())
setwd("~/research/climate-RL-mod/1_simulation_showcase")
main_dir <- "~/research/climate-RL-mod/"
model_dir <- paste0(main_dir, "0_models/")
util_dir <- paste0(main_dir, "9_utilities/")

plot <- new.env()
source(paste0(util_dir,"plot_utils.R"), local = plot)  # access functions using plot$fun()

params_std <- list(
  n_part = 50,
  n_trials = 30,
  LR_group = 0.4,
  inv_temp_group = 0.5,
  initQ_dev_group = 2,
  mu_R_group = 5.5,
  sigma_R_group = 2
)

sim <- new.env()
source(paste0(model_dir, "1_std.R"), local = sim)  # access functions using sim$fun()

dat <- sim$run(params_std)
plot$sim_plots(dat, params_std)

## ----run-std-LR---------------------------------------------------------------
params <- modifyList(params_std, list(LR_group = 0.1))
dat <- sim$run(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(LR_group = 0.8))
dat <- sim$run(params)
plot$sim_plots(dat, params)

## ----run-std-inv-temp---------------------------------------------------------
params <- modifyList(params_std, list(inv_temp_group = 0))
dat <- sim$run(params)
plot$sim_plots(dat, params)

params <- modifyList(params_std, list(inv_temp_group = 2))
dat <- sim$run(params)
plot$sim_plots(dat, params)

## ----gauss, echo = FALSE------------------------------------------------------
gauss <- function(R, sigma = 2) {
  LR_max <- 1
  belief <- 6
  distance <- abs(belief - R)
  LR <- LR_max * exp(-distance^2/(2*sigma^2))
  return(LR)
}

ggplot() + 
  geom_function(fun = gauss) +
  geom_vline(aes(xintercept = 6, linetype = "belief")) +
  scale_linetype_manual(values = c("belief" = 2), name = NULL) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10), limits = c(1, 10)) +
  scale_y_continuous(breaks = c(0, 1), labels = c(0, "LR_max")) +
  labs(x = "Rating", y = "LR[t]") +
  plot$my_theme_classic

## ----run-LRN-gauss------------------------------------------------------------
my_order <- c("n_part", "n_trials", "LR_max_group", "sigma_LR_group", "inv_temp_group", "initQ_dev_group", "mu_R_group", "sigma_R_group")
params <- modifyList(params_std[-3], list(LR_max_group = 0.6,
                                          sigma_LR_group = 2))[my_order]

sim <- new.env()
source(paste0(model_dir, "2_LRN_gauss.R"), local = sim)  # access functions using sim$fun()

dat <- sim$run(params)
plot$sim_plots(dat, params)

