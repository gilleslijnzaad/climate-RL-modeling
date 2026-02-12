## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----create-data--------------------------------------------------------------
rm(list = ls())
main_dir <- "~/research/climate-RL/"
sim_dir <- paste0(main_dir, "R_simulation/")
sim <- new.env()
source(paste0(sim_dir, "sim.R"), local = sim)  # access functions using sim$fun()

params <- list(
  n_part = 20,
  n_trials = 200,
  LR = 0.5,
  inv_temp = 1.2,
  initQF = 5,
  initQU = 5,
  mu_R = c(8, 2), # F and U
  sigma_R = 2
)

sim_dat <- sim$run_sim(params, save_to_JSON = FALSE)

cat(paste0("PARAMETER SETTINGS:"), capture.output(dplyr::glimpse(params)), sep = "\n")
cat(paste0("SIMULATED DATA:"), capture.output(dplyr::glimpse(sim_dat)), sep = "\n")

## ----inspecting-data, warning = FALSE-----------------------------------------
library(grid)
library(gridExtra)

plot <- new.env()
source(paste0(main_dir, "plot_utils.R"), local = plot)  # access functions using plot$fun()

grid.arrange(plot$Q(sim_dat), plot$choice(sim_dat), nrow = 1,
             top = textGrob("Simulated data", gp = gpar(fontsize = 20, font = 2)))
plot$param_annotation(params, extra_vertical_spacing = TRUE) 

## ----explmod-data, comment = NA-----------------------------------------------
mod_code <- readLines("climate-RL.stan")
start <- grep("data \\{", mod_code)[1]
end <- grep("transformed data \\{", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-transf-data, eval = FALSE, comment = NA--------------------------
# start <- grep("transformed data \\{", mod_code)
# end <- grep("parameters \\{", mod_code)[1] - 2
# cat(mod_code[start:end], sep = "\n")

## ----explmod-params, comment = NA---------------------------------------------
start <- grep("parameters \\{", mod_code)[1]
end <- grep("transformed parameters \\{", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-transf-params, eval = FALSE, comment = NA------------------------
# start <- grep("transformed parameters \\{", mod_code)
# end <- grep("model \\{", mod_code) - 2
# cat(mod_code[start:end], sep = "\n")

## ----explmod-model-priors, comment = NA---------------------------------------
start <- grep("model \\{", mod_code)
end <- grep("participant loop", mod_code)[1] - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-model-part, comment = NA-----------------------------------------
start <- grep("participant loop", mod_code) + 1
end <- grep("trial loop", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-model-trial, comment = NA----------------------------------------
start <- grep("trial loop", mod_code) + 1
end <- grep("generated quantities", mod_code) - 2
cat(mod_code[start:end], sep = "\n")

## ----explmod-gen-quant, eval = FALSE, comment = NA----------------------------
# start <- grep("generated quantities", mod_code)
# end <- length(mod_code)
# cat(mod_code[start:end], sep = "\n")

## ----run-model----------------------------------------------------------------
data_file <- paste0(sim_dir, "sim_dat.json")
refit <- sim$did_sim_dat_change(data_file, sim_dat)
sim$save_sim_dat(params, sim_dat)

library(cmdstanr)
options(mc.cores = parallel::detectCores())
if (refit) {
  m <- cmdstan_model("climate-RL.stan")
  it <- 1000
  fit <- m$sample(
    data = data_file,
    iter_sampling = it,
    chains = 1,
    thin = 1,
    iter_warmup = it / 2,
    refresh = it / 5,
    seed = 1234
  )
  fit$save_object(file = "climate-RL_fit.rds")
} else {
  fit <- readRDS(file = "climate-RL_fit.rds")
}

## ----inspect-results----------------------------------------------------------
plot$posterior_density(fit, c("LR", "inv_temp"), params)

