## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----create-data--------------------------------------------------------------
rm(list = ls())
sim_dir <- "../../R_simulation/"
source(paste0(sim_dir, "sim.R"))

params <- list(
  n_part = 1,
  n_trials = 100,
  LR = 0.4,
  inv_temp = 1,
  initQF = 5,
  initQU = 5,
  mu_R = c(8, 3), # F and U
  sigma_R = 3
)

sim_dat <- run_sim(params, save_to_JSON = TRUE)

dplyr::glimpse(sim_dat)

## ----inspecting-data, echo = FALSE--------------------------------------------
library(grid)
library(gridExtra)
grid.arrange(plot_Q(sim_dat), plot_choice(sim_dat), nrow = 1,
                        top = textGrob("Simulated data", gp = gpar(fontsize = 20, font = 2)))
my_annotation(params)

## ----defmod-data--------------------------------------------------------------
mod <- ""
mod <- paste0(mod, 
"data {
  int<lower=1> n_part;
  int<lower=1> n_trials;
  array[n_part, n_trials] int<lower=1, upper=2> choice;
  array[n_part, n_trials] int<lower=0, upper=10> R;
  int<lower=0, upper=10> initQF;
  int<lower=0, upper=10> initQU;
}
")

## ----defmod-params------------------------------------------------------------
mod <- paste0(mod, "
parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=5> inv_temp;
}
")

## ----defmod-transf-params-----------------------------------------------------
mod <- paste0(mod, "
// transformed parameters {
// }
")

## ----defmod-model-priors------------------------------------------------------
mod <- paste0(mod, "
model {
  // this is where priors would go. leaving them empty leads to uninformative priors
")

## ----defmod-model-inits-------------------------------------------------------
mod <- paste0(mod, "
  for (j in 1:n_part) {
    array[n_trials, 2] real Q;
    Q[1, 1] = initQF;
    Q[1, 2] = initQU;
    vector[2] Q_t;

    real pred_err;
")

## ----defmod-data-trials-------------------------------------------------------
mod <- paste0(mod, "
    for (t in 1:n_trials) {
      Q_t = to_vector(Q[t]);

      // sample choice (1 is F, 2 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp * Q_t);

      // prediction error
      if (choice[j, t] == 1) {
        pred_err = R[j, t] - Q[t, 1];
      } else {
        pred_err = R[j, t] - Q[t, 2];
      }

      // update value (learn)
      if (t < n_trials) {    // no updating in the very last trial
        if (choice[j, t] == 1) {
          Q[t+1, 1] = Q[t, 1] + LR * pred_err;
          Q[t+1, 2] = Q[t, 2];
        } else {
          Q[t+1, 1] = Q[t, 1];
          Q[t+1, 2] = Q[t, 2] + LR * pred_err;
        }
      }
    }
  }
}
")

## ----save-stan----------------------------------------------------------------
write(mod, file = "climate-RL.stan")

## ----run-model----------------------------------------------------------------
library(cmdstanr)
options(mc.cores = parallel::detectCores())

# function allows for easily changing whether you want to refit or use a saved fit
it <- 10000
fit_model <- function(refit) {
  if (refit) {
    m <- cmdstan_model("climate-RL.stan")
    data_file <- paste0(sim_dir, "sim_dat.json")
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
  return(fit)
}

fit <- fit_model(refit = TRUE)

## ----inspect-results----------------------------------------------------------
source("../../plot_utils.R")

posterior_density_plot(fit, c("LR", "inv_temp"), params)
posterior_dot_error_plot(fit, c("LR", "inv_temp"), params)

