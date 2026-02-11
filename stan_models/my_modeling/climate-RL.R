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
      pred_err = R[j, t] - Q[t, choice[j, t]];

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

