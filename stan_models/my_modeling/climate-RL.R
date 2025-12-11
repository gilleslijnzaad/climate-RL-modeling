## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----loading-data, message=FALSE----------------------------------------------
sim_dir <- "~/research/climate-RL/R_simulation/"
library(rjson)
param_settings <- fromJSON(file = paste0(sim_dir, "sim_param_settings.json"))
data <- fromJSON(file = paste0(sim_dir, "sim_dat.json"))
library(dplyr)
glimpse(param_settings)
glimpse(data)

## ----defmod-data--------------------------------------------------------------
mod <- ""
mod <- paste0(mod, 
"data {
  int<lower=1> n_part;
  int<lower=1> T;
  array[n_part, T] int<lower=1, upper=2> choice;
  array[n_part, T] int<lower=0, upper=10> R;
  int<lower=0, upper=10> initQF;
  int<lower=0, upper=10> initQU;
}
")

## ----defmod-params------------------------------------------------------------
mod <- paste0(mod, "
parameters {
  real<lower=0, upper=1> LR_raw;
  real<lower=0, upper=5> inv_temp_raw;
}
")

## ----defmod-transf-params-----------------------------------------------------
mod <- paste0(mod, "
transformed parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=10> inv_temp;

  LR = inv_logit(LR_raw);
  inv_temp = inv_logit(inv_temp_raw) * 10.0;
}
")

## ----defmod-model-priors------------------------------------------------------
mod <- paste0(mod, "
model {
  // priors: all uninformative
  LR_raw ~ normal(0, 1);
  inv_temp_raw ~ normal(0, 1);
")

## ----defmod-model-inits-------------------------------------------------------
mod <- paste0(mod, "
  for (j in 1:n_part) {
    array[T, 2] real Q;
    Q[1, 1] = initQF;
    Q[1, 2] = initQU;
    vector[2] Q_t;

    real pred_err;
")

## ----defmod-data-trials-------------------------------------------------------
mod <- paste0(mod, "
    for (t in 1:T) {
      Q_t = to_vector(Q[t]);

      // sample choice (0 is F, 1 is U) via softmax
      choice[j, t] ~ categorical_logit(inv_temp * Q_t);

      // prediction error
      if (choice[j, t] == 0) {
        pred_err = R[j, t] - Q[t, 1];
      } else {
        pred_err = R[j, t] - Q[t, 2];
      }

      // update value (learn)
      if (t < T) {    // no updating in the very last trial
        if (choice[j, t] == 0) {
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
mod_dir <- "~/research/climate-RL/stan_models/my_modeling/"
write(mod, file = paste0(mod_dir, "climate-RL.stan"))

## ----run-model----------------------------------------------------------------
library(cmdstanr)

# function allows for easily changing whether you want to refit or use a saved fit
fit_model <- function(refit) {
  if (refit) {
    m <- cmdstan_model(paste0(mod_dir, "climate-RL.stan"))
    data_file <- paste0(sim_dir, "sim_dat.json")
    it <- 10000
    fit <- m$sample(
      data = data_file,
      iter_sampling = it,
      chains = 1,
      thin = 1,
      # init = my_inits,
      iter_warmup = it / 2,
      refresh = it / 5,
      seed = 1234
    )
    fit$save_object(file = paste0(mod_dir, "climate-RL_fit.rds"))
  } else {
    fit <- readRDS(file = paste0(mod_dir, "climate-RL_fit.rds"))
  }
  return(fit)
}

fit <- fit_model(refit = FALSE)

## ----inspect-model-2----------------------------------------------------------
library(ggplot2)
update_geom_defaults("density", list(linewidth = 1.5))
update_geom_defaults("vline", list(linewidth = 1.5))
my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))

LR_posterior <- array(fit$draws("LR"))
LR_plot <- ggplot() +
  geom_density(aes(x = LR_posterior)) +
  labs(title = "Learning rate", x = "Estimate", y = "Density") +
  geom_vline(aes(xintercept = param_settings$LR, linetype = "simulated value"), linetype = 2, color = "blue") +
  # xlim(c(0.49, 0.51)) +
  my_theme
inv_temp <- array(fit$draws("inv_temp"))
inv_temp_plot <- ggplot() +
  geom_density(aes(x = inv_temp)) +
  labs(title = "Inverse temperature", x = "Estimate", y = "Density") +
  geom_vline(aes(xintercept = param_settings$inv_temp, linetype = "simulated value"), linetype = 2, color = "blue") +
  my_theme

library(gridExtra)
grid.arrange(LR_plot, inv_temp_plot, nrow = 1)

