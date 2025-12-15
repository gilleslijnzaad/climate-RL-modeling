## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----loading-data, message=FALSE----------------------------------------------
rm(list = ls())
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
it <- 10000
fit_model <- function(refit) {
  if (refit) {
    m <- cmdstan_model(paste0(mod_dir, "climate-RL.stan"))
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
    fit$save_object(file = paste0(mod_dir, "climate-RL_fit.rds"))
  } else {
    fit <- readRDS(file = paste0(mod_dir, "climate-RL_fit.rds"))
  }
  return(fit)
}

fit <- fit_model(refit = FALSE)

## -----------------------------------------------------------------------------
library(ggplot2)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_blue <- "#00ccff"
my_colors <- c(my_teal, my_pink, my_blue)
my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))

dens_plot <- function(fit, pars, include_sim_value = FALSE) {
  plot_data <- data.frame()

  for (p in pars) {
    dat <- data.frame(
      parameter = rep(p, it),
      estimate = array(fit$draws(p)),
      sim_value = rep(param_settings[[p]], it)
    )
    plot_data <- rbind(plot_data, dat)
  }

  plot <- ggplot(plot_data, aes(x = estimate, color = parameter, fill = parameter)) +
    geom_density(alpha = 0.6) +
    geom_vline(aes(xintercept = sim_value, color = parameter, linetype = "simulated value")) +
    labs(title = "Posterior distribution", x = "Estimate", y = "Density", color = "Parameter", fill = "Parameter") +
    scale_color_manual(values = my_colors) +  
    scale_fill_manual(values = my_colors) +
    scale_linetype_manual(values = c("simulated value" = 2), name = NULL) +
    my_theme
  
  return(plot)
}

## ----inspect-model------------------------------------------------------------
dens_plot(fit, c("LR"))

