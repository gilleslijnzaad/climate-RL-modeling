## ----setup, include = FALSE---------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 4)

## ----loading-data, message=FALSE----------------------------------------------
library(rjson)
data_file <- "~/research/climate-RL/R_simulation/sim_dat.json"

data <- fromJSON(file = data_file)
library(dplyr)
glimpse(data)

## ----defmod-data--------------------------------------------------------------
mod <- ""
mod <- paste0(mod, 
"data {
  int<lower=1> T;
  array[T] int<lower=1, upper=2> choice;
  array[T] int<lower=0, upper=10> R;
}
")

## ----defmod-params------------------------------------------------------------
mod <- paste0(mod, "
parameters {
  real<lower=0, upper=1> LR_raw;
  real<lower=0, upper=5> inv_temp_raw;
  real<lower=0, upper=10> initQF_raw;
  real<lower=0, upper=10> initQU_raw;
  real<lower=0, upper=10> mu_R_raw;
  real<lower=0, upper=10> sigma_R_raw;
}
")

## ----defmod-transf-params-----------------------------------------------------
mod <- paste0(mod, "
transformed parameters {
  real<lower=0, upper=1> LR;
  real<lower=0, upper=10> inv_temp;
  real<lower=0, upper=10> initQF;
  real<lower=0, upper=10> initQU;
  real<lower=0, upper=10> mu_R;
  real<lower=0, upper=10> sigma_R;

  LR = inv_logit(LR_raw);
  inv_temp = inv_logit(inv_temp_raw) * 10.0;
  initQF = inv_logit(initQF_raw) * 10.0;
  initQU = inv_logit(initQU_raw) * 10.0;
  mu_R = inv_logit(mu_R_raw) * 10.0; 
  sigma_R = inv_logit(sigma_R_raw) * 10.0;
}
")

## ----defmod-model-priors------------------------------------------------------
mod <- paste0(mod, "
model {
  // priors: all uninformative
  LR_raw ~ normal(0, 1);
  inv_temp_raw ~ normal(0, 1);
  initQF_raw ~ normal(0, 1);   
  initQU_raw ~ normal(0, 1);   
  mu_R_raw ~ normal(0, 1);          
  sigma_R_raw ~ normal(0, 1);
")

## ----defmod-model-inits-------------------------------------------------------
mod <- paste0(mod, "
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
    choice[t] ~ categorical_logit(inv_temp * Q_t);

    // rate
    R[t] ~ normal(mu_R, sigma_R);

    // prediction error
    if (choice[t] == 0) {
      pred_err = R[t] - Q[t, 1];
    } else {
      pred_err = R[t] - Q[t, 2];
    }

    // update value (learn)
    if (t < T) {
      if (choice[t] == 0) {
        Q[t+1, 1] = Q[t, 1] + LR * pred_err;
        Q[t+1, 2] = Q[t, 2];
      } else {
        Q[t+1, 1] = Q[t, 1];
        Q[t+1, 2] = Q[t, 2] + LR * pred_err;
      }
    }
  }
}
")

## ----save-stan----------------------------------------------------------------
write(mod, file = "~/research/climate-RL/stan_models/my_modeling/climate-RL.stan")

## ----run-model----------------------------------------------------------------
library(cmdstanr)

m <- cmdstan_model("climate-RL.stan")

data_file <- "~/research/climate-RL/R_simulation/sim_dat.json"

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

## ----inspect-model------------------------------------------------------------
LR_post <- fit$draws("LR")
print(paste0("sim: ", data$LR, "      fit: ", mean(LR_post)))
inv_temp_post <- fit$draws("inv_temp")
print(paste0("sim: ", data$inv_temp, "      fit: ", mean(inv_temp_post)))

QF_post <- fit$draws("initQF")
print(paste0("sim: ", data$initQF, "      fit: ", mean(QF_post)))
QU_post <- fit$draws("initQU")
print(paste0("sim: ", data$initQU, "      fit: ", mean(QU_post)))

plot_data <- data.frame(
  draws = c(array(fit$draws("initQF")),
            array(fit$draws("initQU")),
            array(fit$draws("mu_R"))),
  parameter = c(rep("initQF", it),
                rep("initQU", it),
                rep("mu_R", it))
)

library(ggplot2)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_blue <- "#11ccff"
update_geom_defaults("density", list(linewidth = 1.5))
update_geom_defaults("vline", list(linewidth = 1.5))
my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))


# TODO: plot mu_R as well, change the values in the sim and see if the initQ values follow the mu_R value
ggplot() +
  geom_density(data = plot_data, aes(x = draws, color = parameter, fill = parameter), alpha = 0.6, linewidth = 1) +
  labs(x = "Estimate", y = "Density") +
  scale_fill_manual(values = c(my_teal, my_pink, my_blue)) +
  scale_color_manual(values = c(my_teal, my_pink, my_blue)) +
  geom_vline(aes(xintercept = data$initQF, linetype = "simulated value"), color = my_teal) +
  geom_vline(aes(xintercept = data$initQU, linetype = "simulated value"), color = my_pink) +
  geom_vline(aes(xintercept = data$mu_R, linetype = "simulated value"), color = my_blue) +
  scale_linetype_manual(values = c("simulated value" = 2), name = NULL) +
  my_theme

