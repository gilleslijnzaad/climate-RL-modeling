util <- new.env()

library(tidyverse)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_blue <- "#00ccff"
my_colors <- c(my_teal, my_pink, my_blue)

my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18, face = "bold"))

# === posterior_density_plot =============================
# arguments:
#   - fit: model fit obtained by cmdstanr::model$sample()
#   - to_plot: character array of parameters to plot
#   - param_settings: named list of parameter settings (e.g., list(LR = 0.5, inv_temp = 0.5))
#   - include_sim_value: default TRUE
# returns: density plot of posterior distribution(s), optionally with simulated value as dashed line
posterior_density_plot <- function(fit, to_plot, param_settings, include_sim_value = TRUE) {
  plot_data <- data.frame()

  for (param in to_plot) {
    dat <- data.frame(
      parameter = rep(param, length(fit$draws(param))),
      estimate = array(fit$draws(param))
    )
    if (include_sim_value) {
      sim_value <- param_settings[[param]]
      dat$sim_value <- rep(sim_value, length(fit$draws(param)))
    }
    plot_data <- rbind(plot_data, dat)
  }

  plot <- ggplot(plot_data, aes(x = estimate, color = parameter, fill = parameter)) +
    geom_density(alpha = 0.6) +
    labs(title = "Posterior distribution", x = "Estimate", y = "Density", color = "Parameter", fill = "Parameter") +
    facet_grid(. ~ parameter, scales = "free") +
    scale_color_manual(values = my_colors) +
    scale_fill_manual(values = my_colors) +
    my_theme
  
  if (include_sim_value) {
    plot <- plot +
      geom_vline(aes(xintercept = sim_value, color = parameter, linetype = "sim_value")) +
      scale_linetype_manual(values = c("sim_value" = 2), name = NULL)
  }

  return(plot)
}

# === posterior_dot_error_plot =============================
# arguments:
#   - fit: model fit obtained by cmdstanr::model$sample()
#   - to_plot: character array of parameters to plot
#   - param_settings: named list of parameter settings (e.g., list(LR = 0.5, inv_temp = 0.5))
#   - include_sim_value: default TRUE
# returns: dot plot with error bars (median + cred. int.), optionally with simulated value as dashed line
posterior_dot_error_plot <- function(fit, to_plot, param_settings, include_sim_value = TRUE) {
  plot_data <- data.frame()

  cred_int <- function(posterior_dist) {
    return(as.numeric(quantile(posterior_dist, c(0.025, 0.975))))
  }

  for (param in to_plot) {
    dat <- data.frame(
      parameter = param,
      median = median(array(fit$draws(param))),
      cred_int_min = cred_int(array(fit$draws(param)))[1],
      cred_int_max = cred_int(array(fit$draws(param)))[2],
      sim_value = param_settings[[param]]
    )
    plot_data <- rbind(plot_data, dat)
  }

  plot <- ggplot(plot_data, aes(x = parameter, y = median, color = parameter)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = cred_int_min, ymax = cred_int_max), width = 0.25) +
    labs(title = "Posterior median ± 95% credibility interval", x = element_blank(), y = "Estimate") +
    scale_color_manual(values = my_colors) +
    guides(color = "none") +
    my_theme

  if (include_sim_value) {
    plot <- plot +
      geom_hline(aes(yintercept = sim_value, color = parameter, linetype = "sim_value")) +
      scale_linetype_manual(values = c("sim_value" = 2), name = NULL)
  }
  
  return(plot)
}

while ("util" %in% search())
  detach("util")

attach(util)