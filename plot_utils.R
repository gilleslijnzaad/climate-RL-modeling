util <- new.env()

library(tidyverse)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_dark_blue <- "#001199"
my_blue <- "#00ccff"
my_colors <- c(my_teal, my_pink, my_dark_blue, my_blue)
my_param_colors <- setNames(my_colors, c("F", "U", "inv_temp", "LR"))

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
# returns: density plot of posterior distribution(s) with simulated value as dashed line
posterior_density_plot <- function(fit, to_plot, param_settings) {
  plot_data <- data.frame()

  for (param in to_plot) {
    sim_value <- param_settings[[param]]
    dat <- data.frame(
      parameter = rep(param, length(fit$draws(param))),
      estimate = array(fit$draws(param)),
      sim_value = rep(sim_value, length(fit$draws(param)))
    )
    plot_data <- rbind(plot_data, dat)
  }

  plot <- ggplot(plot_data, aes(x = estimate, color = parameter, fill = parameter)) +
    geom_density(alpha = 0.6) +
    labs(title = "Posterior distribution", x = "Estimate", y = "Density") +
    facet_grid(. ~ parameter, scales = "free") +
    scale_color_manual(values = my_param_colors) +
    scale_fill_manual(values = my_param_colors) +
    guides(linetype = "legend", color = "none", fill = "none") +
    geom_vline(aes(xintercept = sim_value, color = parameter, linetype = "sim_value")) +
     scale_linetype_manual(values = c("sim_value" = 2), name = NULL) +
    my_theme

  return(plot)
}

while ("util" %in% search())
  detach("util")

attach(util)