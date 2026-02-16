# ---------------------
#        GENERAL
# ---------------------
library(tidyverse)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_dark_blue <- "#001199"
my_blue <- "#00aadd"
my_colors <- c(my_teal, my_pink, my_dark_blue, my_blue, my_teal, my_pink)
my_param_colors <- setNames(my_colors, c("F", "U", "inv_temp", "LR", "initQF", "initQU"))

my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 18, face = "bold"))


# --------------------------------------
#        PLOTS FOR SIMULATED DATA
# --------------------------------------
# === Q() ========================
# arguments: data frame of simulated data
# returns: ggplot object: smooth plot of Q values over time
Q <- function(sim_dat) {
  # data to long format
  sim_dat <- sim_dat %>%
    pivot_longer(c(Q_F, Q_U), names_prefix = "Q_", names_to = "option", values_to = "Q") %>%
    mutate(option = factor(option),
           choice = factor(choice))

  p <- ggplot(sim_dat, aes(x = trial,
                           y = Q,
                           color = option)) +
    geom_smooth(aes(fill = option)) +
    ylim(c(1, 10)) +
    labs(x = "Trial") +
    scale_color_manual(values = my_param_colors,
                       labels = c("Friendly", "Unfriendly")) +  
    scale_fill_manual(values = my_param_colors,
                      labels = c("Friendly", "Unfriendly")) +
    my_theme +
    theme(legend.position = "inside",
          legend.position.inside = c(0.83, 0.91))

  return(p)
}

# === choice() ========================
# arguments: data frame of simulated data
# returns: ggplot object: smooth plot of choices over time
choice <- function(sim_dat) {
  # data to long format
  sim_dat <- sim_dat %>%
    mutate(choice_is_F = as.numeric(choice == 1),
           choice_is_U = 1 - choice_is_F)
    
  p <- ggplot(sim_dat, aes(x = trial)) +
    geom_smooth(aes(y = choice_is_F),
                color = my_param_colors[["F"]],
                fill = my_param_colors[["F"]]) +
    geom_smooth(aes(y = choice_is_U),
                color = my_param_colors[["U"]],
                fill = my_param_colors[["U"]]) +
    ylim(c(0, 1)) +
    labs(x = "Trial",
        y = "Proportion chosen") +
    my_theme
  return(p)
}

# === param_annotation() ======================
# arguments: vector of parameter settings; whether to include extra vertical spacing
# returns: nothing
param_annotation <- function(params, extra_vertical_spacing = FALSE) {
  library(grid)
  text <- paste0("LR = ", params$LR,
                 "\ninv_temp = ", params$inv_temp,
                 "\ninitQF = ", params$initQF,
                 "\ninitQU = ", params$initQU,
                 "\nmu_R_F = ", params$mu_R[1],
                 "\nmu_R_U = ", params$mu_R[2],
                 "\nsigma_R = ", params$sigma_R
                 )
  y_offset <- if_else(extra_vertical_spacing, 0.87, 0.95)
  grid.text(text, x = unit(0.98, "npc"), y = unit(y_offset, "npc"), hjust = 1, vjust = 1)
}


# --------------------------------
#        PLOTS FOR MODELING
# --------------------------------
# === posterior_density() =============================
# arguments:
#   - fit: model fit obtained by cmdstanr::model$sample()
#   - to_plot: character array of parameters to plot
#   - param_settings: named list of parameter settings (e.g., list(LR = 0.5, inv_temp = 0.5))
# returns: density plot of posterior distribution(s) with simulated value as dashed line
posterior_density <- function(fit, to_plot, param_settings) {
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
    labs(title = "Posterior distributions", x = "Estimate", y = "Density") +
    facet_wrap(. ~ parameter, scales = "free") +
    scale_color_manual(values = my_param_colors) +
    scale_fill_manual(values = my_param_colors) +
    guides(linetype = "legend", color = "none", fill = "none") +
    geom_vline(aes(xintercept = sim_value, color = parameter, linetype = "sim_value")) +
     scale_linetype_manual(values = c("sim_value" = 2), name = NULL) +
    my_theme

  return(plot)
}