# ---------------------
#        GENERAL
# ---------------------
library(tidyverse)
my_teal <- "#008080"
my_pink <- "#ff00dd"
my_blue <- "#00aadd"
my_dark_blue <- "#001199"
my_colors <- c(my_teal, my_pink, my_blue, my_dark_blue, my_teal, my_pink)
my_param_colors <- setNames(my_colors, c("F", "U", "LR_group", "inv_temp_group", "initQ_group$F", "initQ_group$U"))

my_theme <- theme_bw() +
  theme(plot.title = element_text(size = 22, face = "bold")) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 18)) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  theme(strip.text = element_text(size = 18, face = "bold"))

my_theme_classic <- theme_classic() +
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
# arguments: 
# - sim_dat: data frame of simulated data
# 
# returns: 
# - smooth plot of Q values over time (ggplot)
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
# arguments: 
# - sim_dat: data frame of simulated data
# 
# returns: 
# - smooth plot of choices over time (ggplot)
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
# arguments: 
# - named list of parameter settings
# 
# returns: 
# - textGrob of parameter settings list
param_annotation <- function(params) {
  library(grid)
  full_text <- c()
  for (p in names(params)) {
    if (length(params[[p]]) == 1) {
      add_text <- paste0(p, " = ", params[[p]])
      full_text <- paste(full_text, add_text, sep = "\n")
    } else {
      for (i in 1:length(params[[p]])) {
        param_name <- paste0(p, "$", names(params[[p]][i]))
        add_text <- paste0(param_name, " = ", params[[p]][i])
        full_text <- paste(full_text, add_text, sep = "\n")
      }
    }
  }
  g <- textGrob(label = full_text, x = unit(1, "npc"), y = unit(0.98, "npc"), just = c("right", "top"))
  return(g)
}

# === sim_plots() ======================
# arguments: 
# - sim_dat: simulated data; 
# - params: list of parameter settings;
# - plot_title: string providing the title of the plot, or NA for no title
# 
# returns: nothing
sim_plots <- function(sim_dat, params, plot_title = NA) {
  annotation <- param_annotation(params)
  if (is.na(plot_title)) {
    title <- NA
  } else {
    title <- textGrob(plot_title, gp = gpar(fontsize = 20, font = 2))
  }
  
  gridExtra::grid.arrange(Q(sim_dat), 
                          choice(sim_dat), 
                          annotation,
                          ncol = 3,
                          widths = unit.c(unit(1, "null"), # fill space evenly
                                          unit(1, "null"),
                                          grobWidth(annotation) + unit(2, "mm")),
                          top = title
                         )
}

# --------------------------------
#        PLOTS FOR MODELING
# --------------------------------

# === posterior_density() =============================
# arguments:
# - draws: data frame of posterior draws from model
# - to_plot: string array of parameters to plot
# - param_settings: named list of parameter settings; if NA, don't show simulated value
# 
# returns: 
# - density plot(s) of posterior distribution(s) with simulated value as dashed line. plots are organized using facet_grid and are color-coded
posterior_density <- function(draws, to_plot, param_settings = NULL) {
  plot_data <- data.frame()

  for (p in to_plot) {
    if (!is.null(param_settings)) {
      if (grepl("\\$", p)) { # parameter is part of a list
        split_name <- strsplit(p, "\\$")[[1]]
        sim_value <- purrr::pluck(param_settings, split_name[1], split_name[2])
      } else {
        sim_value <- param_settings[[p]]
      }
    }
    dat <- data.frame(
      parameter = as.factor(p),
      estimate = draws[[p]]
    )
    if (!is.null(param_settings)) {
      dat$sim_value <- sim_value
    }
    plot_data <- rbind(plot_data, dat)
  }

  plot <- ggplot(plot_data, aes(x = estimate, color = parameter, fill = parameter)) +
    geom_density(alpha = 0.6) +
    labs(title = "Posterior distributions", x = "Estimate", y = "Density") +
    facet_wrap(. ~ factor(parameter, to_plot), scales = "free") +
    scale_color_manual(values = my_param_colors) +
    scale_fill_manual(values = my_param_colors) +
    guides(linetype = "legend", color = "none", fill = "none") +
    my_theme

  if (!is.null(param_settings)) {
    plot <- plot + 
    geom_vline(aes(xintercept = sim_value, color = parameter, linetype = "sim_value")) +
    scale_linetype_manual(values = c("sim_value" = 2), name = NULL)
  }

  return(plot)
}
