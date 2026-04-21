# -----------------------------
#        SET DIRECTORIES
# -----------------------------
main_dir <- "~/research/climate-RL-mod/"
util_dir <- paste0(main_dir, "9_utilities/")
sim_utils <- new.env()
source(paste0(util_dir, "sim_utils.R"), local = sim_utils)

# ---------------------
#        GENERAL
# ---------------------
library(tidyverse)
my_teal <- "#008080"
my_pink <- "#dd66dd"
my_blue <- "#0055bb"
my_light_blue <- "#00aadd"
my_dark_blue <- "#001199"
my_red <- "#ff4455"
my_param_colors <- c(
  F = my_teal,                  U = my_pink,
  initQF = my_teal,             initQU = my_pink,
  initQF_group = my_teal,       initQU_group = my_pink,
  LR = my_blue,                 inv_temp = my_red,
  LR_group = my_blue,           inv_temp_group = my_red,
  LR_conf = my_dark_blue,       LR_disconf = my_light_blue,
  LR_conf_group = my_dark_blue, LR_disconf_group = my_light_blue
)

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

# function from: https://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot
get_legend <- function(myggplot){
  library(gridExtra)
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# --------------------------------------
#        PLOTS FOR SIMULATED DATA
# --------------------------------------

#' Creates a plot of Q-values over time (smooth)
#' 
#' @param   sim_dat: data frame of simulated data
#' 
#' @return  ggplot2 object
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

#' Creates a plot of choices over time (smooth)
#' 
#' @param   sim_dat: data frame of simulated data
#' 
#' @return  ggplot2 object
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

#' Creates an annotation listing the parameter settings
#' 
#' @param   params named list of parameter settings
#' 
#' @return `textGrob` detailing the parameter settings
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

#' Combines `Q()` and `choice()` plots with the annotation
#' 
#' @param sim_dat     data frame of simulated data
#' 
#' @param params      named list of parameter settings
#' 
#' @param plot_title  string for plot title; if `NA`, plot will not
#' have a title
#' 
#' @return nothing
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

#' Plots posterior density distributions for all given free
#' parameters
#' 
#' @param draws: data frame of posterior draws from model
#' 
#' @param to_plot: string array of parameters to plot
#' 
#' @param param_settings: named list of parameter settings; if NULL,
#' don't show simulated value
#' 
#' @return nothing
posterior_density <- function(draws, to_plot, param_settings = NULL) {
  plots <- list()
  legend <- NULL

  for (p in to_plot) {
    dat <- data.frame(
      estimate = draws[[p]]
    )
    if (!is.null(param_settings)) {
      dat$sim_value <- param_settings[[p]]
    }

    plot <- ggplot(dat, aes(x = estimate)) +
      geom_density(alpha = 0.6, color = my_param_colors[[p]], fill = my_param_colors[[p]]) +
      labs(title = "Posterior distributions", x = "Estimate", y = "Density") +
      labs(title = p, x = NULL, y = NULL) +
      my_theme + 
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

    if (!is.null(param_settings)) {
      plot <- plot + 
        geom_vline(aes(xintercept = sim_value, linetype = "sim_value")) +
        scale_linetype_manual(values = c("sim_value" = 2), name = NULL)
      legend <- get_legend(plot)
    }
    plots[[p]] <- plot + theme(legend.position = "none")
  }

  library(grid)
  plot_grid <- gridExtra::arrangeGrob(grobs = plots,
                                      ncol = 2,
                                      top = textGrob("Posterior distributions\n", x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 22, font = 2)),
                                      bottom = textGrob("Estimate", gp = gpar(fontsize = 18)),
                                      left = textGrob("Density", rot = 90, gp = gpar(fontsize = 18))
                                     )

  gridExtra::grid.arrange(grobs = list(plot_grid, legend),
                          ncol = 2,
                          widths = unit.c(unit(1, "null"), # fill space evenly
                                          grobWidth(legend) + unit(2, "mm")))
}

#' Plots participant-level simulated parameters against their
#' estimated value, with a dashed line indicating perfect agreement
#'
#' @param draws data frame of posterior draws from model
#' 
#' @param to_plot string array of parameters to plot
#' 
#' @param param_settings named list of parameter settings
#' 
#' @return nothing
pp_level_param_fit <- function(draws, to_plot, param_settings) {
  n_part <- param_settings$n_part
  plots <- list()
  for (p in to_plot) {
    median_draws <- c()
    sim_values <- c()
    for (j in 1:n_part) {
      sim_values <- c(sim_values, param_settings[[p]][j])
      param_name <- paste0(p, "[", j, "]")
      median_draws <- c(median_draws, median(draws[[param_name]]))
    }
    dat <- data.frame(sim_value = sim_values, 
                      fit_value = median_draws)
    
    bounds <- range(sim_values)

    plot <- ggplot(dat, aes(x = sim_value, y = fit_value)) +
      geom_point(color = my_param_colors[[p]], size = 2) +
      lims(y = bounds) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      labs(title = p, x = NULL, y = NULL) +
      my_theme + 
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    plots[[p]] <- plot
  }

  library(grid)
  gridExtra::grid.arrange(grobs = plots,
                          ncol = 2,
                          top = textGrob("Participant-level parameter estimations\n", x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 22, font = 2)),
                          bottom = textGrob("Simulated value", gp = gpar(fontsize = 18)),
                          left = textGrob("Fitted value", rot = 90, gp = gpar(fontsize = 18))
                         )
}

#' Plots simulated values against estimated values for all given free
#' parameters, with a dashed line indicating perfect agreement
#' 
#' @param sim_params data frame containing the simulated value used
#' for each run for each free parameter
#' 
#' @param fit_params data frame containing the estimated value (median
#' of the posterior draws) used for each run for each free parameter
#' 
#' @param to_plot string array of parameters to plot
#' 
#' @return nothing
many_runs_param_fit <- function(sim_params, fit_params, to_plot) {
  plots <- list()
  for (p in to_plot) {
    bounds <- sim_utils$param_bounds[[p]]

    plot_dat <- data.frame(x = sim_params[[p]], y = fit_params[[p]])

    plot <- ggplot(plot_dat, aes(x = x, y = y)) +
      geom_point(color = my_param_colors[[p]], size = 2) +
      geom_abline(intercept = 0, slope = 1, linetype = 2) +
      lims(x = bounds, y = bounds) +
      labs(title = p, x = NULL, y = NULL) +
      my_theme + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
    plots[[p]] <- plot
  }

  title <- paste0("Parameter recovery for ", max(sim_params$k), " simulations\n")

  library(grid)
  gridExtra::grid.arrange(grobs = plots,
                          ncol = 2,
                          top = textGrob(title, x = unit(0, "npc"), just = "left", gp = gpar(fontsize = 22, font = 2)),
                          bottom = textGrob("Simulated value", gp = gpar(fontsize = 18)),
                          left = textGrob("Fitted value", rot = 90, gp = gpar(fontsize = 18))
                         )
}