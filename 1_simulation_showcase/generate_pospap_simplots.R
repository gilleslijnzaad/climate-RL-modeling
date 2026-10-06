# setwd("1_simulation_showcase")
rm(list = ls())
library(showtext)
font_add("Geologica", "~/Downloads/Geologica-Light.ttf")
showtext_auto()


util <- new.env()
source("app_utils.R", local = util)
util$my_theme <- util$my_theme + 
  theme(axis.title = element_text(family = "Geologica"),
        axis.text = element_text(family = "Geologica"),
        legend.text = element_text(family = "Geologica"),
        plot.caption = element_text(size = 20, family = "Geologica", hjust = 0.5))

gen_plot_dat <- function(sim_dat) {
  plot_dat <- sim_dat %>%
    mutate(choice_F = as.numeric(choice == 1),
            choice_U = as.numeric(choice == 2)) %>%
    pivot_longer(c(choice_F, choice_U), names_prefix = "choice_", names_to = "option", values_to = "choice_prop") %>%
    mutate(option = factor(option)) %>%
    filter(option == "F")
  return(plot_dat)
}

params <- list(
  n_part = 50,
  n_trials = 30,
  sigma_R = 2
)

# FIGURE 3A: INITQ AND LR
# -----------------------

params_control <- c(params, list(
  LR = 0.65,
  inv_temp = 0.5,
  initQ = list(F = 3, U = 6),
  mu_R = list(F = 3, U = 6)
))
params_A <- params_control
params_A$initQ <- list(F = 8, U = 6)
params_B <- params_A
params_B$LR <- 0.005

dat_control <- gen_plot_dat(util$run_std(params_control))
dat_control$intervention <- "Control"
dat_A <- gen_plot_dat(util$run_std(params_A))
dat_A$intervention <- "Intervention A"
dat_B <- gen_plot_dat(util$run_std(params_B))
dat_B$intervention <- "Intervention B"

plot_dat <- rbind(dat_control, dat_A, dat_B)

colors <- list("Control" = "#a0a0a0",
  "Intervention A" = "#7ebcdd",
  "Intervention B" = "#0065a9",
  "Intervention C" = "#0065a9"
)

fig_3a <- ggplot(plot_dat) +
  geom_smooth(aes(x = trial, y = choice_prop, color = intervention, fill = intervention)) +
  ylim(c(0, 1)) +
  labs(x = "Trial",
       y = "Chose climate-friendly",
       caption = "\n(a)") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  util$my_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.095))

# ggsave("simulation-plot-initQ-LR.svg", width = 5, height = 5)

# FIGURE 3B: INVERSE TEMPERATURE AND RATINGS
# ------------------------------------------

params_control <- c(params, list(
  LR = 0.65,
  inv_temp = 0.5,
  initQ = list(F = 3, U = 6),
  mu_R = list(F = 3, U = 6)
))
params_C <- params_control
params_C$mu_R <- list(F = 7, U = 6)

dat_control <- gen_plot_dat(util$run_std(params_control))
dat_control$intervention <- "Control"
dat_C <- gen_plot_dat(util$run_std(params_C))
dat_C$intervention <- "Intervention C"

plot_dat <- rbind(dat_control, dat_C)

fig_3b <- ggplot(plot_dat) +
  geom_smooth(aes(x = trial, y = choice_prop, color = intervention, fill = intervention)) +
  ylim(c(0, 1)) +
  labs(x = "Trial",
       y = "Chose climate-friendly",
       caption = "\n(b)") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  util$my_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.07))

# ggsave("simulation-plot-invtemp-R.svg", width = 5, height = 5)
p <- gridExtra::grid.arrange(fig_3a, fig_3b, ncol = 2)
ggsave("simulation-plots.svg", p, width = 10, height = 6)
ggsave("simulation-plots.png", p, width = 10, height = 6)
