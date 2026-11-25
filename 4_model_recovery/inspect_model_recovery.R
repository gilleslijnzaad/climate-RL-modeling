## ----rmd-setup, include = FALSE-----------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl) # creates an R file upon knitting the Rmd
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)
knitr::opts_chunk$set(fig.width = 10, fig.height = 8)

## ----setup, message = FALSE---------------------------------------------------
rm(list = ls())
library(tidyverse)
library(bridgesampling)
main_dir <- "~/research/climate-RL-mod/"
recov_dir <- paste0(main_dir, "4_model_recovery/")
util_dir <- paste0(main_dir, "9_utilities/")
plot <- new.env()
source(paste0(util_dir, "plot_utils.R"), local = plot)

## ----create-dat---------------------------------------------------------------
n_runs <- 1

confusion <- data.frame(
  sim_mod = (rep(0:2, each = 3)),
  fit_mod = (rep(0:2, 3)),
  p = 0
)

fits <- list(
  "0" = NA,
  "1" = NA,
  "2" = NA
)

for (sim_model in as.character(0:2)) {
  for (run in 1:n_runs) {
    for (fit_model in as.character(0:2)) {
      fits[[fit_model]] <- readRDS(sprintf("%ssim%s/fit%s/bridge_%.03d.rds", recov_dir, sim_model, fit_model, run))
    }
    winner_I <- ifelse(bf(fits[["1"]], fits[["0"]])$bf > 3,
                       "1",
                       "0")
    winner_II <- ifelse(bf(fits[["2"]], fits[["1"]])$bf > 3,
                       "2",
                       "1")
    final_winner <- ifelse(bf(fits[[winner_II]], fits[[winner_I]])$bf > 3,
                       winner_II,
                       winner_I)
    confusion <- confusion %>%
      mutate(p = if_else(sim_mod == sim_model & fit_mod == final_winner,
                         p + 1/n_runs,
                         p))
  }
}

## ----make-confusion-matrix----------------------------------------------------
ggplot(confusion, aes(x = sim_mod, y = fit_mod, fill = p)) +
  geom_tile(color = "black") +
  geom_text(aes(label = p), size = 18, size.unit = "pt") +
  scale_fill_gradient(low = "white", high = plot$my_teal, guide = "none") +
  labs(x = "Simulated model", y = "Fitted model") +
  plot$my_theme

