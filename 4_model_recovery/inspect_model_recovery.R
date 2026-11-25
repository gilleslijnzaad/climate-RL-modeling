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

