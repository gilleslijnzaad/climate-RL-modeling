packages <- c(
  "dplyr",
  "ggplot2",
  "tidyr",
  "truncnorm",
  "irr"
)

install.packages(packages, dependencies = TRUE)

install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
cmdstanr::install_cmdstan()