# === cred_int() ========================
# arguments: 
# - posterior_dist: array of posterior distribution
# 
# returns: 
# - bounds of 95% credibility interval in array
cred_int <- function(posterior_dist) {
  return(as.numeric(quantile(posterior_dist, c(0.025, 0.975))))
}

# === print_posterior_table() ========================
# arguments: 
# - draws: data frame of posterior draws from model
# - param_settings: named list of parameter settings
# - to_show: string array of parameters to show in table
# 
# returns: 
# - nothing
print_posterior_table <- function(draws, param_settings, to_show) {
  table_data <- data.frame()

  for (p in to_show) {
    if (grepl("\\$", p)) { # parameter is part of a list
      split_name <- strsplit(p, "\\$")[[1]]
      sim_value <- purrr::pluck(param_settings, split_name[1], split_name[2])
      param_name <- paste0(split_name[1], "\\$", split_name[2]) # escape dollar sign for kable
    } else {
      sim_value <- param_settings[[p]]
      param_name <- p
    }
    dat <- data.frame(
      parameter = param_name,
      sim_value = sim_value,
      median_CI = sprintf("%.2f [%.2f, %.2f]",
                          median(draws[[p]]),
                          cred_int(draws[[p]])[1],
                          cred_int(draws[[p]])[2])
    )
    table_data <- rbind(table_data, dat)
  }
  colnames <- c("Parameter", "Simulated value",
                "Median [95% credibility interval]")
  knitr::kable(table_data,
               col.names = colnames,
               align = "lll",
               caption = "Posteriors for free parameters") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left")
}

print_corr_indiv_table <- function(draws, param_settings, to_show) {
  n_part <- param_settings$n_part
  table_data <- c()
  for (p in to_show) {
    median_draws <- c()
    for (j in 1:n_part) {
      param_name <- paste0(p, "[", j, "]")
      val <- median(draws[[param_name]])
      median_draws <- c(median_draws, val)
    }
    corr <- round(cor(param_settings[[p]], median_draws), 2)
    dat <- data.frame(parameter = p, corr = corr)
    table_data <- rbind(table_data, dat)
  }
  colnames <- c("Parameter", "Correlation")
  knitr::kable(table_data,
               col.names = colnames,
               align = "lll",
               caption = "Correlation of simulated and estimated parameters, at the participant level") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left")
}
