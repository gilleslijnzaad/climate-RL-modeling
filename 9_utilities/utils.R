#' Calculates 95% credibility interval for a posterior distribution
#' 
#' @param posterior_dist array of posterior distribution draws
#' 
#' @return vector of bounds of the 95% credibility interval
cred_int <- function(posterior_dist) {
  return(as.numeric(quantile(posterior_dist, c(0.025, 0.975))))
}

#' Prints a kable (knitr table) with the simulated value of a
#' parameter and the median of the posterior draws (95% CI)
#' 
#' @param draws: data frame of posterior draws from model
#' 
#' @param param_settings: named list of parameter settings
#' 
#' @param to_show: string array of parameters to show in table
#' 
#' @return nothing
print_posterior_table <- function(draws, param_settings, to_show) {
  table_data <- data.frame(parameter = rep(NA, length(to_show)))

  i <- 1
  for (p in to_show) {
    table_data[["parameter"]][i] <- p
    if (p %in% c("LR_disconf_group", "LR_diff_group")) {
      idx <- if (p == "LR_disconf_group") 1 else 2
      table_data[["sim_value"]][i] <- param_settings[["LRs_group"]][idx]
    } else {
      table_data[["sim_value"]][i] <- param_settings[[p]]
    }
    table_data[["median_CI"]][i] <- sprintf("%.2f [%.2f, %.2f]",
                                            median(draws[[p]]),
                                            cred_int(draws[[p]])[1],
                                            cred_int(draws[[p]])[2])
    i <- i + 1
  }
  colnames <- c("Parameter", "Simulated value",
                "Median [95% credibility interval]")
  knitr::kable(table_data,
               col.names = colnames,
               align = "lll",
               caption = "Posteriors for free parameters") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "left")
}

#' Creates one name for two parameters
#' 
#' @param name1 string
#' 
#' @param name2 string
#' 
#' @return a common name (string)
common_param_name <- function(name1, name2) {
  name1_chars <- str_split_1(name1, "")
  name2_chars <- str_split_1(name2, "")
  common_name <- paste(name1_chars[name1_chars %in% name2_chars], collapse = "")
  
  split_common_name <- str_split_1(common_name, "_")
  if (length(split_common_name) == 3) {
    common_name <- paste(split_common_name[1], split_common_name[3], sep = "_")
  }

  return(common_name)
}
