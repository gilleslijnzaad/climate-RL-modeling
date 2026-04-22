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
