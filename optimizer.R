# Load necessary libraries
library(randtoolbox)
library(dplyr)
library(ggplot2)

# Function to generate Sobol sequence samples for the given parameters
generate_sobol_samples <- function(param_specs, n_samples) {
  param_names <- names(param_specs)
  param_ranges <- sapply(param_specs, function(x) length(x))
  dimension <- length(param_names)
  sobol_samples <- sobol(n = n_samples, dim = dimension)
  
  for (i in seq_along(param_names)) {
    if (is.numeric(param_specs[[i]])) {
      sobol_samples[, i] <- param_specs[[i]][1] + sobol_samples[, i] * (param_specs[[i]][2] - param_specs[[i]][1])
    } else {
      sobol_samples[, i] <- factor(param_specs[[i]][as.integer(sobol_samples[, i] * param_ranges[i]) + 1])
    }
  }
  
  colnames(sobol_samples) <- param_names
  return(as.data.frame(sobol_samples))
}

# Function to execute the command with given arguments and capture the output
run_command <- function(command, args) {
  cmd <- paste(command, paste(args, collapse = " "))
  result <- system(cmd, intern = TRUE, ignore.stderr = TRUE)
  return(length(result))  # Number of lines in the output
}

# Function to find the inflection point of the curve
find_inflection_point <- function(df) {
  df <- df %>% arrange(y)
  dy <- diff(df$y)
  ddy <- diff(dy)
  inflection_idx <- which(diff(sign(ddy)) != 0)[1] + 1
  return(df[inflection_idx, ])
}

# Main function to optimize command line arguments
optimize_command <- function(command, param_specs, n_samples = 100, output_file = NULL) {
  samples <- generate_sobol_samples(param_specs, n_samples)
  results <- sapply(1:nrow(samples), function(i) {
    args <- unlist(samples[i, ])
    run_command(command, args)
  })
  
  samples$y <- results
  samples <- samples %>% arrange(desc(y))
  
  # Plot the results
  plot_df <- samples %>% mutate(x = factor(1:n()))
  ggplot(plot_df, aes(x = x, y = y)) +
    geom_point() +
    geom_line() +
    labs(x = "Argument Combination", y = "Number of Output Lines", title = "Command Line Argument Optimization")
  
  inflection_point <- find_inflection_point(samples)
  print(inflection_point)
  
  if (!is.null(output_file)) {
    write.csv(samples, output_file, row.names = FALSE)
  }
}

# Example usage:
# Define parameter specifications (can be edited as per requirements)
param_specs <- list(
  arg1 = c(1, 100),  # Continuous range
  arg2 = c(TRUE, FALSE),  # Binary flag
  arg3 = c("option1", "option2", "option3")  # Categorical values
)

# Run the optimization function
optimize_command("your_command_here", param_specs, n_samples = 100, output_file = "results.csv")
