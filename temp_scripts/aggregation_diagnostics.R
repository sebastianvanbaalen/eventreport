aggregation_diagnostics <- function(data = NULL, group_var = NULL) {

  # Group the data by the group_var
  grouped_data <- data %>%
    group_by(!!sym(group_var))

  # Calculate the total number of observations
  total_observations <- nrow(data)

  # Calculate the total number of unique values for the group_var
  total_unique_values <- n_distinct(data[[group_var]])

  # Calculate the mean number of observations per unique value of group_var
  mean_observations_per_group <- total_observations / total_unique_values

  # Create the final diagnostics dataframe
  diagnostics <- data.frame(
    metric = c("total_observations", "total_unique_values", "mean_observations_per_group"),
    value = c(total_observations, total_unique_values, mean_observations_per_group)
  )

  return(diagnostics)

}
