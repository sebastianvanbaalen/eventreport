#' @export
calc_max_precision <- function(precision, x, tie_break, second_tie_break) {
  # Check if all precision values are NA, return NA or an appropriate message/value
  if (all(is.na(precision))) {
    return("")  # Or return("") if an empty string is more appropriate
  }

  # Filter the values to those with the maximum precision (ignoring NA values in precision)
  max_precision <- max(precision, na.rm = TRUE)
  valid_indices <- precision == max_precision

  x_filtered <- x[valid_indices]
  tie_break_filtered = tie_break[valid_indices]
  second_tie_break_filtered = second_tie_break[valid_indices]

  # If after filtering there are no entries left, return NA or empty string
  if (length(x_filtered) == 0) {
    return("")  # Or return("") if an empty string is more appropriate
  }

  # Use your existing calc_mode function to find the mode
  # Assuming calc_mode is adapted to handle dynamic tie-breaks as well
  mode_value <- calc_mode(x_filtered, tie_break_filtered, second_tie_break_filtered)

  return(mode_value)
}
