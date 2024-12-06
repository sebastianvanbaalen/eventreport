#' @export
calc_min_precision <- function(data, x, precision_var, tie_break_var = NULL, second_tie_break_var = NULL) {
  # Safe version of the function using purrr::possibly
  safe_calc_min_precision <- possibly(function(data, x, precision_var, tie_break_var, second_tie_break_var) {
    precision <- data[[precision_var]]
    values <- data[[x]]
    tie_break <- if (!is.null(tie_break_var)) data[[tie_break_var]] else NULL
    second_tie_break <- if (!is.null(second_tie_break_var)) data[[second_tie_break_var]] else NULL

    # Check for all NA precision values
    if (all(is.na(precision))) {
      return("")
    }

    # Filter to minimum precision values
    min_precision <- min(precision, na.rm = TRUE)
    valid_indices <- precision == min_precision

    values_filtered <- values[valid_indices]
    tie_break_filtered <- if (!is.null(tie_break)) tie_break[valid_indices] else NULL
    second_tie_break_filtered <- if (!is.null(second_tie_break)) second_tie_break[valid_indices] else NULL

    # Return empty string if no entries left after filtering
    if (length(values_filtered) == 0) {
      return("")
    }

    # Use calc_mode to determine the mode with potential tie-breaking
    mode_value <- calc_mode(values_filtered, tie_break_filtered, second_tie_break_filtered)

    return(mode_value)
  }, otherwise = "", quiet = TRUE)

  # Call the safe function
  return(safe_calc_min_precision(data, x, precision_var, tie_break_var, second_tie_break_var))
}
