#' @export
calc_max_value <- function(x, tie_break = NULL, second_tie_break = NULL) {
  if (length(x) == 0) return(0)  # Return an empty string if vector is empty

  # Ensure that x, tie_break, and second_tie_break have no NA values for consistency
  valid_indices <- !is.na(x) & !is.na(tie_break) & !is.na(second_tie_break)
  x <- x[valid_indices]
  tie_break <- tie_break[valid_indices]
  second_tie_break <- second_tie_break[valid_indices]

  if (length(x) == 0) return(0)  # Return if all are NA after filtering

  # Find the maximum value in x
  max_value <- max(x)

  # Filter data for entries that match the maximum value
  indices_max_value <- x == max_value
  x_filtered <- x[indices_max_value]
  tie_break_filtered <- tie_break[indices_max_value]
  second_tie_break_filtered <- second_tie_break[indices_max_value]

  # Use tie_break to determine the highest tie-breaking value if multiple max values
  if (length(x_filtered) > 1 && !is.null(tie_break)) {
    highest_tie_break_index <- which.max(tie_break_filtered)
    x_filtered <- x_filtered[highest_tie_break_index]
    second_tie_break_filtered <- second_tie_break_filtered[highest_tie_break_index]
  }

  # Check again if multiple items have the same highest tie_break value
  if (length(x_filtered) > 1 && !is.null(second_tie_break)) {
    highest_second_tie_break_index <- which.max(second_tie_break_filtered)
    x_filtered <- x_filtered[highest_second_tie_break_index]
  }

  if (length(x_filtered) == 1) {
    return(x_filtered)
  } else {
    return(0)
  }
}
