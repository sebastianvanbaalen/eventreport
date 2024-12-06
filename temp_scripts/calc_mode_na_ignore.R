#' A function for finding the mode value of character variables while ignoring NA values and empty strings
#' @param x A character vector for which you want to find the mode value.
#' @param tie_break A numeric vector that acts as the first tie-break in the case of multiple mode values.
#' @param second_tie_break A numeric vector that acts as the second tie-break in the case of multiple mode values.
#' @returns A character vector that represents the mode value. Ignores NA values and empty strings. Returns the
#' value "Indeterminate" when no mode value can be determined after two attempted tie-breaks.
#' @importFrom dplyr filter select summarise group_by
#' @importFrom purrr possibly
#' @examples
#' calc_mode_na_ignore(c("a", "b", "a", "b", NA), c(1, 2, 2, 1, 1), c(1, 2, 2, 1, 1))
#' @export
calc_mode_na_ignore <- function(x, tie_break, second_tie_break) {
  # Safe function wrapper to handle errors and suppress warnings
  safe_calc_mode_na_ignore <- possibly(function(x, tie_break, second_tie_break) {
    # Input validation and filtering
    if (!is.character(x)) {
      warning("Input 'x' should be a character vector.")
      return("Indeterminate")
    }

    if (!is.null(tie_break) && !is.numeric(tie_break)) {
      warning("Tie break values should be numeric.")
      return("Indeterminate")
    }

    if (!is.null(second_tie_break) && !is.numeric(second_tie_break)) {
      warning("Second tie break values should be numeric.")
      return("Indeterminate")
    }

    # Remove NAs and empty strings
    valid_indices <- !is.na(x) & x != ""
    x <- x[valid_indices]
    tie_break <- tie_break[valid_indices]
    second_tie_break <- second_tie_break[valid_indices]

    if (length(x) == 0) return("")

    # Calculate mode
    tbl <- table(x)
    max_count <- max(tbl)
    mode_values <- names(tbl)[tbl == max_count]

    if (is.null(tie_break) || length(mode_values) == 1) {
      return(ifelse(length(mode_values) == 1, mode_values, "Indeterminate"))
    }

    # Handling first tie-break
    data <- data.frame(x, tie_break, second_tie_break, stringsAsFactors = FALSE)
    data <- data[order(data$tie_break, decreasing = TRUE),]
    highest_tie_break_value <- data$tie_break[1]
    data <- data[data$tie_break == highest_tie_break_value,]

    if (length(unique(data$x)) == 1) {
      return(data$x[1])
    }

    # Handling second tie-break
    data <- data[order(data$second_tie_break, decreasing = TRUE),]
    highest_second_tie_break_value <- data$second_tie_break[1]
    data <- data[data$second_tie_break == highest_second_tie_break_value,]

    final_mode_values <- unique(data$x)

    if (length(final_mode_values) == 1) {
      return(final_mode_values)
    } else {
      return("Indeterminate")
    }
  }, otherwise = "Indeterminate", quiet = TRUE)

  # Execute the safe function
  return(safe_calc_mode_na_ignore(x, tie_break, second_tie_break))
}
