#' A function for finding the mode value of character variables
#' @param x A character vector for which you want to find the mode value.
#' @param tie_break A numeric vector that acts as the first tie-break in the case of multiple mode values.
#' @param second_tie_break A numeric vector that acts as the second tie-break in the case of multiple mode values.
#' @returns A character vector that represents the mode value. Treats empty strings as real values,
#' and returns an empty string in cases where an empty string is the mode value. Returns the value "Indeterminate"
#' when no mode value can be determined after two attempted tie-breaks.
#' @importFrom dplyr data_frame group_by summarise
#' @importFrom purrr possibly
#' @examples
#' calc_mode(c("a", "b", "a", "b"), c(1, 2, 2, 1), c(1, 2, 2, 1))
#' @export
calc_mode <- function(x, tie_break = NULL, second_tie_break = NULL) {
  safe_calc_mode <- possibly(function(x, tie_break, second_tie_break) {
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

    if (length(x) == 0) return("") # Return an empty string if vector is empty

    tbl <- table(x)
    max_count <- max(tbl)
    mode_values <- names(tbl)[tbl == max_count]

    if (is.null(tie_break) || length(mode_values) == 1) {
      return(ifelse(length(mode_values) == 1, mode_values, "Indeterminate"))
    }

    # First tie-break process
    data <- data.frame(x, tie_break, second_tie_break, stringsAsFactors = FALSE)
    data <- na.omit(data)
    data <- data[order(data$tie_break, decreasing = TRUE),]

    if (length(unique(data$x)) == 1) {
      return(data$x[1])
    }

    # Check if there's still a tie after first tie-break
    highest_tie_break_value <- data$tie_break[1]
    data <- data[data$tie_break == highest_tie_break_value,]

    if (length(unique(data$x)) == 1) {
      return(data$x[1])
    }

    # Second tie-break process
    data <- data[order(data$second_tie_break, decreasing = TRUE),]
    if (length(unique(data$x)) == 1) {
      return(data$x[1])
    }

    highest_second_tie_break_value <- data$second_tie_break[1]
    final_mode_values <- unique(data$x[data$second_tie_break == highest_second_tie_break_value])

    if (length(final_mode_values) == 1) {
      return(final_mode_values)
    } else {
      return("Indeterminate")
    }

  }, otherwise = "Indeterminate", quiet = TRUE)

  return(safe_calc_mode(x, tie_break, second_tie_break))
}
