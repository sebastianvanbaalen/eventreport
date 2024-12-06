aggregateData <- function(
    data, group_var = "event_id",
    find_mode = NULL, find_mode_na_ignore = NULL, find_mode_bin = NULL, find_mode_date = NULL,
    find_mode_numeric = NULL, find_least_precise = NULL, find_most_precise = NULL,
    combine_strings = NULL, find_max = NULL, find_min = NULL, precision_var = NULL,
    aggregation_name = NULL, tie_break = "default_tie_break", second_tie_break = "default_tie_break"
) {
  suppressWarnings({
    # Add a default tie break column to the data if not already present
    if (!"default_tie_break" %in% names(data)) {
      data$default_tie_break <- 1
    }

    summarise_list <- list()

    # Add additional summarising logic as needed for other variables
    if (!is.null(find_mode)) {
      cat_vars <- syms(find_mode)
      summarise_list[find_mode] <- purrr::map(cat_vars, ~quo(calc_mode(!!.x, !!sym(tie_break), !!sym(second_tie_break))))
    }

    if (!is.null(find_mode_na_ignore)) {
      na_ignore_vars <- syms(find_mode_na_ignore)
      summarise_list[find_mode_na_ignore] <- purrr::map(na_ignore_vars, ~quo(calc_mode_na_ignore(!!.x, !!sym(tie_break), !!sym(second_tie_break))))
    }

    if (!is.null(find_mode_bin)) {
      bin_vars <- syms(find_mode_bin)
      summarise_list[find_mode_bin] <- purrr::map(bin_vars, ~quo(calc_mode_binary(!!.x)))
    }

    if (!is.null(find_mode_date)) {
      date_vars <- syms(find_mode_date)
      summarise_list[find_mode_date] <- purrr::map(date_vars, ~quo(calc_mode_date(!!.x)))
    }

    if (!is.null(find_mode_numeric)) {
      numeric_vars <- syms(find_mode_numeric)
      summarise_list[find_mode_numeric] <- purrr::map(numeric_vars, ~quo(calc_mode_numeric(!!.x)))
    }

    if (!is.null(combine_strings)) {
      string_vars <- syms(combine_strings)
      summarise_list[combine_strings] <- purrr::map(string_vars, ~quo(aggregate_strings(!!.x)))
    }

    if (!is.null(find_max)) {
      max_vars <- syms(find_max)
      summarise_list[find_max] <- purrr::map(max_vars, ~quo(max(!!.x, na.rm = TRUE)))
    }

    if (!is.null(find_min)) {
      min_vars <- syms(find_min)
      summarise_list[find_min] <- purrr::map(min_vars, ~quo(min(!!.x, na.rm = TRUE)))
    }

    if (!is.null(find_least_precise)) {
      min_precision_vars <- syms(find_least_precise)
      if (!is.null(precision_var)) {
        summarise_list[find_least_precise] <- purrr::map(min_precision_vars, ~quo(calc_min_precision(!!.x, !!sym(precision_var), !!sym(tie_break), !!sym(second_tie_break))))
      }
    }

    if (!is.null(find_most_precise)) {
      max_precision_vars <- syms(find_most_precise)
      if (!is.null(precision_var)) {
        summarise_list[find_most_precise] <- purrr::map(max_precision_vars, ~quo(calc_max_precision(!!.x, !!sym(precision_var), !!sym(tie_break), !!sym(second_tie_break))))
      }
    }

    result <- data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        !!!summarise_list,
        number_of_sources = n(),
        .groups = "drop"
      ) %>%
      mutate(
        unit_of_analysis = "Event",
        aggregation = aggregation_name
      )
  })

  return(result)
}
