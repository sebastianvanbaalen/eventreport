# Create a subset of the data
small_maverick_event_report <- maverick_event_report %>%
  dplyr::select(id, event_id, country, city, location, date_start, actor1, deaths_best, injuries_best, source) %>%
  dplyr::arrange(event_id) %>%
  utils::head(100)  # Keep only the first 100 rows

# Save the subset to the package
usethis::use_data(small_maverick_event_report, overwrite = TRUE)

