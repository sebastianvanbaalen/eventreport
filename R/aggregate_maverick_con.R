#' Load the most-conservative aggregation of MAVERICK
#'
#' This convenience function aggregates the MAVERICK event report data to the event level using the most-conservative aggregation model.
#'
#' @param data The MAVERICK event report level dataset. Already pre-loaded.
#'
#' @return Returns a dataframe of the most-conservative aggregation of MAVERICK.
#' @export
#'
#' @examples
#' maverick_conservative <- aggregate_maverick_con()

aggregate_maverick_con <- function(data) {
  aggregateData(
    data = maverick_event_report,
    group_var = "event_id",
    find_mode = c(
      "country", "election", "event_context", "target"
    ),
    find_least_precise = list(
      list(var = "city", precision_var = "geo_precision"),
      list(var = "location", precision_var = "geo_precision"),
      list(var = "latitude", precision_var = "geo_precision"),
      list(var = "longitude", precision_var = "geo_precision"),
      list(var = "geo_precision", precision_var = "geo_precision"),
      list(var = "actor1", precision_var = "actor1_precision"),
      list(var = "actor1_id", precision_var = "actor1_precision"),
      list(var = "actor1_type", precision_var = "actor1_precision"),
      list(var = "actor1_subtype", precision_var = "actor1_precision"),
      list(var = "actor1_party", precision_var = "actor1_precision"),
      list(var = "actor1_violence", precision_var = "actor1_precision"),
      list(var = "actor2", precision_var = "actor2_precision"),
      list(var = "actor2_id", precision_var = "actor2_precision"),
      list(var = "actor2_type", precision_var = "actor2_precision"),
      list(var = "actor2_subtype", precision_var = "actor2_precision"),
      list(var = "actor2_party", precision_var = "actor2_precision"),
      list(var = "actor2_violence", precision_var = "actor2_precision"),
      list(var = "actor3", precision_var = "actor3_precision"),
      list(var = "actor3_id", precision_var = "actor3_precision"),
      list(var = "actor3_type", precision_var = "actor3_precision"),
      list(var = "actor3_subtype", precision_var = "actor3_precision"),
      list(var = "actor3_party", precision_var = "actor3_precision"),
      list(var = "actor3_violence", precision_var = "actor3_precision"),
      list(var = "actor4", precision_var = "actor4_precision"),
      list(var = "actor4_id", precision_var = "actor4_precision"),
      list(var = "actor4_type", precision_var = "actor4_precision"),
      list(var = "actor4_subtype", precision_var = "actor4_precision"),
      list(var = "actor4_party", precision_var = "actor4_precision"),
      list(var = "actor4_violence", precision_var = "actor4_precision"),
      list(var = "actor5", precision_var = "actor5_precision"),
      list(var = "actor5_id", precision_var = "actor5_precision"),
      list(var = "actor5_type", precision_var = "actor5_precision"),
      list(var = "actor5_subtype", precision_var = "actor5_precision"),
      list(var = "actor5_party", precision_var = "actor5_precision"),
      list(var = "actor5_violence", precision_var = "actor5_precision"),
      list(var = "actor6", precision_var = "actor6_precision"),
      list(var = "actor6_id", precision_var = "actor6_precision"),
      list(var = "actor6_type", precision_var = "actor6_precision"),
      list(var = "actor6_subtype", precision_var = "actor6_precision"),
      list(var = "actor6_party", precision_var = "actor6_precision"),
      list(var = "actor6_violence", precision_var = "actor6_precision")
    ),
    find_min = c(
      "date_start", "actor1_initiator", "actor1_perpetrator", "actor1_victim", "actor1_intervener", "actor1_bystander",
      "actor2_initiator", "actor2_perpetrator", "actor2_victim", "actor2_intervener", "actor2_bystander",
      "actor3_initiator", "actor3_perpetrator", "actor3_victim", "actor3_intervener", "actor3_bystander",
      "actor4_initiator", "actor4_perpetrator", "actor4_victim", "actor4_intervener", "actor4_bystander",
      "actor5_initiator", "actor5_perpetrator", "actor5_victim", "actor5_intervener", "actor5_bystander",
      "actor6_initiator", "actor6_perpetrator", "actor6_victim", "actor6_intervener", "actor6_bystander",
      "deaths_best", "deaths_low", "deaths_high", "injuries_best", "injuries_low", "injuries_high",
      "displacement", "damage"
    ),
    combine_strings = c("source"),
    find_max = c("date_end", "certain1", "certain2", "certain3", "certain4", "certain5", "certain6"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Conservative"
  ) %>%
  mutate(certain = certain1 + certain2 + certain3 + certain4 + certain5 + certain6) %>%
  select(
    event_id:election, date_start, date_end, city, location, latitude, longitude, geo_precision,
    actor1:actor1_violence, actor1_initiator:actor1_bystander,
    actor2:actor2_violence, actor2_initiator:actor2_bystander,
    actor3:actor3_violence, actor3_initiator:actor3_bystander,
    actor4:actor4_violence, actor4_initiator:actor4_bystander,
    actor5:actor5_violence, actor5_initiator:actor5_bystander,
    actor6:actor6_violence, actor6_initiator:actor6_bystander,
    event_context:damage, deaths_best:injuries_high,
    certain, certain1:certain6, source, number_of_sources:aggregation
  )
}

