#' Load the most-typical aggregation of MAVERICK
#'
#' This convenience function aggregates the MAVERICK event report data to the event level using the most-typical aggregation model.
#'
#' @param data The MAVERICK event report level dataset. Already pre-loaded.
#'
#' @return Returns a dataframe of the most-typical aggregation of MAVERICK.
#' @export
#'
#' @examples
#' maverick_typical <- aggregate_maverick_typ()

aggregate_maverick_typ <- function(data) {
  aggregateData(
    data = maverick_event_report,
    group_var = "event_id",
    find_mode = c(
      "country", "election", "city", "location",
      "actor1", "actor1_id", "actor1_type", "actor1_subtype", "actor1_party", "actor1_violence",
      "actor2", "actor2_id", "actor2_type", "actor2_subtype", "actor2_party", "actor2_violence",
      "actor3", "actor3_id", "actor3_type", "actor3_subtype", "actor3_party", "actor3_violence",
      "actor4", "actor4_id", "actor4_type", "actor4_subtype", "actor4_party", "actor4_violence",
      "actor5", "actor5_id", "actor5_type", "actor5_subtype", "actor5_party", "actor5_violence",
      "actor6", "actor6_id", "actor6_type", "actor6_subtype", "actor6_party", "actor6_violence",
      "event_context", "target"
    ),
    find_mode_date = c("date_start", "date_end"),
    find_mode_numeric = c(
      "latitude", "longitude", "geo_precision",
      "actor1_initiator", "actor1_perpetrator", "actor1_victim", "actor1_intervener", "actor1_bystander",
      "actor2_initiator", "actor2_perpetrator", "actor2_victim", "actor2_intervener", "actor2_bystander",
      "actor3_initiator", "actor3_perpetrator", "actor3_victim", "actor3_intervener", "actor3_bystander",
      "actor4_initiator", "actor4_perpetrator", "actor4_victim", "actor4_intervener", "actor4_bystander",
      "actor5_initiator", "actor5_perpetrator", "actor5_victim", "actor5_intervener", "actor5_bystander",
      "actor6_initiator", "actor6_perpetrator", "actor6_victim", "actor6_intervener", "actor6_bystander",
      "deaths_best", "deaths_low", "deaths_high", "injuries_best", "injuries_low", "injuries_high"
    ),
    find_mode_bin = c("displacement", "damage"),
    combine_strings = "source",
    find_max = c("certain1", "certain2", "certain3", "certain4", "certain5", "certain6"),
    tie_break = "source_classification",
    second_tie_break = "certain",
    aggregation_name = "Typical"
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
