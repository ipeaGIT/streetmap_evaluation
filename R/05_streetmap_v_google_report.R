library(tidyverse)
library(patchwork)
# source(here::here('R/setup.R'))
source(here::here('R/munis_df.R'))



validated_streetmap_df <-
  read_csv("../../../data/geocode/streetmap_eval/validated_sample_completo.csv") %>%
  mutate(geocode_result = if_else(geocode_distance <= 350, "GOOD", "BAD"))

validated_gmaps_df <-
  read_csv("../../../data/geocode/streetmap_eval/validated_sample_gmaps.csv") %>%
  mutate(geocode_result = if_else(geocode_distance <= 350, "GOOD", "BAD"))


validated_combined <- left_join(validated_streetmap_df, validated_gmaps_df,
                                by = c("cnefe_id", "country", "state", "city")) %>%
  select(cnefe_id, country, state, city,
         streetmap_status = geocode_status,
         streetmap_score = geocode_score,
         streetmap_precision = geocode_type,
         streetmap_result = geocode_result.x,
         gmaps_precision = geocode_precision,
         gmaps_result = geocode_result.y)


unique(validated_combined$streetmap_precision)
unique(validated_combined$gmaps_precision)

geocode_type_levels <- c("PointAddress", "StreetAddress", "StreetAddressExt", "StreetName", "POI",
                         "PostalExt", "PostalLoc", "Postal", "Locality", "DistanceMarker")

validated_count_df <- validated_combined %>%
  filter(streetmap_status == "M") %>%
  mutate(score = round(streetmap_score)) %>%
  count(streetmap_precision, score, streetmap_result, gmaps_result)

types = "PointAddress"
types = c("StreetAddress", "StreetAddressExt", "StreetName")
plot_type_by_score <- function(types) {

  types_text <- paste(types, collapse = ", ")

  validated_count_df %>%
    filter(streetmap_precision %in% types) %>%
    mutate(n = if_else(gmaps_result == "GOOD", n, -n )) %>%
    ggplot(aes(x=score, y = n, fill = gmaps_result)) +
    geom_col(position = "stack") +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 95) +
    scale_fill_brewer(palette = "Set1", direction = 1) +
    scale_x_continuous(breaks = seq(0, 100, 5), minor_breaks = 0:100) +
    # facet_wrap(~streetmap_result, ncol = 2, scales = "fixed") +
    labs(title = "Streetmap vs Google Maps",
         subtitle = paste("Precision:", types_text)) +
    theme(legend.position = "none")

}

plot_type_by_score("PointAddress")
plot_type_by_score(c("StreetAddress", "StreetAddressExt", "StreetName"))
plot_type_by_score(c("Postal", "PostalExt", "PostalLoc"))
plot_type_by_score(c("DistanceMarker", "Locality", "POI"))


pa_df <- validated_count_df %>% filter(streetmap_precision == "PointAddress")
st_df <- validated_count_df %>% filter(streetmap_precision %in% c("StreetAddress", "StreetAddressExt", "StreetName"),
                                       score >= 95)

validated_choice_df <- validated_count_df %>% mutate(locator = if_else( (streetmap_precision == "PointAddress") |
                                                 ((streetmap_precision %in% c("StreetAddress", "StreetAddressExt", "StreetName")) &
                                                     (score >= 70)), "streetmap", "gmaps")) %>%
  mutate(result = if_else(locator == "gmaps", gmaps_result, streetmap_result ))

validated_choice_df %>%
  count(result, wt = n)%>%
  mutate(p = n / sum(n))

