# description -------------------------------------------------------------

#

# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

# Load data

validated_address_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_address.csv") %>%
  select(cnefe_id:landuse_description, geocode_distance, geocode_result) %>%
  mutate(locator = "streetmap_address")

validated_address_zip_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_address_zip.csv") %>%
  select(cnefe_id:landuse_description, geocode_distance, geocode_result) %>%
  mutate(locator = "streetmap_address_zip")

validated_complete_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_completo.csv") %>%
  select(cnefe_id:landuse_description, geocode_distance, geocode_result) %>%
  mutate(locator = "streetmap_complete")

validated_galileo_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_galileo.csv") %>%
  select(cnefe_id:landuse_description, geocode_distance, geocode_result) %>%
  mutate(locator = "galileo")

validated_gmaps_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_gmaps.csv") %>%
  select(cnefe_id:landuse_description, geocode_distance, geocode_result) %>%
  mutate(locator = "google_maps")

validated_sample_df <- rbind(validated_address_df,
                             validated_address_zip_df,
                             validated_complete_df,
                             validated_galileo_df,
                             validated_gmaps_df) %>%
  mutate(code_tract = as.character(code_tract))

# By Locator and City

validated_count_by_city <- validated_sample_df %>%
  count(city, locator, geocode_result) %>%
  mutate(geocode_result = factor(geocode_result,
                                 levels = c("VALID", "GOOD", "AVERAGE", "BAD"),
                                 labels = c("VALID", "GOOD (< 350m)", "AVERAGE (< 1km)", "BAD")))

fct_city <- validated_count_by_city %>%
  filter(geocode_result == "VALID", locator == "google_maps") %>%
  arrange(n)


validated_count_by_city %>%
  filter(locator %in% c("streetmap_complete", "galileo", "google_maps")) %>%
  mutate(locator = factor(locator, levels = c("google_maps", "streetmap_complete", "galileo"))) %>%
  mutate(city = factor(city, levels = fct_city$city)) %>%
  mutate(geocode_result = fct_rev(geocode_result)) %>%
  filter(locator %in% c("streetmap_complete", "galileo", "google_maps")) %>%
  # filter(geocode_result == "BAD") %>%
  ggplot() +
  geom_col(aes(x=city, y = n, fill = geocode_result), position = "stack") +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral", direction = 1, guide = guide_legend(reverse = FALSE)) +
  scale_y_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 50)) +
  facet_wrap(~locator) +
  theme(legend.position = "bottom")


# By Locator
validated_count_by_locator <- validated_sample_df %>%
  count(locator, geocode_result) %>%
  mutate(geocode_result = factor(geocode_result, levels = c("VALID", "GOOD", "AVERAGE", "BAD"),
                                 labels = c("VALID", "GOOD (< 350m)", "AVERAGE (< 1km)", "BAD")))

fct_city <- validated_count_by_city %>%
  filter(geocode_result == "VALID", locator == "streetmap_complete") %>%
  arrange(n)


validated_count_by_locator %>%
  ggplot() +
  geom_col(aes(x=locator, y = n, fill = locator), position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~geocode_result, ncol = 1) +
  theme(legend.position = "none")

