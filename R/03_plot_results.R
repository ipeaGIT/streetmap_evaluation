# description -------------------------------------------------------------

#

# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

# Load data

validated_sample_df <- read_csv("../../data/geocode/streetmap_eval/validated_sample_completo.csv") %>%
  mutate(geocode_result = case_when(geocode_distance <= 0 ~ "VALID",
                                  geocode_distance <= 350 ~ "GOOD",
                                  geocode_distance <= 1000 ~ "AVERAGE",
                                  TRUE ~ "BAD"))

unique(validated_sample_df$geocode_type)
geocode_type_levels <- c("PointAddress", "StreetAddress", "StreetAddressExt", "StreetName", "POI",
                         "PostalExt", "PostalLoc", "Postal", "Locality", "DistanceMarker")

# Preparacao dos dados


validated_count_df <- validated_sample_df %>%
  count(city, geocode_result)


city_order <- validated_count_df %>%
  pivot_wider(names_from = geocode_result, values_from = n) %>%
  ungroup() %>%
  arrange(VALID, GOOD, AVERAGE, BAD)

validated_count_df <- validated_count_df %>%
  mutate(city = factor(city, levels = city_order$city),
         geocode_result = factor(geocode_result,
                                 levels = c("VALID", "GOOD", "AVERAGE", "BAD"),
                                 labels = c("VALID", "GOOD (< 350m)", "AVERAGE (< 1km)", "BAD"))) %>%
  mutate(geocode_result = fct_rev(geocode_result))



# Resultado geral por MUNICIPIO -------------------------------------------

validated_count_df %>%
  ggplot() + geom_col(aes(x = city, y = n, fill = geocode_result)) +
  coord_flip() +
  scale_fill_brewer(palette = "Spectral", direction = 1, guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 50)) +
  labs(title = "Resultado geral do geocoding, por município",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  )


# Resultado por STATUS: Match, Unmatch, Tie -------------------------------

p_status_perc <- validated_sample_df %>%
  count(geocode_status, geocode_result) %>%
  group_by(geocode_status) %>%
  mutate(p = n / sum(n)) %>%
  mutate(geocode_status = factor(geocode_status,
                                 levels = c("M", "T", "U"),
                                 labels = c("MATCH", "TIE", "UNMATCH"))) %>%
  mutate(geocode_result = factor(geocode_result,
                                 levels = c("VALID", "GOOD", "AVERAGE", "BAD"),
                                 labels = c("VALID", "GOOD (< 350m)", "AVERAGE (< 1km)", "BAD"))) %>%
  mutate(geocode_result = fct_rev(geocode_result), geocode_status = fct_rev(geocode_status)) %>%
  ggplot() + geom_col(aes(x = geocode_status, y = p, fill = geocode_result)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 1, guide = guide_legend(reverse = TRUE)) +
  scale_y_percent() +
  # scale_y_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 50)) +
  labs(#title = "Resultado geral do geocoding, por STATUS",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  )

p_status_abs <- validated_sample_df %>%
  count(geocode_status, geocode_result) %>%
  group_by(geocode_status) %>%
  mutate(geocode_status = factor(geocode_status,
                                 levels = c("M", "T", "U"),
                                 labels = c("MATCH", "TIE", "UNMATCH"))) %>%
  mutate(geocode_result = factor(geocode_result,
                                 levels = c("VALID", "GOOD", "AVERAGE", "BAD"),
                                 labels = c("VALID", "GOOD (< 350m)", "AVERAGE (< 1km)", "BAD"))) %>%
  mutate(geocode_result = fct_rev(geocode_result), geocode_status = fct_rev(geocode_status)) %>%
  ggplot() + geom_col(aes(x = geocode_status, y = n, fill = geocode_result)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 1, guide = guide_legend(reverse = TRUE)) +
  # scale_y_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 50)) +
  labs(#title = "Resultado geral do geocoding, por STATUS",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  )

(p_status_abs + p_status_perc) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Resultado geral do geocoding, por STATUS")



validated_sample_df %>%
  filter(geocode_status != "U") %>%
  count(geocode_status, geocode_type, geocode_result) %>%
  group_by(geocode_status, geocode_type) %>%
  mutate(p = n / sum(n)) %>%
  mutate(geocode_type = factor(geocode_type, levels = geocode_type_levels)) %>%
  mutate(geocode_type = fct_rev(geocode_type)) %>%
  mutate(geocode_status = factor(geocode_status,
                                 levels = c("M", "T", "U"),
                                 labels = c("MATCH", "TIE", "UNMATCH"))) %>%
  ggplot() + geom_col(aes(x = geocode_type, y = p, fill = geocode_result)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  scale_y_percent() +
  # scale_y_continuous(breaks = seq(0, 1000, 100), minor_breaks = seq(0, 1000, 50)) +
  labs(title = "Resultado geral do geocoding, por ADDR_TYPE",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  ) +
  facet_wrap(~geocode_status, scales = "free", ncol = 1)

validated_sample_df %>%
  filter(geocode_status != "U") %>%
  count(geocode_status, geocode_type, geocode_result) %>%
  group_by(geocode_status, geocode_type) %>%
  mutate(p = n / sum(n)) %>%
  mutate(geocode_type = factor(geocode_type, levels = geocode_type_levels)) %>%
  mutate(geocode_type = fct_rev(geocode_type)) %>%
  mutate(geocode_status = factor(geocode_status,
                                 levels = c("M", "T", "U"),
                                 labels = c("MATCH", "TIE", "UNMATCH"))) %>%
  ggplot() + geom_col(aes(x = geocode_type, y = n, fill = geocode_result)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  # scale_y_percent() +
  scale_y_continuous(breaks = seq(0, 10000, 1000), minor_breaks = seq(0, 10000, 250)) +
  labs(title = "Resultado geral do geocoding, por ADDR_TYPE",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  ) +
  facet_wrap(~geocode_status,  ncol = 1) +
  theme(legend.position = "bottom")





validated_sample_df %>%
  filter(geocode_status == "M") %>%
  mutate(geocode_type = factor(geocode_type, levels = geocode_type_levels),
         geocode_result = factor(geocode_result,
                                 levels = c("VALID", "GOOD", "AVERAGE", "BAD"))) %>%
  mutate(geocode_type = fct_rev(geocode_type)) %>%
  ggplot() + geom_boxplot(aes(x=geocode_type, y = geocode_score, fill = geocode_result)) +
  geom_hline(yintercept = c(90, 95)) +
  coord_flip() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  # scale_y_percent() +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  labs(title = "Resultado do geocoding SCORE, por ADDR_TYPE",
       x = NULL, y = NULL, fill = NULL,
       caption = "VALID: dentro do setor censitário\nGOOD: até 350m do setor censitário\nAVERAGE: até 1km do setor censitário\nBAD: acima de 1km do setor censitário"
  ) +
  facet_wrap(~geocode_result, nrow = 1)

# Confusion tables

validated_sample_df %>%
  filter(geocode_match != "U") %>%
  select(cnefe_id:city, geocode_status:geocode_result, -geocode_match, -geocode_rank) %>%
  mutate(score_cut = if_else(geocode_score >= 95, "HIGH", "LOW"),
         result_cut = if_else(geocode_distance == 0, "CORRECT", "WRONG")) %>%
  count(geocode_status, geocode_type, result_cut, score_cut) %>%
  group_by(geocode_type) %>%
  mutate(p = n / sum(n)) %>%
  ggplot() + geom_col(aes(x=paste(result_cut, score_cut), y = p, group = geocode_type)) +
  coord_polar() +
  facet_wrap(~geocode_type)

  drop_na() %>%
  ggplot() +
  geom_path(aes(x=geocode_type, y=n, fill = p_95)) +
  coord_flip() +
  facet_wrap(~geocode_result)



