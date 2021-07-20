# description -------------------------------------------------------------

# Este script faz o download da base de dados do CNEFE / IBGE, referente ao
# censo de 2010

# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

# list of Brazilian municipalities
munis_all_df <- geobr::lookup_muni("all") %>% setDT()
munis_df[munis_all_df, on = .(code_muni), name_muni_pt := i.name_muni]


streetmap_geocoded_sf <- st_read("../../data/geocode/streetmap_eval/cnefe_sample_streetmap_Geocoded.shp") %>%
  mutate(USER_code_ = as.character(USER_code_))
streetmap_sample_df <- read_csv("../../data/geocode/streetmap_eval/cnefe_sample_streetmap.csv") %>%
  mutate(code_tract = as.character(code_tract))

# functions ---------------------------------------------------------------
# muni <- 2304400 # Fortaleza
# muni <- 1302603 # Manaus
# muni <- 3550308 # São Paulo
# muni <- 3509502 # Campinas

validate_geocoding <- function(muni) {
  # extract urban area name and uf from data
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_estado)
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni_pt)

  message(sprintf("Working on city %s / %s", muni_name, muni_uf))

  # prepare geocoded sample
  sample_df <- streetmap_sample_df %>% filter(city == muni_name) %>% setDT()
  streetmap_sf <- streetmap_geocoded_sf %>% filter(USER_city == muni_name) %>%
    st_set_geometry(NULL) %>% setDT()

  sample_df[streetmap_sf, on = .(cnefe_id == USER_cnefe),
            `:=`(lat = i.Y, lon = i.X,
                 geocode_status = i.Status,
                 geocode_score = i.Score,
                 geocode_match = i.Match_type,
                 geocode_type = i.Addr_type,
                 geocode_rank = i.Rank)]

  sample_sf <- st_as_sf(sample_df, coords = c("lon", "lat"), crs = 4326)

  # load census tracts
  census_sf <- geobr::read_census_tract(code_tract = muni, simplified = FALSE) %>%
    st_transform(crs = 4326)

  # order census tracts according to sample addresses
  code_tracts <- sample_sf$code_tract
  ct_order <- lapply(code_tracts, function(ct) { which(census_sf$code_tract == ct)  }) %>% unlist()
  census_ordered_sf <- census_sf %>% slice(ct_order)
  census_ordered_sf <- st_make_valid(census_ordered_sf)

  # calculate distance between geocoded address and target census tract
  geo_distances <- st_distance(sample_sf, census_ordered_sf, by_element = TRUE)
  sample_sf$geocode_distance <- as.numeric(geo_distances)

  return(sample_sf)
}

# apply functions ---------------------------------------------------------

spo_sf <- validate_geocoding(3550308)
spo_sf %>% filter(geocode_distance < 1000) %>% mapview(zcol="geocode_distance")

codes <- unique(munis_df$code_muni)
validated_sample_sf <- map_df(codes, validate_geocoding)

validated_sample_sf <- validated_sample_sf %>%
  mutate(geocode_result = case_when(geocode_distance <= 0 ~ "VALID",
                                    geocode_distance <= 350 ~ "GOOD",
                                    geocode_distance <= 1000 ~ "AVERAGE",
                                    TRUE ~ "BAD"))

validated_sample_sf %>% st_write("../../data/geocode/streetmap_eval/validated_sample.gpkg")
validated_sample_sf %>%
  st_set_geometry(NULL) %>%
  write_csv("../../data/geocode/streetmap_eval/validated_sample.csv")






