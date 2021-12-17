# description -------------------------------------------------------------

# Este script faz o download da base de dados do CNEFE / IBGE, referente ao
# censo de 2010

# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

# list of Brazilian municipalities
munis_all_df <- geobr::lookup_muni("all") %>% setDT()
munis_df[munis_all_df, on = .(code_muni), name_muni_pt := i.name_muni]


## Galileo results
gmaps_geocoded_df <- read_csv("../../data/geocode/streetmap_eval/cnefe_large_sample_bsb_output.csv")
                                  # locale = locale(decimal_mark = ",", grouping_mark = ".", encoding = "windows-1250")) %>%
  # mutate(code_tract = as.character(code_tract))

gmaps_geocoded_sf <- gmaps_geocoded_df %>%
  mutate(lon = if_else(is.na(lon), 0, lon),
         lat = if_else(is.na(lat), 0, lat)) %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326)

output_sf <- "../../data/geocode/streetmap_eval/validated_sample_gmaps_bsb.gpkg"
output_csv <- "../../data/geocode/streetmap_eval/validated_sample_gmaps_bsb.csv"

# Load sample addresses
streetmap_sample_df <- read_csv("../../data/geocode/streetmap_eval/cnefe_large_sample_bsb.csv") %>%
  mutate(code_tract = as.character(code_tract))

# functions ---------------------------------------------------------------
# muni <- 2304400 # Fortaleza
# muni <- 1302603 # Manaus
# muni <- 3550308 # São Paulo
# muni <- 3509502 # Campinas
# muni <- 5300108 # Brasília

validate_geocoding <- function(muni) {
  # extract urban area name and uf from data
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_estado)
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni_pt)

  message(sprintf("Working on city %s / %s", muni_name, muni_uf))

  # prepare geocoded sample
  sample_df <- streetmap_sample_df %>% filter(city == muni_name) %>% setDT()
  gmaps_sf <- gmaps_geocoded_sf %>%
    st_set_geometry(NULL) %>% setDT()

  sample_df[gmaps_sf, on = .(cnefe_id = co_entidade),
            `:=`(lat = i.lat, lon = i.lon,
                 geocode_precision = i.Addr_type)]

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

# spo_sf <- validate_geocoding(3550308)
# spo_sf %>% filter(geocode_distance < 1000) %>% mapview(zcol="geocode_distance")

# codes <- unique(munis_df$code_muni)
codes <- 5300108

validated_sample_sf <- map_df(codes, validate_geocoding)

validated_sample_sf <- validated_sample_sf %>%
  mutate(geocode_result = case_when(geocode_distance <= 0 ~ "VALID",
                                    geocode_distance <= 350 ~ "GOOD",
                                    geocode_distance <= 1000 ~ "AVERAGE",
                                    TRUE ~ "BAD"))


validated_sample_sf %>% st_write(output_sf)
validated_sample_sf %>%
  st_set_geometry(NULL) %>%
  write_csv(output_csv)






