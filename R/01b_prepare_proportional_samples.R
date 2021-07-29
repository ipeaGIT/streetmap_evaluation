# description -------------------------------------------------------------

# Este script faz o download da base de dados do CNEFE / IBGE, referente ao
# censo de 2010

# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

# list of Brazilian municipalities
munis_all_df <- geobr::lookup_muni("all") %>% setDT()
munis_df[munis_all_df, on = .(code_muni), name_muni_pt := i.name_muni]


# functions ---------------------------------------------------------------
# muni <- 2304400 # Fortaleza
# muni <- 1302603 # Manaus
# muni <- 3550308 # São Paulo


load_cnefe <- function(muni) {
  # extract urban area name and uf from data
  muni_uf <- unique(subset(munis_df, code_muni == muni)$abrev_estado)
  muni_name <- unique(subset(munis_df, code_muni == muni)$name_muni_pt)

  rds_file <- paste0("../../data/urbanformbr/cnefe/db/", muni_uf, "/", muni, "_", muni_name, "_", muni_uf, ".rds")

  cnefe_df <- readRDS(rds_file)
  cnefe_df <- cnefe_df[urban_rural == 1, ] # only urban addresses

  # cnefe_sample <- cnefe_df[][sample(.N, sample_size)]
  cnefe_sample <- cnefe_df

  # format addresses for streetmap
  cnefe_sample[, cnefe_id := .I]

  cnefe_sample[, address_a := fifelse(is.na(address_1),
                                       paste(place, address_2),
                                       paste(place, address_1, address_2))]

  cnefe_sample[, address_b := fifelse(is.na(modifier),
                                       house_number,
                                       paste(house_number, modifier))]

  cnefe_sample[, address2 := fifelse(is.na(complement1),
                                      "",
                                      fifelse(is.na(value_1),
                                              complement1,
                                              paste(complement1, value_1)))]

  cnefe_clean <- cnefe_sample[, .(cnefe_id, address_a, address_b, address2, borough, post_code, code_tract, landuse_id, landuse_description)]
  cnefe_clean[, address_b := fifelse(address_b == "0 SN", "SN", address_b)]
  cnefe_clean[, address := paste0(address_a, ", ", address_b)]
  cnefe_clean[, address_a := NULL]
  cnefe_clean[, address_b := NULL]

  cnefe_clean[, city := muni_name]
  cnefe_clean[, state := muni_uf]
  cnefe_clean[, country := "Brazil"]


  setcolorder(cnefe_clean, c("cnefe_id", "country", "state", "city", "address", "address2", "borough", "post_code", "code_tract"))
  setnames(cnefe_clean, c("cnefe_id", "country", "state", "city", "address", "address2", "neighbourhood", "post_code", "code_tract", "landuse_id", "landuse_description"))


  return(cnefe_clean)
}




# apply functions ---------------------------------------------------------

# process_urban_area(4301602)
codes <- unique(munis_df$code_muni)
streetmap_sample_df <- map_df(codes, load_cnefe)
setDT(streetmap_sample_df)

sample_size <- 100000

# sample_sizes <- streetmap_sample_df[, .N, by = city]
# sample_sizes[, prop := N / sum(N)]
# sample_sizes[, samp := prop * sample_size]

streetmap_sample_df <- streetmap_sample_df[][sample(.N, sample_size)]
streetmap_sample_df[, .N, by = city] %>% View()

streetmap_sample_df[, cnefe_id := .I]

write_csv(streetmap_sample_df, "../../data/geocode/streetmap_eval/cnefe_large_sample_streetmap.csv")

