# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')

library(ggmap) # geocoding

sample_bsb <- read_csv("../../data/geocode/streetmap_eval/cnefe_large_sample_bsb.csv")

enderecos <-
  sample_bsb %>%
  mutate(fim = paste0(address, ", ", address2, " - ", neighbourhood, " - ", city, ", ", state, " - CEP ", post_code)) %>%
  pull(fim)


# 3.3) Registrar Google API Key
my_api <- data.table::fread("../../data-raw/google_key.txt", header = TRUE)


register_google(key = my_api$key[4]) # marcus



coordenadas_google <- lapply(X=enderecos[1:10], ggmap::geocode, output = "all")
names(coordenadas_google) <- sample_bsb$cnefe_id[1:10]



# function to create data.frame from gmaps output
create_dt <- function(x) {

  precision_depth0 <- ifelse(length(x[["results"]][[1]][["address_components"]]) > 0,
                             x[["results"]][[1]][["address_components"]],
                             NA)

  # check length from precision depth
  precision_depth <- ifelse(is.na(precision_depth0), NA,
                            ifelse(length(precision_depth0[[1]]$types) > 0,
                                   precision_depth0[[1]]$types[[1]],
                                   NA))

  a <- data.table(
    # MatchedAddress = ifelse(!is.null(x[["results"]][[1]][["formatted_address"]]), x[["results"]][[1]][["formatted_address"]], NA),
    # PrecisionDepth = ifelse(!is.null(x[["results"]][[1]][["address_components"]][[1]]$types[[1]]), x[["results"]][[1]][["address_components"]][[1]]$types[[1]], NA),
    Addr_type = precision_depth,
    lon = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lng"]]), x[["results"]][[1]][["geometry"]][["location"]][["lng"]], NA),
    lat = ifelse(!is.null(x[["results"]][[1]][["geometry"]][["location"]][["lat"]]), x[["results"]][[1]][["geometry"]][["location"]][["lat"]], NA)
  )

  return(a)

}

create_dt(coordenadas_google[1])
write_rds(coordenadas_google_10, sprintf("../../data/acesso_oport/rais/%s/geocode/gmaps_temp10.rds", ano))


