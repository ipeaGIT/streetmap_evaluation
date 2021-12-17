# setup -------------------------------------------------------------------

source('R/setup.R')
source('R/munis_df.R')



large_sample <- read_csv("../../data/geocode/streetmap_eval/cnefe_large_sample_streetmap.csv")
large_sample %>%
  filter(state == "DF") %>%
  write_csv("../../data/geocode/streetmap_eval/cnefe_large_sample_bsb.csv")

