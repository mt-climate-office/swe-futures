library(magrittr)
library(tidyverse)
library(lmom)
library(snotelr)

# Load all functions
list.files("R", full.names = TRUE) %>%
  purrr::walk(source)

# # Make a map of all MT SNOTEL sites
# snotelr::snotel_info() %>%
#   tibble::as_tibble() %>%
#   # dplyr::filter(state == "MT") %>%
#   sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
#   mapview::mapview()

snotelr::snotel_download(901, 
                         internal = TRUE) %>%
  calc_future_swe() %>%
  plot_snotel()

snotelr::snotel_download(616, 
                         internal = TRUE) %>%
  calc_future_swe() %>%
  plot_snotel()

snotelr::snotel_download(563, 
                         internal = TRUE) %>%
  calc_future_swe() %>%
  plot_snotel()

snotelr::snotel_download(693, 
                         internal = TRUE) %>%
  calc_future_swe() %>%
  plot_snotel()


