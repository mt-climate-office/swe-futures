library(magrittr)
library(tidyverse)
library(lmom)
library(snotelr)

# Load all functions
list.files("R", full.names = TRUE) %>%
  purrr::walk(source)

# # Make a map of all MT SNOTEL sites
snotelr::snotel_info() %>%
  tibble::as_tibble() %>%
  # dplyr::filter(state == "MT") %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  mapview::mapview()

snotel_data <-
  snotelr::snotel_download(407, 
                           internal = TRUE) %>%
  calc_future_swe()

plot_snotel(snotel_data)

ggsave("901.png",
       width = 8,
       height = 5,
       units = "in")

plot_snotel(snotel_data, usdm = TRUE)

ggsave("901_usdm.png",
       width = 8,
       height = 5,
       units = "in")
