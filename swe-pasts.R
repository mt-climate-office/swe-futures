library(magrittr)
library(tidyverse)
library(lmom)
library(snotelr)
library(furrr)

# Load all functions
list.files("R", full.names = TRUE) %>%
  purrr::walk(source)

get_snotel <-
  function(id, path, force.redo = FALSE){
    out.path <-
      file.path(path,paste0("snotel_",id,".csv"))
    
    if(force.redo | !file.exists(out.path)){
      
      snotelr::snotel_download(id,
                               internal = FALSE,
                               path = path)
      
    }
    
    return(out.path)
  }

snotel_pasts <-
  function(x = 
             snotelr::snotel_download(901, 
                                      internal = TRUE)){
    
    station <-
      stringr::str_to_title(x$site_name[[1]]) %>%
      stringr::str_trim() %>%
      paste0(" (Site ID: ",x$site_id[[1]],")")
    
    latest_date <-
      max(x$date)
    
    latest_swe <-
      x$snow_water_equivalent[x$date == latest_date]
    
    current_snow_year <-
      ifelse(lubridate::month(latest_date) >= 9, 
             lubridate::year(latest_date) + 1, 
             lubridate::year(latest_date))
    
    past_years <-
      min(x$date) %>%
      lubridate::year() %>%
      c(current_snow_year - 1)
    
    past <-
      calc_past_swe(x)
    
    present <-
      x %>%
      tibble::as_tibble() %>%
      dplyr::transmute(date = lubridate::as_date(date),
                       snow_year = 
                         ifelse(lubridate::month(date) >= 9, 
                                lubridate::year(date) + 1, 
                                lubridate::year(date)),
                       swe = snow_water_equivalent) %>%
      na.omit() %>%
      dplyr::filter(snow_year == current_snow_year) %>%
      dplyr::select(date, swe)
    
    past %>%
      dplyr::rename(past_swe = swe) %>%
      dplyr::left_join(present,
                       by = join_by(date)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(`% of Median SWE` = round(100 * swe / quantile(past_swe, 0.5), 1),
                    `Percentile` = round(100 * past_swe(swe), 1),
                    `Percentile` = ifelse(is.nan(`% of Median SWE`), NaN, `Percentile`)
      )
    
  }

# Get historical data for all snotel sites
plan(multisession, workers = 10)

snotel_pasts <-
  snotelr::snotel_info() %>%
  tibble::as_tibble() %>%
  filter(state %in% c('MT', 'ID', 'OR', 'WA', 'WY', 'ND', 'SD'),
         start <= as.Date('2000-10-01')) %$%
  site_id %>%
  magrittr::set_names(.,.) %>%
  furrr::future_map_chr(
    ~get_snotel(.x, path = "data/snotel"),
    .options = furrr_options(seed = NULL)
  ) %>%
  furrr::future_map_dfr(
    ~(
      tryCatch(
        readr::read_csv(.x,
                        show_col_types = FALSE) %>%
          snotel_pasts(),
        error = function(e) NULL
      )
      
    ),
    .id = "site_id",
    .options = furrr_options(seed = NULL)
  ) %>%
  dplyr::ungroup()

pal <- function(x){x}

plan(sequential)

unlink("~/Desktop/current_snotel.fgb")
snotel_pasts %>%
  dplyr::mutate(fillColor = 
                  dplyr::case_when(
                    Percentile <= 2 ~ "#730000",
                    Percentile <= 5 ~ "#E60000",
                    Percentile <= 10 ~ "#FFAA00",
                    Percentile <= 20 ~ "#FCD37F",
                    Percentile <= 30 ~ "#FFFF00",
                    
                    Percentile >= 98 ~ "#303B83",
                    Percentile >= 95 ~ "#4030E3",
                    Percentile >= 90 ~ "#325CFE",
                    Percentile >= 80 ~ "#32E1FA",
                    Percentile >= 70 ~ "#82FCF9",
                    
                    Percentile > 30 & Percentile < 70 ~ "#FFFFFF",
                    
                    .default = "#FFFFFF00"
                  )) %>%
  dplyr::group_by(site_id) %>%
  dplyr::filter(!is.na(swe)) %>%
  dplyr::filter(date == max(date)) %>%
  dplyr::mutate(site_id = as.numeric(site_id)) %>%
  dplyr::left_join(snotelr::snotel_info() %>%
                     tibble::as_tibble()) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::select(site_id, fillColor) %>%
  sf::write_sf("~/Desktop/current_snotel.fgb")

# 
#   mapview::mapview(., zcol = "fillColor", col.regions = pal)
# 
# snotel_pasts %>%
#   dplyr::filter(site_id == 901) %>%
#   dplyr::filter(date == "2024-03-19")
# 
# viridis::inferno
# 
# 
# 
# 
# 
# 
# 
# snotel_pasts %>%
#   dplyr::filter(site_id == 901) %$%
#   Percentile %>%
#   plot(type = 'l',
#        ylim = c(0,100))
# 
# snotel_pasts %>%
#   dplyr::filter(site_id == 901) %$%
#   `% of Median SWE` %>%
#   lines(col = "red")
# 
# 
# snotelr::snotel_download(991, 
#                          internal = TRUE) %>%
#   tibble::as_tibble() %>%
#   calc_past_swe()
