calc_past_swe <-
  function(
    x = 
      snotelr::snotel_download(901, 
                               internal = TRUE)){
    
    x %>%
      tibble::as_tibble() %>%
      dplyr::transmute(date = lubridate::as_date(date),
                       snow_year = 
                         ifelse(lubridate::month(date) >= 9, 
                                lubridate::year(date) + 1, 
                                lubridate::year(date)),
                       swe = snow_water_equivalent) %>%
      na.omit() %>%
      dplyr::filter(snow_year != current_snow_year) %>%
      dplyr::group_by(snow_year) %>%
      ## Some snotel sites only had monthly values for the first several years of their records.
      ## Drop those years.
      dplyr::filter(n() > 12) %>%
      dplyr::mutate(delta_time = date - lubridate::as_date(paste0(snow_year, "-09-01"))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        date =
          delta_time + 
          lubridate::as_date(
            paste(current_snow_year,9,1, sep = "-")
          )
      ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        swe = 
          list(
            ecdf(swe)
          )
      )
    
  }
