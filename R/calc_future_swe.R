calc_future_swe <-
  function(
    x = 
      snotelr::snotel_download(901, 
                               internal = TRUE),
    ...){
    
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
        ) %>%
      dplyr::mutate(swe = ecdf_to_quantiles(swe)) %>%
      tidyr::unnest(swe)
    
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
    
    future <-
      x %>%
      model_delta_swe() %>%
      simulate_delta_swe() %>%
      dplyr::filter(date >= latest_date) %>%
      dplyr::group_by(run) %>%
      dplyr::mutate(
        swe = 
          calc_swe_from_delta_swe(delta = delta_swe, 
                                  start_swe = latest_swe)
      ) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(
        swe = 
          list(
            ecdf(swe)
          )
      ) %>%
      dplyr::mutate(swe = ecdf_to_quantiles(swe)) %>%
      tidyr::unnest(swe)
    
    return(
      dplyr::lst(
        station,
        latest_date,
        past_years,
        past,
        present,
        future
      )
    )
  }
