model_delta_swe <-
  function(x,
           roll_width = 6,
           current_year = FALSE,
           climatology_length = 30,
           snow_year_start_month = 9
  ){
    
    x %<>%
      tibble::as_tibble() %>%
      dplyr::transmute(date = lubridate::as_date(date),
                       snow_year = 
                         ifelse(lubridate::month(date) >= snow_year_start_month, 
                                lubridate::year(date) + 1, 
                                lubridate::year(date)),
                       swe = snow_water_equivalent) %>%
      na.omit()
    
    current_snow_year <-
      max(x$snow_year)
    
    if(!is.null(climatology_length))
      x %<>%
      dplyr::filter(
        snow_year >= (current_snow_year - climatology_length)
      )
    
    if(!current_year)
      x %<>%
      dplyr::filter(
        snow_year < current_snow_year
      )
    
    x %<>%
      dplyr::group_by(snow_year) %>%
      dplyr::mutate(
        daily_delta = 
          c(diff(swe), 0),
        delta_time =
          date - lubridate::as_date(
            paste0(
              lubridate::year(
                min(date)
              ), 
              "-09-01")
          )
      ) %>%
      dplyr::ungroup() %>%
      na.omit()
    
    roll_date <-
      x %>%
      dplyr::select(date) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        groups = list(
          tibble::tibble(
            groups = lubridate::as_date(
              (date - roll_width):(date + roll_width)
            )
          )
        )
      ) %>%
      tidyr::unnest(groups)
    
    x %>%
      dplyr::select(date, delta_time) %>%
      dplyr::left_join(roll_date) %>%
      dplyr::rename(date = groups,
                    group = date) %>%
      dplyr::left_join(x %>%
                         dplyr::select(date, swe, snow_year, daily_delta)) %>%
      dplyr::arrange(delta_time) %>%
      dplyr::group_by(delta_time) %>%
      dplyr::summarise(
        `p(SWE)` = sum(swe > 0, na.rm = TRUE) /n(),
        
        `p(-)` = sum(daily_delta[swe > 0] < 0, na.rm = TRUE) / sum(swe > 0, na.rm = TRUE),
        `p(+)` = sum(daily_delta > 0, na.rm = TRUE) / n(),
        
        `gamma(+)` = list(calc_gam(daily_delta[daily_delta > 0])),
        `gamma(-)` = list(calc_gam(-1 * daily_delta[daily_delta < 0 & (swe + daily_delta) != 0])),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        `p(0)` = 1 - (`p(-)` + `p(+)`),
        `p(-)` = ifelse(`p(SWE)` < 0.01, 1, `p(-)`),
        `p(0)` = ifelse(`p(SWE)` < 0.01, 0, `p(0)`),
        `p(+)` = ifelse(`p(SWE)` < 0.01, 0, `p(+)`),
        `p(-)` = ifelse(`p(-)` < 0, 0, `p(-)`),
        `p(0)` = ifelse(`p(0)` < 0, 0, `p(0)`),
        `p(+)` = ifelse(`p(+)` < 0, 0, `p(+)`)
      ) %>%
      dplyr::transmute(
        date =
          delta_time + 
          lubridate::as_date(
            paste(current_snow_year - 1,snow_year_start_month,1, sep = "-")
          ), 
        `p(SWE)`, 
        `p(-)`, 
        `p(0)`, 
        `p(+)`, 
        `gamma(-)`, 
        `gamma(+)`
      ) 
    
  }
