simulate_delta_swe <-
  function(x,
           n = 99){
    
    (1:n) %>%
      magrittr::set_names(.,.) %>%
      purrr::map_dfr(
        ~(
          x %>%
            dplyr::rowwise() %>%
            dplyr::transmute(
              date = date,
              Event = 
                sample(
                  x = c("-", "0", "+"), 
                  size = 1,
                  prob = c(`p(-)`, `p(0)`, `p(+)`)
                ),
              `-` = 
                -1 * 
                suppressWarnings(
                  rgamma(n = 1, 
                         shape = `gamma(-)`['alpha'], 
                         scale = `gamma(-)`['beta'])
                ),
              `+` = 
                suppressWarnings(
                  rgamma(n = 1, 
                         shape = `gamma(+)`['alpha'], 
                         scale = `gamma(+)`['beta'])
                )
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              delta_swe = 
                dplyr::case_when(
                  Event == "0" ~ 0,
                  Event == "-" ~ `-`,
                  Event == "+" ~ `+`
                )
            )
        ),
        .id = "run"
      )
  }