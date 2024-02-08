ecdf_to_quantiles <-
  function(x){
    x %>%
      purrr::map(~quantile(.x, c(0, 0.02, 0.05, 0.1, 0.2, 0.25, 0.3, 0.5, 0.7, 0.75, 0.8, 0.9, 0.95, 0.98, 1))) %>%
      do.call(rbind, .) %>%
      magrittr::set_colnames(
        c("Minimum", "2%", "5%", "10%", "20%", "25%", "30%", "50%", "70%", "75%", "80%", "90%", "95%", "98%", "Maximum")
      ) %>%
      tibble::as_tibble()
  }
