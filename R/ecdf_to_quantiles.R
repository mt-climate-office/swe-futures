ecdf_to_quantiles <-
  function(x){
    x %>%
      purrr::map(~quantile(.x, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))) %>%
      do.call(rbind, .) %>%
      magrittr::set_colnames(
        c("1%", "5%", "25%", "50%", "75%", "95%", "99%")
      ) %>%
      tibble::as_tibble()
  }