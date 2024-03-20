get_snodas <-
  function(out_dir = file.path("data","snodas"),
           force.redo = FALSE){
    dir.create(out_dir,
               showWarnings = FALSE,
               recursive = TRUE)
    
    snodas_path <- "https://noaadata.apps.nsidc.org/NOAA/G02158/masked"
    
    snodas_files <-
      tibble::tibble(
        date = seq(lubridate::as_date("2003-09-30"), 
                   lubridate::today(), 
                   "1 day")
      ) %>%
      dplyr::mutate(year = lubridate::year(date),
                    month = lubridate::month(date),
                    month_abb = month.abb[month],
                    month = stringr::str_pad(month,2,pad = "0"),
                    day = lubridate::day(date),
                    day = stringr::str_pad(day,2,pad = "0"),
                    filename = paste0("SNODAS_", year, month, day, ".tar"),
                    path = file.path(snodas_path, 
                                     year, 
                                     paste(month, month_abb, sep = "_"), 
                                     filename),
                    outpath = file.path(out_dir, filename),
                    exists = file.exists(outpath)
      ) %>%
      rowwise() %>%
      dplyr::mutate(request = list(httr2::request(path))) %>%
      dplyr::ungroup()
    
    if(force.redo){
      snodas_files %$%
      httr2::req_perform_parallel(
        reqs = request,
        path = outpath,
        pool = curl::new_pool(host_con = 30),
        on_error = "continue",
        progress = TRUE
      )
    }else{
      snodas_files %>%
        dplyr::filter(!exists) %$%
        httr2::req_perform_parallel(
          reqs = request,
          path = outpath,
          pool = curl::new_pool(host_con = 30),
          on_error = "continue",
          progress = TRUE
        )
    }
    
    return(snodas_files)
  }
