calc_gam <-
  function(x){
    
    tryCatch(
      suppressWarnings(
        x %>%
          lmom::samlmu() %>%
          lmom::pelgam()
      ),
      error = 
        function(e){
          as.numeric(NA)
        }
    )
  }