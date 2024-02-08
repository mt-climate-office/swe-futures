calc_swe_from_delta_swe <-
  function(delta, start_swe = 0){
    swe <- 
      rep(NA, length(delta))
    swe[1] <- start_swe
    
    delta[is.nan(delta)] <- 0
    
    for(i in 1:(length(delta) - 1)){
      swe[i + 1] <- max(0, swe[i] + delta[i])
    }
    
    swe[is.nan(swe)] <- 0
    
    return(swe)
  }
  