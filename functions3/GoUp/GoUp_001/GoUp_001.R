#helper function (convert vector to named list)
GoUp <- function (up=NULL){
  
  up <- "data"
  step <- "../"
  
  candado <- FALSE
  contador_candado <- 0
  
  while(candado == FALSE) {

        
    contador_candado <- contador_candado + 1
    
    ja <- rep(step, contador_candado)
    
    for (p in 1:length(ja)) {
      
      if (p == 1) look <- ja[1]
      if (p > 1) look <- paste0(look, ja[p])
    }
    
    
    my_files <- list.files(look)
    
    dt <- my_files == up
    
    if (sum(dt) == 1 | contador_candado == 15) candado <- TRUE
    
  }
  
  my_way <- paste0(look, up)
  return(my_way)
}



