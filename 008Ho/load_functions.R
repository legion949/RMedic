
#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

'
EVA01 <- function(input_direccion=NULL){
  
  esta_direccion <- input_direccion
  
  FUNCIONES <- list.files(esta_direccion)
  
  
  for (n in 1:length(FUNCIONES)) {
    
    
    DIR_FUNCION <- paste(esta_direccion, "/", FUNCIONES[n], sep="")
    
    dentro <- list.files(DIR_FUNCION)
    dentro <- dentro[length(dentro)]
    carga_final <- paste(DIR_FUNCION, "/", dentro, "/", dentro, ".R", sep="")
    
    source(carga_final)
    
  } # Fin for n
  
  
  
} # Fin function
'
library('stringr')

EVA01 <- function(root){
    stopifnot(root != NULL)
    root_content <- list.files(root)
    
    for (n in 1:length(root_content)) {
        fst_level <- paste(root, "/", root_content[n], sep="")
        fst_level_content <- list.files(fst_level)
        
        for (i in 1:length(fst_level_content)){
            snd_level <- paste(fst_level, "/", fst_level_content[i], sep="")
            snd_level_content <- list.files(snd_level)
            
            for (j in 1:length(snd_level_content)) {
                if ( any(grepl('.*\\.R$', snd_level_content[j]))) {
                    final_path <- paste(snd_level, "/", snd_level_content[j], sep="")
                    source(final_path)
                }
            }
        }
        
    }
}



                                        #aqui <- paste(getwd(), "/FUNCIONES", sep="")
aqui = "../FUNCIONES"
EVA01(aqui)
getwd()


###########
