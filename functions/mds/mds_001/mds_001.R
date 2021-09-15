


mds <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
 
  # mds: Medidas de Dispersion Simultaneas
   
  # Input originales 
  {
    ###
    
    input_originales <- list(input_decimales, input_cadena)
    names(input_originales) <- c("input_decimales", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Control 1 - input_base
  {
    ###
    # Vemos si todas las columnas de input_base son numericas
    
    vector1 <- rep(NA, ncol(input_base))
    
    for (n in 1:ncol(input_base)) {
      
      recorte <- input_base[n]
      vector1[n] <- control_1c(input_base = recorte, input_cadena = output_cadena)
      
    }
    veredicto1 <- sum(vector1) == length(vector1)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
    # Hacemos el control de input_decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    
    # Si no pasa el control de input_decimales, asignamos un nuevo valor
    # pero guardamos el original por si hace falta
    if (veredicto2 == FALSE){
      input_decimales_original <- input_decimales
      input_decimales <- 2
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto1) output_cadena <- veredicto2
    
    ###  
  } # Fin Control 2 - input_decimales
  ############################################################################
  
  
  # # # Modificaciones, Controles 3, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error mp: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      estas_columnas <- rep(1, ncol(input_data))
      mini <- as.data.frame(mtcars[estas_columnas])
      colnames(mini) <- "No Data"
      cat("Error mp: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
    

  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Medidas de Dispersion
  {
    ###
    
    # Todas las Medidas de Dispersion separadas
    {
    ###
   
      tablas_mds <- list()
      
      for (n in 1:ncol(mini)){
        recorte <- mini[n]
        tablas_mds[[n]] <- md(input_base = recorte, input_decimales = input_decimales, input_cadena = output_cadena)
      }  
      names(tablas_mds) <- paste0("var",c(1:ncol(mini)))
      
    ###    
    } # Fin Todas
    ###########################################
    
    
    
    
    # Tabla 1 
    {
      ###
      
      for (n in 1:length(tablas_mds)) {
        
        if (n == 1) {
          
          tabla1_mds <- tablas_mds[[n]]$md$tabla1_md
          
        } else {
        
          tabla1_mds <- rbind(tabla1_mds, tablas_mds[[n]]$md$tabla1_md)
        }
      } # Fin for n
       
   
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      for (n in 1:length(tablas_mds)) {
        
        if (n == 1) {
          
          tabla2_mds <- tablas_mds[[n]]$md$tabla2_md
          
        } else {
          
          tabla2_mds <- rbind(tabla2_mds, tablas_mds[[n]]$md$tabla2_md)
        }
      } # Fin for n
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
  
    
    
    ###
  } # Fin Medidas de Posicion
  ############################################################################
  
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_mds, tabla2_mds)
    names(mis_tablas) <- c("tabla1_mds", "tabla2_mds")
    
    ###  
  } # Fin Mis Tablas
  ###########################################################################
  
  
  # Cambios "NO DATA" o "Errores"
  {
    ###
    
    # Si no es valido trabajar... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin si no es valido trabajar...
    ########################################
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("mds", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function mp()
########################################################################################################





if (1 == 2) {
  
  
  input_base <- as.data.frame(mtcars[c(1,2)]) 
  input_decimales = 2
  input_cadena = TRUE
  # Ejemplo...
  
  SALE <-     mds(input_base = input_base, input_decimales = input_decimales, input_cadena = input_cadena)
  
  SALE$mds$tabla1_mds
  
  SALE$mds$tabla4_mds
}


