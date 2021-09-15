


mp <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
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
        veredicto1 <- control_1c(input_base = input_base, input_cadena = output_cadena)
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
        mini <- as.data.frame(mtcars[1])
        colnames(mini) <- "No Data"
        cat("Error mp: 'mini' externo agregado (NO DATA)", "\n")
      }
      ### 
      
      mini_vector <- mini[,1]
      
  } # Fin Modificaciones y Controles 2
    ############################################################################
  
  
    # # # Medidas de Posicion
    {
    ###
      
      # Tabla 1 
      {
      ###
        
      
      nombres_elementos <- c("Variable", "Mínimo", "Media", "Mediana", "Máximo", "n")
      
      tabla1_mp <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
      colnames(tabla1_mp) <- nombres_elementos
      
     
      
      minimo <- min(mini_vector)
      minimo <- round2(minimo, input_decimales)
      
      # Media
      media <- mean(mini_vector)
      media <- round2(media, input_decimales)
      
      # Mediana
      mediana <- median(mini_vector)
      mediana <- round2(mediana, input_decimales)
      
      
      # Maximo
      maximo <- max(mini_vector)
      maximo <- round2(maximo, input_decimales)
      
      # Cantidad de Datos
      n_muestra <- length(mini_vector)
      
      
      tabla1_mp[,1] <- colnames(mini)
      tabla1_mp[,2] <- minimo
      tabla1_mp[,3] <- media
      tabla1_mp[,4] <- mediana
      tabla1_mp[,5] <- maximo
      tabla1_mp[,6] <- n_muestra
      
      ###
      } # Fin Tabla 1
      ############################################################
      
      
      # Tabla 2
      {
      ###
        
        celdas_vacias <- nrow(input_base) - nrow(mini)
        
        tabla2_mp <- as.data.frame(matrix(NA, 1, (ncol(tabla1_mp) + 1)))
        
        for (n in 1:ncol(tabla1_mp)) tabla2_mp[1,n] <- tabla1_mp[1,n]
        
        colnames(tabla2_mp) <- c(colnames(tabla1_mp), "Celdas Vacías")
        tabla2_mp[ncol(tabla2_mp)] <- celdas_vacias
        
      ###  
      } # Fin Tabla 2
      ############################################################
  
      
      # Tabla 3: Intervalos de Confianza
      {
      ###
        
           
          alfa <- c(0.10, 0.05, 0.01)
          nombres_columnas <- c("Variable", "Media", "Confianza", "Límite Inferior IC", "Límite Superior IC", "n")
          tabla_ic <- as.data.frame(matrix(NA, length(alfa), length(nombres_columnas)))
          colnames(tabla_ic) <- nombres_columnas
        
          for (n in 1:nrow(tabla_ic)) {
            
            este_alfa <- alfa[n]
            este_alfa_2 <- este_alfa/2
            gl <- n_muestra - 1
        
        
            desvio <- sd(mini_vector)
            desvio <- round2(desvio, input_decimales)
        
            t_li <- qt(este_alfa_2, df = gl, lower.tail = TRUE)
            t_ld <- qt((1-este_alfa_2), df = gl, lower.tail = TRUE)
        
            brazo <- t_ld*(desvio/sqrt(n_muestra))
            brazo <- round2(brazo, input_decimales)
        
            media_li <- media - brazo
            media_ld <- media + brazo
        
            tabla_ic[n, 1] <- colnames(input_base)
            tabla_ic[n, 2] <- media
            tabla_ic[n, 3] <- paste0((1-este_alfa)*100, "%")
            tabla_ic[n, 4] <- media_li
            tabla_ic[n, 5] <- media_ld
            tabla_ic[n, 6] <- n_muestra
            
          } # Fin for n
          
          tabla3_mp <- tabla_ic
     
          
          
          
          
      ###  
      } # Fin Tabla 3
      #############################################################
    
      
   
      
    ###
    } # Fin Medidas de Posicion
    ############################################################################
  
  
  
    # # #Mis Tablas
    {
    ###
      
      mis_tablas <- list(tabla1_mp, tabla2_mp, tabla3_mp)
      names(mis_tablas) <- c("tabla1_mp", "tabla2_mp", "tabla3_mp")
      
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
      names(salida) <- c("mp", "output_cadena", "input_originales")
      
      return(salida)
      
    ###  
    } # Salida
    ############################################################################
 

  
  
  
} # Fin function mp()
########################################################################################################





if (1 == 2) {
  
 
  input_base <- as.data.frame(mtcars[1]) 
  input_decimales = 2
  input_cadena = TRUE
  # Ejemplo...
  
  SALE <-     mp(input_base = input_base, input_decimales = input_decimales, input_cadena = input_cadena)

  SALE$mp$tabla1_mp
  
SALE$mp$tabla4_mp
}


