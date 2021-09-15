


df02_mod <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL,
                     input_tipo = NULL, input_marginal_fila = NULL, input_marginal_columna = NULL) {
  

    # Input originales 
    {
    ###
    
    input_originales <- list(input_decimales, input_cadena, input_tipo, input_marginal_fila,
                             input_marginal_columna)
    
    names(input_originales) <- c("input_decimales", "input_cadena", "input_tipo", "input_marginal_fila",
                                 "input_marginal_columna")
    
    ###    
    } # Fin Argumentos originales
    ##############################################################################
  
  
    # # # Funcionamiento por defecto
    {
    ###
    
    # Funcionamiento por defecto
    if (is.null(input_cadena)) input_cadena <- TRUE
  
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
  
    # Tipo por defecto
    if (is.null(input_tipo)) input_tipo <- "total"
  
    # Marginal fila
    if (is.null(input_marginal_fila)) input_marginal_fila <- "filas"
  
    # Marginal Columna
    if (is.null(input_marginal_columna)) input_marginal_columna <- "columnas"
  
    # Tipo por defecto
    if (is.null(input_tipo)) input_tipo <- "total"
  
    # Armamos la cadena
    output_cadena <- input_cadena
  
    ###
    } # Fin Funcionamiento
    ###########################################################
  
  
    # # # Controles 1
    {
    ###
    
    # 1) Que todos sean vectores...
    {
    ###  
    
        # Verificar que input_tipo sea un vector
        if(output_cadena && !is.vector(input_tipo)) {
        cat("Error df02_mod: input_tipo debe ser un vector", "\n")
        output_cadena <- FALSE
        }  
    
        # Verificar que input_marginal_fila sea un vector
        if(output_cadena && !is.vector(input_marginal_fila)) {
        cat("Error df02_mod: input_marginal_fila debe ser un vector", "\n")
        output_cadena <- FALSE
        }  
      
        # Verificar que input_marginal_columna sea un vector
        if(output_cadena && !is.vector(input_marginal_columna)) {
        cat("Error df02_mod: input_marginal_columna debe ser un vector", "\n")
        output_cadena <- FALSE
        }  
      
      
    ###
    } # Fin 1)
    #############################################
    
    
    # 2) Si todos son vectores...
    {
    ###
      
      if (output_cadena) {

        valido <- c("total", "filas", "columnas", "NO")
        union <- list(input_tipo, input_marginal_fila, input_marginal_columna)
        names(union) <- c("input_tipo", "input_marginal_fila", "input_marginal_columna")
    
      for (n in 1:length(union)) {
      
        este_input <- union[[n]]
      
        # Verificar que este_input tenga un solo elemento
        if(output_cadena && length(este_input) != 1) {
        cat(paste0("Error df02: ", names(union)[n], " debe tener solo un elemento", "\n"))
        output_cadena <- FALSE
        }  
    
        # Verificar que input_tipo sea valido
        if(output_cadena && sum(valido == este_input) != 1) {
        cat(paste0("Error df02: ", names(union)[n], " debe ser 'total', 'filas' o 'columnas'", "\n"))
        output_cadena <- FALSE
        }  
    
 
  
      } # Fin for n
      
      } #
      
    ###  
    } # Fin 2)
    ############################################
  
    ###
    } # Fin Controles 1
    ##################################
  
  
    # # # Control 2 y Tabla Combinada
    {
    ###
    
    # La funcion "df02" ya tiene dentro controles para las variables ingresadas.
      
    # Todas las tablas de doble entrada posibles...    
    TODO <- df02(input_base = input_base, input_decimales = input_decimales, 
                   input_cadena = output_cadena)
    
    TABLAS <- TODO$df02
    
    # Tabla que reune la informacion de los detalles de los argumentos indicados
    info <- as.data.frame(matrix(NA, 3, 3))
    info[,1] <- c("Cuerpo", "Marginal Fila", "Marginal Columna")
    info[,2] <- c("input_tipo", "input_marginal_fila", "input_marginal_columna")
    info[,3] <- c(input_tipo, input_marginal_fila, input_marginal_columna)
    colnames(info) <- c("Posición", "Argumento", "Detalle")
    
    # Las referencias de quien esta en filas y en columnas
    referencias <- TODO$referencias
    
    # La cadena
    output_cadena <- TODO$output_cadena  
    
    # # # Opciones de Tipo de Tabla
    {
    ###
    
      # Por defecto, la tabla sera la completa con los dos marginales
      # respecto al total.
      # Si hay que cambiar una tabla, solo hay que cambiar la de porcentajes, que
      # puede ser al total, por fila o por columna.
      # La tabla de FA es igual en los 3 casos.
      
      # Tabla FA con marginales
      tabla_fa <- TABLAS$`Al total`$FA
      tabla_p  <- TABLAS$`Al total`$`%%`
      
      # Correccion para tipo tabla por "filas"
      if (input_tipo == "filas") {
        
        tabla_p_mod <- TABLAS$`Por filas`$`%%`
        
        for (n in 1:nrow(tabla_p_mod)) tabla_p[n, ] <- tabla_p_mod[n,]
        
      }
      
      # Correccion para tipo tabla por "columnas"
      if (input_tipo == "columnas") {
        
        tabla_p_mod <- TABLAS$`Por columnas`$`%%`
        
        for (n in 1:ncol(tabla_p_mod)) tabla_p[,n ] <- tabla_p_mod[,n]
        
      }
      
        
    ###  
    }
    #######################################################
      
    # # # Opciones de marginales: "NO"
    {
  ###    
    
      # Si no quiere alguno de los marginales, serruchamos las tablas que tenemos
      
    if(input_marginal_fila == "NO"){
    
      tabla_fa <- tabla_fa[, -ncol(tabla_fa)]  
      tabla_p <- tabla_p[, -ncol(tabla_p)]
    }

    
    if(input_marginal_columna == "NO"){
      
      tabla_fa <- tabla_fa[-nrow(tabla_fa), ]  
      tabla_p  <- tabla_p[-nrow(tabla_p), ]
    }
    
  ### 
    }
    #########################################################
    
    
    # # # Opciones Marginales Fila
    {
  ###    
     # El marginal de filas, si lo pide por columnas, es el mismo que al total.
     # Lo separe en dos partes por las dudas este equivocado... pero... si te dicen
     # que quierne una tabla de porcentajes por filas, pero los totales por fila lo quieren
     # respecto al total de la columna que tiene los totales de las filas, el total del total
     # de filas es el "n" total de la muestra.
      
      
    if(input_marginal_fila == "filas") {
    
      tabla_fila_porc <- TABLAS$`Por filas`$`%%`
      recorte <- tabla_fila_porc[,ncol(tabla_fila_porc)]
      
      for (n in 1:length(recorte)) tabla_p[n,ncol(tabla_p)] <- recorte[n]
    } 
    
    
    if(input_marginal_fila == "columnas") {
      
      tabla_total_porc <- TABLAS$`Al total`$`%%`
      recorte <- tabla_total_porc[,ncol(tabla_fila_porc)]
      
      for (n in 1:length(recorte)) tabla_p[n,ncol(tabla_p)] <- recorte[n]
    }
    
    
    if(input_marginal_fila == "total") {
      
      tabla_total_porc <- TABLAS$`Al total`$`%%`
      recorte <- tabla_total_porc[,ncol(tabla_fila_porc)]
      
      for (n in 1:length(recorte)) tabla_p[n,ncol(tabla_p)] <- recorte[n]
    }
  
  ###
    } # Fin Opciones Fila
    ###########################################################
    
    
    # # # Opciones Marginales Columna
    {
      ###    
      # El marginal de columnas, si lo pide por respecto al total de fila, es el mismo que al total.
      # Lo separe en dos partes por las dudas este equivocado... pero... si te dicen
      # que quierne una tabla de porcentajes por filas, pero los totales por fila lo quieren
      # respecto al total de la columna que tiene los totales de las filas, el total del total
      # de filas es el "n" total de la muestra.
      
      
      if(input_marginal_columna == "columnas") {
        
        tabla_columna_porc <- TABLAS$`Por columnas`$`%%`
        recorte <- tabla_columna_porc[nrow(tabla_columna_porc),]
        
        for (n in 1:length(recorte)) tabla_p[nrow(tabla_p),n] <- recorte[n]
      } 
      
      
      if(input_marginal_columna == "filas") {
        
        tabla_total_porc <- TABLAS$`Al total`$`%%`
        recorte <- tabla_total_porc[nrow(tabla_total_porc),]
        
        for (n in 1:length(recorte)) tabla_p[nrow(tabla_p),n] <- recorte[n]
      }
      
      
      if(input_marginal_columna == "total") {
        
        tabla_total_porc <- TABLAS$`Al total`$`%%`
        recorte <- tabla_total_porc[nrow(tabla_total_porc),]
        
        for (n in 1:length(recorte)) tabla_p[nrow(tabla_p),n] <- recorte[n]
      }
      
      ###
    } # Fin Opciones Columna
    ###########################################################  
    
    
    # # # Tabla de Porcentajes sin simbolos
    {
    ###
    
      tabla_p2 <- as.data.frame(matrix(NA, nrow(tabla_p), ncol(tabla_p)))
      colnames(tabla_p2) <- colnames(tabla_p)
      rownames(tabla_p2) <- rownames(tabla_p)
      
      for (n in 1:ncol(tabla_p2)) for(k in 1:nrow(tabla_p2)) {
        
        esta_celda <- tabla_p[k,n]
        
        metralla <- strsplit(esta_celda, "%")[[1]]
        
        tabla_p2[k,n] <-  metralla
      } # Fin doble for
      
        
    ###  
    } # Fin Tabla de Porcentajes sin simbolos
    ###########################################################
    
    
    # # # Fusion
    {
    ###
      
      tabla_fusion <- tabla_p
      
      for(n in 1:ncol(tabla_fusion)) tabla_fusion[,n] <- paste0(tabla_fusion[,n], "(", tabla_fa[,n], ")")
        
    
      
    ###  
    } # Fin Fusion
    ###############################################

    ###  
    } # Fin Tabla Combinada
    #######################################   

  
    # # # Mis Tablas
    {
    ###
    
      mis_tablas <- list(tabla_fusion, tabla_fa, tabla_p, tabla_p2)
      names(mis_tablas) <- c("Fusion", "FA", "%", "%%")
      
    
    ###    
    } # Mis Tablas
    ##########################################
  
  
  
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
        
      } # Fin if
      
    ###   
    } # Fin Cambios "NO DATA" o "Errores"
    ##################################################
  
  
    # # # Salida
    {
    ###
      
      salida <- list(mis_tablas, referencias, info, output_cadena, input_originales)
      names(salida) <- c("df02_mod", "Referencias", "Informacion", "output_cadena", "input_originales")
      
      return(salida)
      
      
    ###  
    } # Fin Salida
    ##################
    
        
 
  

  
  
  
  
} # Fin function



if (2 == 1) {
  
  # # Ejemplo
  BASE <- mtcars[,c(2,8)]
  input_base <- BASE
  input_decimales <- 2
  input_marginal <- NULL
  input_cadena <- NULL
  input_tipo <- "total"
  input_marginal_fila <- "filas"
  input_marginal_columna <- "filas"
  
  
  AVER <-   df02_mod(input_base = input_base, input_decimales = input_decimales,
                     input_cadena = input_cadena,
                     input_tipo = input_tipo, 
                     input_marginal_fila = input_marginal_fila,
                     input_marginal_columna = input_marginal_columna)
  
  
  AVER$df02_mod$Fusion
  
}