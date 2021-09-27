


library(shiny)
library(shinyjs)
library(bslib)
library(readxl)
library(datasets)
library(DT)
library(htmltools)
library(openxlsx)

# library(shinydashboard)



num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}

let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}


#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

OpcionesDeColumnas <- function(my_names = ""){
  
  # Letras
  letras_elegidas <- paste0("(", num2let(c(1:length(my_names))), ")")
  
  # Visual del usuario
  visual_usuario <- paste0(letras_elegidas, " - ", my_names)

  
  # Armamos el vector de salida
  vector_salida <- my_names
  names(vector_salida) <- visual_usuario
  
  return(vector_salida)
}


AllEyesOnMe <- function(ListBase = NULL, the_col = NULL) {
  
  
  dt_ok <- FALSE
  
  if(!is.null(ListBase))
    if(!is.null(the_col))
      if(the_col != "")
        if(sum(colnames(ListBase[[1]]) == the_col) > 0)
          dt_ok <- TRUE
  
  
  dt_ok
}

MyLetter <- function(ListBase = NULL, the_col = NULL) {
  
  
  dt_ok <- FALSE
  
  if(!is.null(ListBase))
    if(!is.null(the_col))
      if(the_col != "")
        if(sum(colnames(ListBase[[1]]) == the_col) > 0)
          dt_col <- colnames(ListBase[[1]]) == the_col
          pos_col <- c(1:length(dt_col))
          the_col <- pos_col[dt_col]
          my_letter <- num2let(the_col)
          
  my_letter
}

ModifyMe <- function(the_text = NULL, end_var = NULL){
  
  
  
  internal_text <- '
  the_text <- gsub("_THE_LISTBASE_", "BaseSalida()", the_text)
  the_text <- gsub("_MY_BASE_", "BaseSalida()[[1]]", the_text)
  the_text <- gsub("_MENU1_", "input$menu1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_VAR1_", "var1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_VAR2_", "var2_control", the_text)
  the_text <- gsub("_INPUT_VAR1_", "input$var1_control", the_text)
  the_text <- gsub("_INPUT_VAR2_", "input$var2_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_TIPO_VAR1_", "tipo_var1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_TIPO_VAR2_", "tipo_var2_control", the_text)
  '
  if (!is.null(end_var)) internal_text <- gsub("_control", end_var, internal_text)
  eval(parse(text = internal_text))
  
  return(the_text)
  
}

##########

MyDate <- function(){
  
  armado <- Sys.time()
  armado <- gsub("-" ,"_" , armado)
  armado <- gsub(" " ,"__" , armado)
  armado <- gsub(":" ,"_" , armado)
  
  armado 
  
}

##################################################################

df01 <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
  # # # Input originales 
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
    veredicto1 <- control_1q(input_base = input_base, input_cadena = output_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  # # # Control 2 - input_decimales
  {
    ###
    
    # Hacemos el control de input_decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
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
  
  
  # # # Modificaciones y Controles 3, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta que sea de tipo factor.
      # Si es factor... la dejamos como está...
      # Sino, al obligamos...
      if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error df01: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[2])
      mini[,1] <- as.factor(as.character(mini[,1]))
      colnames(mini) <- "No Data"
      cat("Error df01: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Modificaciones y Controles 3, y base "NO DATA"
  ################################################################
  
  
  
  # # # Resolucion
  {
    ###
    
    
    # Tabla 1: Distribucion de Frecuencias
    {
      ###
      
      # Frecuencais Absolutas
      fa <- table(mini)
      
      # Total Absoluto
      n_total <- sum(fa)
      grupos <- as.character(names(fa))
      salida_n_total <- rep(n_total, length(fa))
      
      # Cociente
      cociente <- paste0(fa, "/", n_total)
      
      # Frecuencias Relativas
      fr <- round2((fa/n_total), input_decimales)
      if (n_total == 0) fr <- rep(0, length(grupos))
      
      
      # Porcentajes
      porcentaje <- fr*100
      
      # Porcentaje2
      porcentaje2 <- paste(porcentaje, "%", sep="")
      
      # Frec. Abs. Acumuladas
      faa <- cumsum(fa)
      
      # Frec. Rel. Acumuladas
      fra <- cumsum(fr)
      
      # Cociente Acumulado
      cocientea <- paste0(faa, "/", n_total)
      
      # Porcentajes Acumulados
      pa <- fra*100
      
      # Porcentajes Acumulados 2
      pa2 <- paste(pa, "%", sep="")
      
      # Tabla de Distribucion de Frecuencias
      tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  porcentaje, porcentaje2, faa, fra, cocientea, pa, pa2)
      tabla01 <- as.data.frame(tabla01)
      nombres_tabla01 <- c(colnames(mini), "FA", "Total", "Cociente", "FR", "%", "%%", "FAA", "FRA", "CocienteAcum", "%Acum", "%%Acum")
      colnames(tabla01) <- nombres_tabla01
      
      numericas <- c(2, 3, 5, 6, 8, 9, 11)
      for(n in 1:length(numericas)) tabla01[,numericas[n]] <- as.numeric(as.character(tabla01[,numericas[n]])) 
      
      
      
      ###
    } # Fin Tabla 1
    ##################################
    
    
    # Tabla 2: Intervalos de Confianza
    {
      ###
      
      p <- fr
      q <- rep(NA, length(p))
      for (n in 1:length(q)) q[n] <- sum(p[-n])
      n <- n_total  
      
      alfa <- c(0.10, 0.05, 0.01)
      
      
      tablas2_ic <- list()
      
      for (k in 1:length(alfa)) {
        
        este_alfa <- alfa[k]
        este_alfa_2 <- este_alfa/2
        confianza <- paste0((1 - este_alfa)*100, "%")
        
        Z <- qnorm(1-este_alfa_2)
        Z <- round2(Z, input_decimales)
        
        desvio <- Z*sqrt((p*q)/n)
        desvio <- round2(desvio, input_decimales)
        
        li_prop <- p - desvio
        ls_prop <- p + desvio
        
        dt_li <- li_prop < 0
        dt_ls <- ls_prop > 1
        
        if (sum(dt_li) > 0) li_prop[dt_li] <- 0
        if (sum(dt_ls) > 0) ls_prop[dt_ls] <- 1
        
        
        li_porcentaje <- paste0((li_prop*100), "%")
        ls_porcentaje <- paste0((ls_prop*100), "%")
        
        
        tabla02 <- cbind(grupos, porcentaje2, li_porcentaje, ls_porcentaje)
        colnames(tabla02) <- c(colnames(mini), "%", paste0("LI", confianza), paste0("LS", confianza))
        tabla02 <- as.data.frame(tabla02)
        rownames(tabla02) <- rownames(tabla01)
        
        
        if (output_cadena == FALSE) {
          
          estas_dimensiones <- dim(tabla02)
          tabla_no_data02 <- as.data.frame(matrix("Sin datos en la Columna", estas_dimensiones[1], estas_dimensiones[2]))
          colnames(tabla_no_data02) <- colnames(tabla02)
          tabla02 <- tabla_no_data02
          
        }
        
        tablas2_ic[[k]] <- tabla02
        
      }
      
      
      
      ###
    } # Fin Tabla 2
    ##################################
    
    
    # Tabla 3: Fusion
    {
      ###
      fusion <- paste0(porcentaje2, "(", fa, ")")
      
      tabla03 <- cbind(grupos, fusion)
      tabla03 <- as.data.frame(tabla03)
      nombres_tabla03 <- c(colnames(mini), "%(FA)")
      colnames(tabla03) <- nombres_tabla03
      
      ###  
    } # Fin Tabla 3
    ##################################
    
    # Tabla 4: Recorte de la Tabla 1
    {
      ###
      estas_columnas1 <- c(1,2,3,4,5,7)
      tabla04 <- tabla01[, estas_columnas1]
      
      estas_columnas2 <- c(2,5,6)
      colnames(tabla04)[estas_columnas2] <- c("Frecuencia Absoluta", "Frecuencia Relativa", "%")
      ###  
    } # Fin Tabla 4
    ########################################
    
    
    
    ###  
  } # Fin Resolucion
  ################################################################
  
  
  # Mis Tablas
  {
    ###
    
    # Reunimos las tablas
    mis_tablas <- list(tabla01, tablas2_ic, tabla03, tabla04)
    names(mis_tablas) <- c("DF", "IC", "Fusion", "DF2")
    
    ###    
  }
  ##########################################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      cambio1 <- "Sin datos en la Columna"
      cambio2 <- "Modificar input_decimales"
      
      # Damos aviso si es algo de los datos o los decimales
      cambio_aplicado <- cambio1
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        
        if (is.list(esta_tabla)) next
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin if si hay errores...
    #############################################################
    
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    
    names(salida) <- c("df01", "output_cadena", "input_originales")
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
  
  
} # Fin function

##################################################


control_1q <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Explicacion
  {
    ###
    # La funcion "control_1q()" es una funcion de control sobre input_base.
    # Verifica que input_base sea un dataframe de solo una columna, con al menos una fila.
    # Otorgara avisos si el objeto input_base:
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    # Aclaramos que el punto 5 es ver las filas que tiene input_base, y no 
    # ver si las filas ingresadas son vacias o no.
    
    
    # Si se cumple todo, la funcion devuelve un "TRUE". 
    # Si al menos un detalle falta, devuelve un "FALSE".
    # El control se realiza si el objeto input_cadena es TRUE.
    # Si input_cadena es FALSE quiere decir que ya hay un paso previo de una funcion
    # externa que no se cumple, y por lo tanto carece de sentido realizar las tareas, devolviendo
    # directamente un FALSE control_1q() como resultado.
    
    ###    
  } # Fin Explicacion
  ############################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("Error control_1q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_1q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_1q: input_base no tiene columnas.")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("Error control_1q: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_1q: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


################


control_decimales <- function(input_decimales = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles  - input_decimales
  {
    ###
    
    # Verificar que input_decimales es un vector
    if(output_cadena && !is.vector(input_decimales)) {
      cat("Error input_decimales: input_decimales debe ser un vector", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un solo elemento
    if(output_cadena && length(input_decimales) > 1) {
      cat("Error input_decimales: input_decimales debe ser un solo número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales es un numero
    if(output_cadena && is.numeric(input_decimales) == FALSE) {
      cat("Error input_decimales: input_decimales debe ser un número", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar que input_decimales no es "NA"
    if(output_cadena && is.na(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NA'", "\n")
      output_cadena <- FALSE
    }
    
    
    # Verificar que input_decimales no es "NaN"
    if(output_cadena && is.nan(input_decimales)) {
      cat("Error input_decimales: input_decimales no debe ser 'NaN'", "\n")
      output_cadena <- FALSE
    }
    
    ###      
  } # Fin Controles 
  ###################################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}



#########

round2 <- function(x, n) { 
  posneg <- sign(x) 
  
  z <- abs(x)*10^n 
  z <- z + 0.5 
  z <- trunc(z) 
  z <- z/10^n 
  z*posneg 
} 
#############


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


############


control_1c <- function(input_base = NULL, input_cadena = NULL){
  
  # # # Funcionamiento por defecto
  {
    ###
    
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ################################################
  
  
  # # # Controles 1
  {
    ###
    
    # Verificar que input_base no sea nulo
    if(output_cadena && is.null(input_base)) {
      cat("\n", "Error control_1c: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("\n", "Error control_1c: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 1 columna
    if(output_cadena && ncol(input_base) > 1) {
      cat("\n", "Error control_1c: input_base debe ser solo una columna")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      cat("\n", "Error control_1c: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar que sea numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      cat("\n", "Error control_1c: input_base debe ser numérica.", "\n", "Utilice la solapa 'Control' sobre esta variable.", "\n")
      output_cadena <- FALSE
    }
    
    
    ###      
  } # Fin Controles 1
  ###################################################################
  
  # # # Salida
  {
    ###
    
    salida <- output_cadena
    
    names(salida) <- c("output_cadena")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
}


##############

md <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
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
    
    # Hacemos el control de decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
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
    } # Fin si algo no esta OK
    ### #####################################################
    
    mini_vector <- mini[,1]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Medidas de Dispersion
  {
    ###
    
    # Tabla 1 
    {
      ###
      
      
      nombres_elementos <- c("Variable", "Varianza", "Desvío Estándard", "Error Estándard", "Coeficiente de Variación", "n")
      
      tabla1_md <- as.data.frame(matrix(NA,1, length(nombres_elementos)))
      colnames(tabla1_md) <- nombres_elementos
      
      
      # Varianza
      varianza <- var(mini_vector)
      varianza <- round2(varianza, input_decimales)
      
      # Desvio
      desvio <- sd(mini_vector)
      
      # Tamanio muestral
      n_muestra <- length(mini_vector)
      
      # Error estandard
      ee <- desvio/sqrt(n_muestra)
      
      # Media
      media <- mean(mini_vector)
      
      # Coeficiente de Variacion
      cv <- desvio/media
      
      
      # Primero sacamos el desvio...
      # Sin redondear el desvio, lo usamos para sacar el error estandard...
      # Y luego redondeamos los dos.
      # Esto es para sacar mejor al EE... por que sino sacas el DE... lo redondeas...
      # lo usas para sacar el EE y lo volves a redondear.
      # Lo mismo con el CV.
      
      desvio <- round2(desvio, input_decimales)
      ee <- round2(ee, input_decimales)
      cv <- round2(cv, input_decimales)
      
      
      
      tabla1_md[,1] <- colnames(mini)
      tabla1_md[,2] <- varianza
      tabla1_md[,3] <- desvio
      tabla1_md[,4] <- ee
      tabla1_md[,5] <- cv
      tabla1_md[,6] <- n_muestra
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      celdas_vacias <- nrow(input_base) - nrow(mini)
      
      tabla2_md <- as.data.frame(matrix(NA, 1, (ncol(tabla1_md) + 1)))
      
      for (n in 1:ncol(tabla1_md)) tabla2_md[1,n] <- tabla1_md[1,n]
      
      colnames(tabla2_md) <- c(colnames(tabla1_md), "Celdas Vacías")
      tabla2_md[ncol(tabla2_md)] <- celdas_vacias
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    ###
  } # Fin Medidas de Posicion
  ############################################################################
  
  
  # # # Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_md, tabla2_md)
    names(mis_tablas) <- c("tabla1_md", "tabla2_md")
    
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
      
    }
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("md", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function md()
########################################################################################################

############



percentiles <- function(input_base=NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
  
  
  # Input originales 
  {
    ###
    
    input_originales <- list(input_busqueda, input_decimales, input_cadena)
    names(input_originales) <- c("input_busqueda", "input_decimales", "input_cadena")
    
    ###    
  } # Fin Argumentos originales
  ##############################################################################
  
  
  # # # Funcionamiento por defecto
  {
    ###
    
    # Busqueda por defecto
    if (is.null(input_cadena)) input_busqueda = c(5, 10, 90, 95)
    
    # Cadena por defecto
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
    
    # Hacemos el control de decimales
    veredicto2 <- control_decimales(input_decimales = input_decimales, input_cadena = output_cadena)
    
    # Si no pasa el control numerico, asignamos un nuevo valor para input_decimales
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
  
  
  # # # Control 3 - input_busqueda
  {
    ###
    
    # Hacemos el control de input_busqueda
    veredicto3 <- TRUE
    
    # Si estuvo bien el control anterior... Seguimos...    
    if (veredicto2 == TRUE){
      
      # Verificar que input_busqueda no sea nulo
      if(veredicto3 && is.null(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda no debe ser nulo", "\n")
        veredicto3 <- FALSE
      }  
      
      # Verificar que input_busqueda sea vector
      if(output_cadena && !is.vector(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda debe ser un vector'", "\n")
        veredicto3 <- FALSE
      }
      
      
      # Verificar que input_busqueda debe ser numerico
      if(output_cadena && !is.numeric(input_busqueda)) {
        cat("\n", "Error percentiles: input_busqueda debe ser numérico'", "\n")
        veredicto3 <- FALSE
      }
      
      
      # Verificar que input_busqueda tenga elementos
      if(output_cadena && length(input_busqueda) == 0) {
        cat("\n", "Error percentiles: input_busqueda debe tener al menos un elemento'", "\n")
        veredicto3 <- FALSE
      }
      
    }
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto2) output_cadena <- veredicto3
    
    
    # Si paso el control anterior... guardamos esta nueva opinion
    if (veredicto2) output_cadena <- veredicto3
    
    ###  
  } # Fin Control 3 - input_busqueda
  ############################################################################
  
  
  # # # Modificaciones, Controles 4, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error percentiles: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[1])
      colnames(mini) <- "No Data"
      cat("Error mp: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    ### #####################################################
    
    mini_vector <- mini[,1]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  
  # # # Percentiles
  {
    ###
    
    # Si tiene al menos un dato... y es numerica
    nombres_columnas <- c("Variable", paste0(input_busqueda, "%"), "n")
    tabla_percentiles <- as.data.frame(matrix(NA, 1, length(nombres_columnas)))
    colnames(tabla_percentiles) <- nombres_columnas
    
    percentiles <- quantile(mini_vector,  probs = input_busqueda/100)
    
    tabla_percentiles[1,1] <- colnames(input_base)
    tabla_percentiles[1, c(2:(ncol(tabla_percentiles)-1))] <- percentiles
    tabla_percentiles[1,ncol(tabla_percentiles)] <- length(mini_vector)
    
    
    ###
  } # Fin Percentiles
  ##############################################################
  
  
  
  # # # Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla_percentiles)
    names(mis_tablas) <- c("tabla_percentiles")
    
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
      cambio3 <- "Modificar input_busqueda"
      
      # Damos aviso si es algo de los datos o los decimales
      
      # Por defecto, el cambio indicado es el cambio1
      cambio_aplicado <- cambio1
      
      # Si esta bien el 1, y esta mal el dos...
      if (veredicto1 == TRUE && veredicto2 == FALSE) cambio_aplicado <- cambio2
      
      # Si esta bien el 2, y esta mal el 3...
      if (veredicto2 == TRUE && veredicto3 == FALSE) cambio_aplicado <- cambio3
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        
        mis_tablas[[n]]  <- esta_tabla
        
      } # Fin for n
      
    }
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("percentiles", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################   
  
  
  
} # Fin CUANTILES

