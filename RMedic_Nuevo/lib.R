


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


#################

paso_detalle <- function(detalle) {
  
  
  if (!is.null(detalle)) {
    if (detalle != "") {
      
      
      paso <- TRUE
      
      
    } else paso <- FALSE
  } else paso <- FALSE
}


###################


paso_BASE <- function(BASE) {
  
  
  if (!is.null(BASE)) {
    if (ncol(BASE) > 0) {
      
      paso <- TRUE
      
    } else paso <- FALSE
  } else paso <- FALSE
}

###############



df02 <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  
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
    
    # Funcionamiento por defecto
    if (is.null(input_cadena)) input_cadena <- TRUE
    
    # Decimales por defecto
    if (is.null(input_decimales)) input_decimales <- 2
    
    
    # Armamos el siguiente eslabon
    output_cadena <- input_cadena
    
    ###
  } # Fin Funcionamiento
  ##########################################################
  
  
  # # # Control 1 - input_base
  {
    ###
    veredicto1 <- control_2q(input_base = input_base, input_cadena = output_cadena)
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
  
  
  # # # Modificaciones, Controles 2, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta cada columna sea de tipo factor.
      if (is.factor(input_base[,1]) == FALSE) input_base[,1] <- as.factor(as.character(input_base[,1]))
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error df02: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ###########################
    
    # Si no hay datos o tienen errores...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(8,2)])
      mini[,1] <- as.factor(as.character(mini[,1]))
      mini[,2] <- as.factor(as.character(mini[,2]))
      colnames(mini) <- "No Data"
      cat("Error df02: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
  } # Fin Modificaciones
  ################################################################    
  
  
  # # # Resolucion
  {
    ###
    
    # 1) Marginales al total
    {
      ###
      
      
      
      # Detalles Iniciales y fa
      fa <- table(mini)
      n_total <- sum(fa)
      fa_interno <- fa
      m_col <- colSums(fa)
      m_row <- rowSums(fa)
      
      # fa marginal
      fa_marginal <- cbind(fa, m_row)
      fa_marginal <- rbind(fa_marginal, c(m_col, n_total))
      colnames(fa_marginal)[ncol(fa_marginal)] <- c("Total por Filas")
      rownames(fa_marginal)[nrow(fa_marginal)] <- c("Total por Columnas")
      
      # Cociente al Total con marginales
      cociente_marginal <- fa_marginal
      for(n in 1:ncol(cociente_marginal)) cociente_marginal[,n] <- paste0(cociente_marginal[,n], "/", n_total)
      
      
      # Frecuencias Relativas al Total con Marginales
      fr_marginal <- fa_marginal/n_total
      fr_marginal <- round2(fr_marginal, input_decimales)
      
      # Si es "NaN" le ponemos un cero... Hay que hacerlo dos veces
      for (n in 1:ncol(fr_marginal)) if (is.nan(fr_marginal[n])) fr_marginal[n,] <- rep(0, nrow(fr_marginal))
      for (k in 1:nrow(fr_marginal)) if (is.nan(fr_marginal[k])) fr_marginal[,k] <- rep(0, ncol(fr_marginal))
      
      m_col2 <- fr_marginal[nrow(fr_marginal),-ncol(fr_marginal)]
      m_row2 <- fr_marginal[-nrow(fr_marginal),ncol(fr_marginal)]
      totales_raros <- c(sum(m_col2), sum(m_row2))
      diferencia <- abs(totales_raros - 1)
      dt_diferencia <- max(diferencia) == diferencia
      orden_diferencia <- c(1,2)[dt_diferencia]
      if (length(orden_diferencia) == 2) orden_diferencia <- orden_diferencia[1]
      
      
      # En "fr"... los marginales al total por filas y columnas deben sumar 1 en cada caso.
      # Por temas de redondeo... puede que uno sea igual a 1 y el otro no... o que ambos sean diferentes de 1.
      # En ese caso deberiamos avisarle que las sumas marginales no estan dando como debieran... y que
      # debe cambiar los decimales para traajar...
      
      # # Deteccion de redondeo incorrecto
      fr_interno <- fr_marginal
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) fr_marginal[nrow(fr_marginal), ncol(fr_marginal)] <- paste0(fr_marginal[nrow(fr_marginal), ncol(fr_marginal)], "(Redondeo Incorrecto)")  
      
      # Porcentajes al Total con marginales
      porcentaje_marginal <- fr_interno
      porcentaje_marginal <- porcentaje_marginal*100
      #dim(PORCENTAJE) <- dim(FR_interno)
      
      porcentaje2_marginal <- porcentaje_marginal
      for(n in 1:ncol(porcentaje2_marginal)) for (k in 1:nrow(porcentaje2_marginal)) {
        porcentaje2_marginal[k,n] <- paste(porcentaje2_marginal[k,n], "%", sep="")
      }
      
      # Guardamos unos porcentajes internos...
      porcentaje_interno <- porcentaje_marginal
      
      # # Si tuvo problemas con las "fr" dejamos constancia tambien en la tabla de porcentajes
      # if (input_aviso == TRUE) if (totales_raros[orden_diferencia] != 1) porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)] <- paste0(porcentaje_marginal[nrow(porcentaje_marginal), ncol(porcentaje_marginal)], "(Redondeo Incorrecto)")  
      
      ###
    } # Fin Marginales al total
    ###################################################################################
    
    
    # 2) Todo por filas
    {
      
      # FA por filas
      fa_filas <- fa
      total_filas <- rowSums(fa)
      fa_filas <- cbind(fa_filas, total_filas)
      colnames(fa_filas)[ncol(fa_filas)] <- "Total por Filas"
      
      # Cociente por filas
      cociente_filas <- fa_filas
      for (n in 1:nrow(cociente_filas)) cociente_filas[n,] <- paste0(cociente_filas[n,], "/", total_filas[n]) 
      
      # "fr" por filas
      fr_filas <- fa_filas
      for (n in 1:nrow(cociente_filas)) {
        
        fr_filas[n, ] <- fr_filas[n, ]/total_filas[n]
        fr_filas[n, ] <- round2(fr_filas[n, ], 2)
        
        if (total_filas[n] == 0) fr_filas[n, ] <- rep(0, length(fr_filas[n, ]))
        
      }
      
      fr_filas_interno <- fr_filas
      totales_fr_filas <- fr_filas[,ncol(fr_filas)]
      dt_fr_filas <- totales_fr_filas != 1
      #    if (input_aviso == TRUE) fr_filas[dt_fr_filas] <- paste0(fr_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por filas
      porcentaje_filas <- fr_filas_interno*100
      
      porcentaje2_filas <-  porcentaje_filas
      for (n in 1:nrow(porcentaje2_filas)) porcentaje2_filas[n,] <- paste0(porcentaje2_filas[n,], "%")
      #     porcentaje_filas[dt_fr_filas] <- paste0(porcentaje_filas[dt_fr_filas], "(Redondeo Incorrecto)")
      
      ###
    } # Todo por filas
    ###################################################################
    
    
    # 3) Todo por columnas
    {
      ###
      
      
      # FA por columnas
      fa_columnas <- fa
      total_columnas <- colSums(fa)
      fa_columnas <- rbind(fa_columnas, total_columnas)
      rownames(fa_columnas)[nrow(fa_columnas)] <- "Total por columnas"
      
      # Cociente por columnas
      cociente_columnas <- fa_columnas
      for (n in 1:ncol(cociente_columnas)) cociente_columnas[,n] <- paste0(cociente_columnas[,n], "/", total_columnas[n]) 
      
      # "fr" por columnas
      fr_columnas <- fa_columnas
      for (n in 1:ncol(cociente_columnas)) {
        
        fr_columnas[,n ] <- fr_columnas[,n ]/total_columnas[n]
        fr_columnas[,n ] <- round2(fr_columnas[,n ], 2)
        if (total_columnas[n] == 0) fr_columnas[,n ] <- rep(0, length(fr_columnas[,n ]))
      }
      
      fr_columnas_interno <- fr_columnas
      totales_fr_columnas <- fr_columnas[nrow(fr_columnas),]
      dt_fr_columnas <- totales_fr_columnas != 1
      #    if (input_aviso == TRUE) fr_columnas[dt_fr_columnas] <- paste0(fr_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      
      # "porcentaje" por columnas
      porcentaje_columnas <- fr_columnas_interno*100
      
      porcentaje2_columnas <- porcentaje_columnas
      for (n in 1:ncol(porcentaje2_columnas)) porcentaje2_columnas[,n] <- paste0(porcentaje2_columnas[,n], "%")
      #    if (input_aviso == TRUE)  porcentaje_columnas[dt_fr_columnas] <- paste0(porcentaje_columnas[dt_fr_columnas], "(Redondeo Incorrecto)")
      
      
      ###
    } # Todo por columnas
    #################################################
    
    
    # 4) Clasico
    {
      ###
      
      # FA Clasico  
      fa_clasico <- fa_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]
      if(is.null(dim(fa_clasico))) {
        
        dim(fa_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fa_clasico <- as.data.frame(fa_clasico)
        rownames(fa_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fa_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }    
      
      # Cociente Clasico
      cociente_clasico <- cociente_marginal[-nrow( fa_marginal), - ncol( fa_marginal)]    
      if(is.null(dim(cociente_clasico))) {
        
        dim(cociente_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        cociente_clasico <- as.data.frame(cociente_clasico)
        rownames(cociente_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(cociente_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }   
      
      # FR Clasico
      fr_clasico <- fr_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(fr_clasico))) {
        
        dim(fr_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        fr_clasico <- as.data.frame(fr_clasico)
        rownames(fr_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(fr_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %
      porcentaje_clasico <- porcentaje_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje_clasico))) {
        
        dim(porcentaje_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje_clasico <- as.data.frame(porcentaje_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      # %%
      porcentaje2_clasico <- porcentaje2_marginal[-nrow( fa_marginal), - ncol( fa_marginal)] 
      if(is.null(dim(porcentaje2_clasico))) {
        
        dim(porcentaje2_clasico) <- c((nrow(fa_marginal)-1), (ncol(fa_marginal)-1)) 
        porcentaje2_clasico <- as.data.frame(porcentaje2_clasico)
        rownames(porcentaje_clasico) <- rownames(fa_marginal)[-nrow(fa_marginal)]
        colnames(porcentaje_clasico) <- colnames(fa_marginal)[-ncol(fa_marginal)]
      }  
      
      
      ###  
    } # Clasico
    ##################################
    
    
    ###
  } # Fin Resolucion
  ##################################################################
  
  
  # # # Objetos Intermedios
  {
    ###  
    
    input_columnas <- c(2,5)  
    
    # Una funcion propia...
    # Para convertir en "character" algunas columnas de varias tablas en una lista
    char_machine <- function(input_lista = NULL, input_columnas = NULL) {
      
      for (k in 1:length(input_columnas)) {
        for (n in 1:ncol(input_lista[[input_columnas[k]]])) {
          
          input_lista[[input_columnas[k]]][,n] <- as.character(input_lista[[input_columnas[k]]][,n])
          
        } # Fin for n
      } # Fin for k
      
      
      return(input_lista)
    } 
    
    
    
    
    
    # Todo lo CLASICO 
    {
      ###
      
      CLASICO <- list()
      CLASICO[[1]] <- as.data.frame(fa_clasico)
      CLASICO[[2]] <- as.data.frame(cociente_clasico)
      CLASICO[[3]] <- as.data.frame(fr_clasico)
      CLASICO[[4]] <- as.data.frame(porcentaje_clasico)
      CLASICO[[5]] <- as.data.frame(porcentaje2_clasico)
      names(CLASICO) <- c("FA", "Cociente", "FR", "%", "%%")
      CLASICO <- char_machine(CLASICO, input_columnas)
      
      ###
    } # Fin Todo lo CLASICO
    ############################################################################
    
    
    # Todo al TOTAL
    {
      ###
      
      TOTAL <- list()
      TOTAL[[1]] <- as.data.frame(fa_marginal)
      TOTAL[[2]] <- as.data.frame(cociente_marginal)
      TOTAL[[3]] <- as.data.frame(fr_marginal)
      TOTAL[[4]] <- as.data.frame(porcentaje_marginal)
      TOTAL[[5]] <- as.data.frame(porcentaje2_marginal)
      names(TOTAL) <- c("FA", "Cociente", "FR", "%", "%%")
      TOTAL <- char_machine(TOTAL, input_columnas)
      
      ###
    } # Todo al TOTAL
    ############################################################################
    
    
    
    # Todo por FILAS
    {
      ###
      
      
      FILAS <- list()
      FILAS[[1]] <- as.data.frame(fa_filas)
      FILAS[[2]] <- as.data.frame(cociente_filas)
      FILAS[[3]] <- as.data.frame(fr_filas)
      FILAS[[4]] <- as.data.frame(porcentaje_filas)
      FILAS[[5]] <- as.data.frame(porcentaje2_filas)
      names(FILAS) <- c("FA", "Cociente", "FR", "%", "%%")
      FILAS <- char_machine(FILAS, input_columnas)
      
      ###
    } # Todo por filas
    ############################################################################
    
    
    
    # Todo por COLUMNAS
    {
      ###
      
      COLUMNAS <- list()
      COLUMNAS[[1]] <- as.data.frame(fa_columnas)
      COLUMNAS[[2]] <- as.data.frame(cociente_columnas)
      COLUMNAS[[3]] <- as.data.frame(fr_columnas)
      COLUMNAS[[4]] <- as.data.frame(porcentaje_columnas)
      COLUMNAS[[5]] <- as.data.frame(porcentaje2_columnas)
      names(COLUMNAS) <- c("FA", "Cociente", "FR", "%", "%%")    
      COLUMNAS <- char_machine(COLUMNAS, input_columnas)
      
      ###
    } # Fin Todo por COLUMNAS
    ############################################################################
    
    
    # SIMPLE
    {
      ###
      
      grupos_filas <- rownames(fa)
      grupos_columnas <- colnames(fa)
      cantidad_filas <- length(grupos_filas)*length(grupos_columnas)
      nombres_columnas <- c(colnames(input_base), "Frecuencia Absoluta", "Total", "Cociente", "Frecuencia Relativa", "%", "%%")
      
      
      SIMPLE <- as.data.frame(matrix(NA, cantidad_filas, length(nombres_columnas)))
      colnames(SIMPLE) <- nombres_columnas
      
      contador_externo <- 0
      for (k in 1:length(grupos_filas)) {
        for (n in 1:length(grupos_columnas)) {
          contador_externo <- contador_externo + 1
          
          SIMPLE[contador_externo,1] <- grupos_filas[k]
          SIMPLE[contador_externo,2] <- grupos_columnas[n]        
          SIMPLE[contador_externo,3] <- TOTAL[[1]][k,n]    
          SIMPLE[contador_externo,4] <- sum(TOTAL[[1]]) 
          SIMPLE[contador_externo,5] <- TOTAL[[2]][k,n]       
          SIMPLE[contador_externo,6] <- TOTAL[[3]][k,n]       
          SIMPLE[contador_externo,7] <- TOTAL[[4]][k,n]       
          SIMPLE[contador_externo,8] <- TOTAL[[5]][k,n] 
          
        } # Fin for n
      } # Fin for k
      
      #SIMPLE <- list(SIMPLE)  
      
      ###
    } # Fin SIMPLE
    ############################################################################
    
    
    
    # SIMPLE2
    {
      ###
      SIMPLE2 <- SIMPLE[,-7]
      colnames(SIMPLE2)[ncol(SIMPLE2)] <- "%"
      SIMPLE2[,3] <- as.character(SIMPLE2[,3])
      SIMPLE2[,4] <- as.character(SIMPLE2[,4])
      ###
    } # Fin SIMPLE
    ############################################################################
    
    ###
  } # Fin Objetos Intermedios
  ##############################################################################
  
  
  # # # Mis Tablas y cambios "NO DATA" y "Errores"
  {
    ###
    
    mis_tablas <- list(CLASICO, TOTAL, FILAS, COLUMNAS, SIMPLE, SIMPLE2)
    names(mis_tablas) <- c("Clasico", "Al total", "Por filas", "Por columnas", "Simple Entrada", "Simple2")  
    
    
    
    
    ###  
  } # Fin Mis Tablas
  ########################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
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
      for (n in 1:length(mis_tablas)) for (k in 1:length(mis_tablas[[n]])) {
        
        esta_tabla <- mis_tablas[[n]][[k]]
        
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix(cambio_aplicado, estas_dimensiones[1], estas_dimensiones[2]))
        colnames(tabla_no_data) <- colnames(esta_tabla)
        esta_tabla <- tabla_no_data
        
        mis_tablas[[n]][[k]]  <- esta_tabla
        
      } # Fin for n
      
    } # Fin si no es valido trabajar
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Objetos Finales
  {
    ###
    
    referencias <- list()
    referencias[[1]] <- paste0("En filas: ", colnames(mini)[1])
    referencias[[2]] <- paste0("En columnas: ", colnames(mini)[2])
    
    ###  
  } # Fin Objetos Finales
  ###############################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, referencias, output_cadena, input_originales)
    names(salida) <- c("df02", "referencias", "output_cadena", "input_originales")
    
    return(salida)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
  
  
  
} # Fin function

###########################


control_2q <- function(input_base = NULL, input_cadena = NULL){
  
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
      cat("Error control_2q: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_2q: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_2q: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene mas de 2 columna
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_2q: input_base debe ser dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_2q: input_base no presenta filas")
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

#######################



id_var <- function(input_cat=NULL, input_colnames=NULL) {
  
  input_cat <- as.vector(as.matrix(input_cat))
  input_colnames <- as.vector(as.matrix(input_colnames))
  
  dt_pos <- input_cat == input_colnames
  orden <- c(1:length(input_colnames))
  este_orden <- orden[dt_pos]
  esta_letra <- num2let(c(este_orden))
  
  armado <- paste0(input_cat, " - (", esta_letra, ")")
  
  return(armado)
}

#############


mps <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # mps: Medidas de Posicion Simultaneas
  
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
        cat("Error mps: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      estas_columnas <- rep(1, ncol(input_base))
      mini <- as.data.frame(mtcars[estas_columnas])
      colnames(mini) <- "No Data"
      cat("Error mps: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
    
    
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Medidas de Posicion
  {
    ###
    
    # Todas las Medidas de Posicion separadas
    {
      ###
      
      tablas_mps <- list()
      
      for (n in 1:ncol(mini)){
        recorte <- mini[n]
        tablas_mps[[n]] <- mp(input_base = recorte, input_decimales = input_decimales, input_cadena = output_cadena)
      }  
      names(tablas_mps) <- paste0("var",c(1:ncol(mini)))
      
      ###    
    } # Fin Todas
    ###########################################
    
    
    
    
    # Tabla 1 
    {
      ###
      
      for (n in 1:length(tablas_mps)) {
        
        if (n == 1) {
          
          tabla1_mps <- tablas_mps[[n]]$mp$tabla1_mp
          
        } else {
          
          tabla1_mps <- rbind(tabla1_mps, tablas_mps[[n]]$mp$tabla1_mp)
        }
      } # Fin for n
      
      
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      for (n in 1:length(tablas_mps)) {
        
        if (n == 1) {
          
          tabla2_mps <- tablas_mps[[n]]$mp$tabla2_mp
          
        } else {
          
          tabla2_mps <- rbind(tabla2_mps, tablas_mps[[n]]$mp$tabla2_mp)
        }
      } # Fin for n
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    # Tabla 3
    {
      ###
      
      tabla3_mps <- list()
      
      
      for (n in 1:nrow(tablas_mps[[1]]$mp$tabla3_mp)) {
        for (k in 1:length(tablas_mps)) {  
          
          if (k == 1) {
            
            
            tabla_armada <- tablas_mps[[k]]$mp$tabla3_mp[n,]
            
            
            tabla3_mps[[n]] <- tabla_armada
          } else {
            
            tabla_armada <- tablas_mps[[k]]$mp$tabla3_mp[n,]
            
            
            
            tabla3_mps[[n]] <- rbind(tabla3_mps[[n]], tabla_armada)
          }
        } # Fin for n
        rownames(tabla3_mps[[n]]) <- c(1:nrow(tabla3_mps[[n]]))
      } # Fin for k
      
      ###  
    } # Fin Tabla 3
    ############################################################
    
    
    
    
    ###
  } # Fin Medidas de Posicion
  ############################################################################
  
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_mps, tabla2_mps, tabla3_mps)
    names(mis_tablas) <- c("tabla1_mps", "tabla2_mps", "tabla3_mps")
    
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
    names(salida) <- c("mps", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function mp()
########################################################################################################


###



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

######


percentiles2 <- function(input_base = NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # percentiles2: Percentiles simultaneos
  
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
    if (is.null(input_busqueda)) input_busqueda <- c(5, 10, 90, 95)
    
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
        cat("Error percentiles2: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      estas_columnas <- rep(1, ncol(input_base))
      mini <- as.data.frame(mtcars[estas_columnas])
      colnames(mini) <- "No Data"
      cat("Error percentiles2: 'mini' externo agregado (NO DATA)", "\n")
    }
    ### 
    
    
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # Percentiles Simultaneos
  {
    ###
    
    # Todos los percentiles separadas
    {
      ###
      
      todas_per2 <- list()
      
      for (n in 1:ncol(mini)){
        recorte <- mini[n]
        todas_per2[[n]] <- percentiles(input_base = recorte, input_busqueda = input_busqueda, input_decimales = input_decimales, input_cadena = output_cadena)
      }  
      
      
      
      for (n in 1:length(todas_per2)) {
        
        if ( n == 1) tabla_per2 <- todas_per2[[n]]$percentiles$tabla_percentiles
        
        if (n > 1) tabla_per2 <- rbind(tabla_per2, todas_per2[[n]]$percentiles$tabla_percentiles)
      }
      
      
      
      ###    
    } # Fin Todas
    ###########################################
    
    
    
    ###
  } # Fin Percentiles Simultaneos
  ############################################################################
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla_per2)
    names(mis_tablas) <- c("tabla_per2")
    
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
      
    } # Fin si no es valido trabajar...
    ########################################
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("perc2", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function percentiles2()
########################################################################################################




mpp <- function(input_base = NULL, input_decimales = 2, input_cadena = NULL) {
  
  
  
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
    veredicto1 <- control_cq(input_base = input_base, input_cadena = output_cadena)
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
      
      # Si cumple todo... Falta la columna 2 sea de tipo factor.
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error mpp: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error mpp: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    #########################################################
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # VR y FACTOR
  {
    ###
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    vector_VR <- mini[,1]
    vector_FACTOR <- mini[,2]
    
    
    ###  
  } # Fin VR y FACTOR
  #############################################
  
  
  # # # Medidas de Dispersion Particionadas
  {
    ###
    
    # Objetos de utilidad general
    {
      ###
      
      niveles_factor <- levels(vector_FACTOR)
      cantidad_niveles <- length(niveles_factor)
      
      ###  
    } # Fin Objetos
    ####################################
    
    
    # Tabla 1 
    {
      ###
      
      # Medidas Generales
      tabla_mp_general <- mp(VR, input_decimales = input_decimales)$mp$tabla1_mp
      
      # Medidas de Posicion Particionadas
      tabla1_mpp <- as.data.frame(matrix(NA, cantidad_niveles , ncol(tabla_mp_general)))
      colnames(tabla1_mpp) <- colnames(tabla_mp_general)
      colnames(tabla1_mpp)[1] <- colnames(FACTOR)
      
      
      # Segun la cantidad de niveles
      if (cantidad_niveles > 0) {
        for(j in 1:cantidad_niveles) {
          
          dt <- NULL
          recorte <- NULL
          
          dt <- FACTOR == levels(FACTOR[,1])[j]
          recorte <- VR[dt, 1]
          dim(recorte) <- c(length(recorte), 1)
          recorte <- as.data.frame(recorte)
          colnames(recorte) <- levels(FACTOR[,1])[j]
          
          tabla1_mpp[j,] <- mp(recorte, input_decimales)$mp$tabla1_mp
          
        } # FIn for j
        
      } # Fin if cantidad > 0
      
      # Agregamos las medidas generales
      tabla_mp_general[1,1] <- " --Medidas Generales--"
      colnames(tabla_mp_general)[1] <- colnames(FACTOR)
      
      tabla1_mpp <- rbind(tabla1_mpp, tabla_mp_general)
      colnames(tabla1_mpp)[1] <- paste0("Variable: ", colnames(FACTOR))      
      
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      # Le sacamos las medidas del general
      tabla2_mpp <- tabla1_mpp[-nrow(tabla1_mpp), ]
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    # Tabla 3
    {
      ###
      
      # Medidas Generales
      tabla_ic_general <- mp(VR, input_decimales = input_decimales)$mp$tabla3_mp
      
      # Agregamos las medidas generales
      tabla_ic_general[,1] <- " --IC General--"
      colnames(tabla_ic_general)[1] <- "Variable"
      
      # IC Particionados
      tabla3_icp <- list()
      
      
      
      for (n in 1:cantidad_niveles) {
        
        dt <- NULL
        recorte <- NULL
        
        dt <- FACTOR == levels(FACTOR[,1])[n]
        recorte <- VR[dt, 1]
        dim(recorte) <- c(length(recorte), 1)
        recorte <- as.data.frame(recorte)
        colnames(recorte) <- levels(FACTOR[,1])[n]
        
        tabla3_icp[[n]] <- mp(recorte, input_decimales)$mp$tabla3_mp
        
        
        
      } # FIn for n
      
      # 
      #       nueva_pos <- (length(tabla3_icp) + 1)
      #       tabla3_icp[[nueva_pos]] <- tabla_ic_general
      
      # tabla3_icp <- rbind(tabla3_icp, tabla_ic_general)
      
      
      
      # IC Particionados V2
      tabla3_icp2 <- list()
      
      for (n in 1:nrow(tabla_ic_general)) {
        for (k in 1:length(tabla3_icp)) {
          
          
          if (k == 1) tabla_armada <- tabla3_icp[[k]][n,]
          
          if (k > 1) tabla_armada <- rbind(tabla_armada, tabla3_icp[[k]][n,])
          
          if (k == length(tabla3_icp)){
            tabla_armada <- rbind(tabla_armada, tabla_ic_general[n,])
            rownames(tabla_armada) <- c(1:nrow(tabla_armada))
            colnames(tabla_armada)[1] <- paste0("Variable: ", colnames(FACTOR))
            tabla3_icp2[[n]] <- tabla_armada
          }
        } # Fin for k
        # rownames(tabla3_icp2[[k]]) <- c(1:nrow(tabla3_icp2[[k]]))
        # 
      } # Fin for n
      
      
      
      
      
      ###  
    } # Tabla 3
    ####################################################################
    
    
    ###
  } # Fin Medidas de Posicion Particionadas
  ############################################################################
  
  
  # # # Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_mpp, tabla2_mpp, tabla3_icp2)
    names(mis_tablas) <- c("tabla1_mpp", "tabla2_mpp", "tabla3_icp2")
    
    ###  
  } # Fin Mis Tablas
  ###########################################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
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
    names(salida) <- c("mpp", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
}

#######


control_cq <- function(input_base = NULL, input_cadena = NULL){
  
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
      cat("Error control_qc: input_base no debe ser nulo", "\n")
      output_cadena <- FALSE
    }  
    
    # Verificar si es un data frame
    if(output_cadena && !is.data.frame(input_base)) {
      cat("Error control_qc: input_base debe ser un data.frame", "\n")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene al menos una columna
    if(output_cadena && ncol(input_base) == 0) {
      cat("Error control_qc: input_base no tiene columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si tiene una cantidad diferente de 2 columnas
    if(output_cadena && ncol(input_base) != 2) {
      cat("Error control_cq: input_base debe tener dos columnas")
      output_cadena <- FALSE
    }
    
    # Verificar si es no tiene datos
    if(output_cadena && nrow(input_base) == 0) {
      stop("Error control_cq: input_base no presenta filas")
      output_cadena <- FALSE
    }
    
    # Verificar si la 1ra columna es numerica
    if(output_cadena && !is.numeric(input_base[,1])) {
      stop("Error control_cq: input_base columna 1 debe ser numérica")
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


###########



mdp <- function(input_base = NULL, input_decimales = 2, input_cadena = NULL) {
  
  
  
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
    veredicto1 <- control_cq(input_base = input_base, input_cadena = output_cadena)
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
      
      # Si cumple todo... Falta la columna 1 sea de tipo factor.
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error mdp: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error mdp: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    #########################################################
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # VR y FACTOR
  {
    ###
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    vector_VR <- mini[,1]
    vector_FACTOR <- mini[,2]
    
    
    ###  
  } # Fin VR y FACTOR
  #############################################
  
  
  # # # Medidas de Dispersion Particionadas
  {
    ###
    
    # Objetos de utilidad general
    {
      ###
      
      niveles_factor <- levels(vector_FACTOR)
      cantidad_niveles <- length(niveles_factor)
      
      ###  
    } # Fin Objetos
    ####################################
    
    
    # Tabla 1 
    {
      ###
      
      # Medidas Generales
      tabla_md_general <- md(VR, input_decimales = input_decimales)$md$tabla1_md
      
      # Medidas de Posicion Particionadas
      tabla1_mdp <- as.data.frame(matrix(NA, cantidad_niveles , ncol(tabla_md_general)))
      colnames(tabla1_mdp) <- colnames(tabla_md_general)
      colnames(tabla1_mdp)[1] <- colnames(FACTOR)
      
      
      # Segun la cantidad de niveles
      if (cantidad_niveles > 0) {
        for(j in 1:cantidad_niveles) {
          
          dt <- NULL
          recorte <- NULL
          
          dt <- FACTOR == levels(FACTOR[,1])[j]
          recorte <- VR[dt, 1]
          dim(recorte) <- c(length(recorte), 1)
          recorte <- as.data.frame(recorte)
          colnames(recorte) <- levels(FACTOR[,1])[j]
          
          tabla1_mdp[j,] <- md(recorte, input_decimales)$md$tabla1_md
          
        } # FIn for j
        
      } # Fin if cantidad > 0
      
      # Agregamos las medidas generales
      tabla_md_general[1,1] <- " --Medidas Generales--"
      colnames(tabla_md_general)[1] <- colnames(FACTOR)
      
      tabla1_mdp <- rbind(tabla1_mdp, tabla_md_general)
      colnames(tabla1_mdp)[1] <- paste0("Variable: ", colnames(FACTOR))
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      # Le sacamos las medidas del general
      tabla2_mdp <- tabla1_mdp[-nrow(tabla1_mdp), ]
      
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    ###
  } # Fin Medidas de Posicion Particionadas
  ############################################################################
  
  
  # # # Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_mdp, tabla2_mdp)
    names(mis_tablas) <- c("tabla1_mdp", "tabla2_mdp")
    
    ###  
  } # Fin Mis Tablas
  ###########################################################################
  
  
  # # # Cambios "NO DATA" o "Errores"
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
    names(salida) <- c("mdp", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
}




percentiles3 <- function(input_base = NULL, input_busqueda = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # percentiles3: Percentiles particionados
  
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
    if (is.null(input_busqueda)) input_busqueda <- c(5, 10, 90, 95)
    
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
    veredicto1 <- control_cq(input_base = input_base, input_cadena = output_cadena)
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
        cat("\n", "Error percentiles3: input_busqueda no debe ser nulo", "\n")
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
  
  
  # # # Modificaciones, Controles 3, y base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta la columna 2 sea de tipo factor.
      if (is.factor(input_base[,2]) == FALSE) input_base[,2] <- as.factor(as.character(input_base[,2]))
      
      # Creamos "mini"  
      mini <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(mini) == 0) {
        cat("Error percentiles3: 'mini' sin filas", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ####################################################
    
    # Si algo no esta OK...
    if (output_cadena == FALSE){
      mini <- as.data.frame(mtcars[c(1,2)])
      colnames(mini) <- c("No Data", "No Data")
      mini[,2] <- as.factor(as.character(mini[,2]))
      cat("Error percentiles3: 'mini' externo agregado (NO DATA)", "\n")
    } # Fin si algo no esta OK
    #########################################################
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    ###  
  } # Fin Modificaciones y Controles 2
  ############################################################################
  
  
  # # # VR y FACTOR
  {
    ###
    
    VR <- mini[1]
    FACTOR <- mini[2]
    
    vector_VR <- mini[,1]
    vector_FACTOR <- mini[,2]
    
    
    ###  
  } # Fin VR y FACTOR
  #############################################
  
  
  # # # Percentiles Particionadas
  {
    ###
    
    # Objetos de utilidad general
    {
      ###
      
      niveles_factor <- levels(vector_FACTOR)
      cantidad_niveles <- length(niveles_factor)
      
      ###  
    } # Fin Objetos
    ####################################
    
    
    # Tabla 1 
    {
      ###
      
      # Medidas Generales
      tabla_percentiles_general <- percentiles(VR, input_decimales = input_decimales, input_busqueda = input_busqueda)$percentiles$tabla_percentiles
      
      # # Medidas de Posicion Particionadas
      tabla1_percp <- as.data.frame(matrix(NA, cantidad_niveles , ncol(tabla_percentiles_general)))
      colnames(tabla1_percp) <- colnames(tabla_percentiles_general)
      colnames(tabla1_percp)[1] <- colnames(FACTOR)
      
      
      
      for(j in 1:cantidad_niveles) {
        
        dt <- NULL
        recorte <- NULL
        
        dt <- FACTOR == levels(FACTOR[,1])[j]
        recorte <- VR[dt, 1]
        dim(recorte) <- c(length(recorte), 1)
        recorte <- as.data.frame(recorte)
        colnames(recorte) <- levels(FACTOR[,1])[j]
        
        tabla1_percp[j,] <- percentiles(recorte, input_decimales, input_busqueda = input_busqueda)$percentiles$tabla_percentiles
        
      } # FIn for j
      
      
      # 
      # Agregamos las medidas generales
      tabla_percentiles_general[1,1] <- " --Medidas Generales--"
      colnames(tabla_percentiles_general)[1] <- colnames(FACTOR)
      
      tabla1_percp <- rbind(tabla1_percp, tabla_percentiles_general)
      colnames(tabla1_percp)[1] <- paste0("Variable: ", colnames(FACTOR))
      
      
      ###
    } # Fin Tabla 1
    ############################################################
    
    
    # Tabla 2
    {
      ###
      
      # Le sacamos las medidas del general
      tabla2_percp <- tabla1_percp[-nrow(tabla1_percp), ]
      ###  
    } # Fin Tabla 2
    ############################################################
    
    
    ###
  } # Fin Medidas de Posicion Particionadas
  ############################################################################
  
  
  # # #Mis Tablas
  {
    ###
    
    mis_tablas <- list(tabla1_percp, tabla2_percp)
    names(mis_tablas) <- c("tabla1_percp", "tabla2_percp")
    
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
      
    } # Fin si no es valido trabajar...
    ########################################
    
    ###   
  } # Fin Cambios "NO DATA" o "Errores"
  ##################################################
  
  
  # # # Salida
  {
    ###
    
    salida <- list(mis_tablas, output_cadena, input_originales)
    names(salida) <- c("perc3", "output_cadena", "input_originales")
    
    return(salida)
    
    ###  
  } # Salida
  ############################################################################
  
  
  
  
  
} # Fin function percentiles3()
########################################################################################################



################


RMedic_1q <- function(input_base = NULL, input_decimales = NULL, input_cadena = NULL) {
  
  # Default values
  if (is.null(input_decimales)) input_decimales <- 2
  if (is.null(input_cadena)) input_cadena <- T
  
  # # # Control 1 - input_base
  {
    ###
    # 1- Es nulo
    # 2- No es un data.frame
    # 3- Tiene cero columnas
    # 4- Tienes más de 1 columna
    # 5- No tiene filas (nrow(input_base))
    
    veredicto1 <- control_1q(input_base = input_base, input_cadena = input_cadena)
    output_cadena <- veredicto1
    
    ###  
  } # Fin Control 1 - input_base
  #################################################
  
  
  
  # # # minibase y Control 2 -  base "NO DATA"
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Creamos "mini"  
      minibase <- na.omit(input_base)
      
      # Vemos que mini tenga filas
      if (nrow(minibase) == 0) {
        cat("Error df01: 'input_base' posee solo celdas vacias.", "\n")
        output_cadena <- FALSE
      }
      
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    
    ### 
  } # Fin Control 2 -  base "NO DATA"
  ################################################################
  
  
  # # # SandBox en caso de no ser valido
  {
    ###
    
    # Si hay algun problema...
    if (output_cadena == FALSE){
      minibase <- as.data.frame(mtcars[2])
      minibase[1] <- as.factor(as.character(minibase[,1]))
      colnames(minibase) <- "No Data"
      cat("Error df01: 'minibase' externo agregado (NO DATA)", "\n")
    } # Fin if si hay problemas
    ##########################################################
    
    ###  
  }
  ############################################################################
  
  
  # # # Cohersion como as.factor()
  {
    ###
    
    # Si todo va OK...
    if (output_cadena) {
      
      # Si cumple todo... Falta que sea de tipo factor.
      minibase[1] <- as.factor(as.character(minibase[,1]))
      
      
      
      
    } # Fin if Si todo va OK...
    ###################################################
    
    
    
    
    ### 
  } # Fin Cohersion como as.factor()
  ################################################################
  
  
  
  
  
  
  
  # Tabla 1: Distribucion de Frecuencias
  {
    ###
    
    # Frecuencais Absolutas
    fa <- table(minibase)
    
    # Total Absoluto
    n_total <- sum(fa)
    grupos <- as.character(names(fa))
    salida_n_total <- rep(n_total, length(fa))
    
    # Cociente
    cociente <- paste0(fa, "/", n_total)
    
    # Frecuencias Relativas
    fr <- round2((fa/n_total), input_decimales)
    
    
    
    # Porcentajes
    porcentaje  <- paste(fr*100, "%", sep="")
    
    # Fusion
    fusion <- paste0(porcentaje, "(", fa, ")")
    
    # Tabla de Distribucion de Frecuencias
    tabla01 <- cbind(grupos, fa, salida_n_total, cociente, fr,  
                     porcentaje, fusion)
    
    rotulo <- paste0("Variable: ", colnames(minibase))
    tabla01 <- as.data.frame(tabla01)
    nombres_tabla01 <- c(rotulo, "FA", "Total", "Cociente", "FR", "%", "%(FA)")
    colnames(tabla01) <- nombres_tabla01
    
    numericas <- c(2, 3, 5)
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
      
      
      tabla02 <- cbind(grupos, porcentaje, li_porcentaje, ls_porcentaje)
      colnames(tabla02) <- c(rotulo, "%", paste0("Límite Inferior ", confianza), paste0("Límite Supeior ", confianza))
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
  
  
  
  # Mis Tablas
  {
    ###
    mis_tablas <- list(tabla01, tablas2_ic[[1]], tablas2_ic[[2]], tablas2_ic[[3]])
    names(mis_tablas) <- c("Distribución de Frecuencias", 
                           "Intervalos de Confianza del 90% para el Porcentaje",
                           "Intervalos de Confianza del 95% para el Porcentaje", 
                           "Intervalos de Confianza del 99% para el Porcentaje")
    
    ###    
  }
  ##########################################################################
  
  
  
  
  
  
  # # # Cambios "NO DATA" o "Errores"
  {
    ###
    
    
    # Si hay errores... Y estamos en "NO DATA"
    if (output_cadena == FALSE) {
      
      
      # Cambiamos los valores por avisos
      for (n in 1:length(mis_tablas)) {
        
        esta_tabla <- mis_tablas[[n]]
        estas_dimensiones <- dim(esta_tabla)
        tabla_no_data <- as.data.frame(matrix("Sin datos", estas_dimensiones[1], estas_dimensiones[2]))
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
    
    return(mis_tablas)
    
    
    ###  
  } # Fin Salida
  ##################
  
  
  
}

########################


CifrasPerfectas <- function(cifras = NULL, digitos = 2){
  
  cifras_perfectas <- rep(NA, length(cifras))
  
  for (k in 1:length(cifras)){
    
    cantidad_cifras <- nchar(cifras[k])
    
    cifras_extra <- digitos - cantidad_cifras
    
    el_extra <- rep(0, cifras_extra)
    
    cifras_perfectas[k] <- paste0(el_extra, cifras[k])
  }
  
  return(cifras_perfectas)
}
