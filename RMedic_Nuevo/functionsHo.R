
Test_1Q_Proporciones <- function(input_base = NULL, 
                              input_categoria_exito = NULL,
                              input_tipo_prueba = "two.sided", 
                              input_prop_ho = 0.5, 
                              input_decimales = 2, 
                              input_alfa = 0.05){
  
  
 
  
  # Reposicion por valores NULL
  if(is.null(input_categoria_exito)) input_categoria_exito <- names(table(input_base))[1]
  


  
   
  

  # Tabla de Frecuencias
   tabla_fa <- table(input_base)
   n_muestra <- sum(tabla_fa)
   tabla_fr <- tabla_fa/n_muestra



 # Vamos armando el test
 p <- tabla_fr[input_categoria_exito]
 q <- 1 - p

 # Estimacion de varianza y error estandard del estimador de proporcion
 varianza <- (p*q)
 error_estandard <- sqrt(varianza/n_muestra)
 

 
 # Proporcion poblacional bajo ho (Esperada)
 prop_esperada <- input_prop_ho
 prop_esperada_externa <- prop_esperada # SIN REDONDEAR!!!!


 # Proporcion Observada
 prop_observada <- p
 prop_observada_externa <- round(prop_observada, input_decimales)
 
 # Tipo de prueba
 if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
   if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
     if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
       
  
      z_obs_interno <- (prop_observada - input_prop_ho)/error_estandard 
      z_obs_externo <- round(z_obs_interno, input_decimales)
  
 


 #  # Valor p
  if (z_obs_interno < 0) valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = T)
  if (z_obs_interno >= 0) valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = F)

  # Si la prueba es bilateral, falta multiplicar por dos.
  if(input_tipo_prueba == "two.sided") valor_p_interno <- valor_p_interno*2
 
 #  # Valor p externo
 valor_p_externo <- round2(valor_p_interno, input_decimales)
 if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01" else valor_p_externo <- round2(valor_p_interno, input_decimales)

 
 
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"

  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"


      # frase2_html: segun valor p
      {

        frase2_v1 <- "No pudo obtenerse un valor p."

        frase2_v2 <- paste0("El valor p=",valor_p_externo, " es mayor que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "La proporción observada para la categoría '",input_categoria_exito,"' (", prop_observada_externa, ") es estadísticamente
                       igual al valor bajo hipótesis (", prop_esperada_externa, ").", "<br/>")


        frase2_v3 <- paste0("El valor p=",valor_p_externo, " es igual que el valor de alfa=", input_alfa, ".","<br/>",
                       "No se rechaza la Ho.", "<br/>",
                       "La proporción observada para la categoría '",input_categoria_exito,"' (", prop_observada_externa, ") es estadísticamente
                       igual al valor bajo hipótesis (", prop_esperada_externa, ").", "<br/>")

        frase2_v4 <- paste0("El valor p=",valor_p_externo, " es menor que el valor de alfa=", input_alfa, ".","<br/>",
                       "Se rechaza la Ho.", "<br/>",
                       "La proporción observada para la categoría '",input_categoria_exito,"' (", prop_observada_externa, ") es estadísticamente
                       distinta del valor bajo hipótesis (", prop_esperada_externa, ").", "<br/>")

        if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_estadistica <- frase2_v1 else
          if (valor_p_interno > input_alfa) frase_estadistica <- frase2_v2 else
            if (valor_p_interno == input_alfa) frase_estadistica <- frase2_v3 else
              if (valor_p_interno < input_alfa) frase_estadistica <- frase2_v4


      
       } # Fin Frase2_html
      # #######################################
      # 
      
      nombres <- c("Variable",  # 1
                   "n",  #  2
                   "Éxito",  # 3
                   "Proporción Observada",  # 4
                   "Proporción bajo hipótesis (esperada)",  # 5
                   "Tipo de prueba", # 6
                   "Valor Z",  # 7
                   "Valor p", "Alfa", "Decisión", 
                   "¿Existen diferencias entre la proporción observada y la 
                   proporción bajo hipótesis?")
      RESUMEN <- matrix(NA, 1, length(nombres))
      colnames(RESUMEN) <- nombres
      
      RESUMEN[1, 1] <- colnames(input_base)[1]
      RESUMEN[1, 2] <- n_muestra
      RESUMEN[1, 3] <- input_categoria_exito
      RESUMEN[1, 4] <-  prop_observada_externa
      RESUMEN[1, 5] <-  input_prop_ho 
      RESUMEN[1, 6] <- tipo_de_prueba
      RESUMEN[1, 7] <- z_obs_externo
       RESUMEN[1, 8] <- valor_p_externo
       RESUMEN[1, 9] <- input_alfa
       RESUMEN[1,10] <- decision
       RESUMEN[1,11] <- respuesta
      # 
     
      
   
     # frase_estadistica <- "Esta es la frase estadística"
    
    SALIDA_ARMADA <- list()
    
    SALIDA_ARMADA$RESUMEN <- RESUMEN
    
    SALIDA_ARMADA$FRASE <- frase_estadistica
    
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function CHI_MASTER()



if ( 1 == 2) {
  
  Test_1Q_Proporciones(input_base = mtcars[8], 
                                   input_categoria_exito = 1,
                                   input_tipo_prueba = "two.sided", 
                                    input_prop_ho = 0.5, 
                                   input_decimales = 2, 
                                   input_alfa = 0.05)
  
  
} # Fin if