
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
      if(input_tipo_prueba == "two.sided") { 
        if (z_obs_interno < 0) valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = T)
        if (z_obs_interno >= 0) valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = F)
      
        valor_p_interno <- valor_p_interno*2
        
        } else
        if(input_tipo_prueba == "less") { 
          valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = T)
        
          } else
          if(input_tipo_prueba == "greater") {
            valor_p_interno <- pnorm(z_obs_interno, 0, 1, lower.tail = F)
          }
      
 
 
 #  # Valor p externo
 valor_p_externo <- round2(valor_p_interno, input_decimales)
 if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01" else valor_p_externo <- round2(valor_p_interno, input_decimales)

 
 
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"

  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"


      # Frases segun valor p
      {
        # Algun inconveniente
        frase0_v1 <- "No pudo obtenerse un valor p."
        
        # Bilateral
        {
        frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto no se rechaza la Ho de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente igual al valor bajo hipótesis (_mi_prop_esp)."


        frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto no se rechaza la Ho de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente igual al valor bajo hipótesis (_mi_prop_esp)."
        
     
        frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto no se rechaza la Ho de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente diferente al valor bajo hipótesis (_mi_prop_esp)."
        }

        # Unilateral Izquierda
        {
          frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto no se rechaza la Ho de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor o igual al valor bajo hipótesis (_mi_prop_esp)."
          
          
          frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto no se rechaza la Ho de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor o igual al valor bajo hipótesis (_mi_prop_esp)."
          
          
          frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto se rechaza la Ho de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor al valor bajo hipótesis (_mi_prop_esp)."
        }
        
        # Unilateral Derecha
        {
          frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto no se rechaza la Ho de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor o igual al valor bajo hipótesis (_mi_prop_esp)."
          
          
          frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto no se rechaza la Ho de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor o igual al valor bajo hipótesis (_mi_prop_esp)."
          
          
          frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto no se rechaza la Ho de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor al valor bajo hipótesis (_mi_prop_esp)."
        }
      
        # Seleccion de Frase Estadistica
        if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_estadistica <- frase0_v1 else
          if(input_tipo_prueba == "two.sided") {
            if (valor_p_interno > input_alfa) frase_estadistica <- frase1_v1 else
              if (valor_p_interno == input_alfa) frase_estadistica <- frase1_v2 else
                if (valor_p_interno < input_alfa) frase_estadistica <- frase1_v3
          } else
            if(input_tipo_prueba == "less") {
              if (valor_p_interno > input_alfa) frase_estadistica <- frase2_v1 else
                if (valor_p_interno == input_alfa) frase_estadistica <- frase2_v2 else
                  if (valor_p_interno < input_alfa) frase_estadistica <- frase2_v3
            } else
              if(input_tipo_prueba == "greater") {
                if (valor_p_interno > input_alfa) frase_estadistica <- frase3_v1 else
                  if (valor_p_interno == input_alfa) frase_estadistica <- frase3_v2 else
                    if (valor_p_interno < input_alfa) frase_estadistica <- frase3_v3
              }
              
         

      frase_estadistica <- gsub("_mi_categoria_", input_categoria_exito, frase_estadistica)
      frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
      frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
      frase_estadistica <- gsub("_mi_prop_obs_", prop_observada_externa, frase_estadistica)
      frase_estadistica <- gsub("_mi_prop_esp", prop_esperada_externa, frase_estadistica)
      
       } # Fin Frases segun valor p
 
 
 # Frases Juego de Hipotesis
 {

   # Bilateral
   frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la proporción de la categoría '_mi_categoria_' es igual a _mi_prop_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la proporción de la categoría '_mi_categoria_' es diferente de _mi_prop_esp_."
     
   # Unilateral Izquierda
   frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la proporción de la categoría '_mi_categoria_' es mayor o igual a _mi_prop_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la proporción de la categoría '_mi_categoria_' es menor a _mi_prop_esp_."
   
   # Unilateral Derecha
   frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la proporción de la categoría '_mi_categoria_' es menor o igual a _mi_prop_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la proporción de la categoría '_mi_categoria_' es mayor a _mi_prop_esp_."
   
   
   
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
   
   frase_juego_hipotesis <- gsub("_mi_categoria_", input_categoria_exito, frase_juego_hipotesis)
   frase_juego_hipotesis <- gsub("_mi_prop_esp_", prop_esperada_externa, frase_juego_hipotesis)
   
   
   
   
 
 } # Fin Frases Juego de Hipotesis
 
 
      # Frase por incontenientes de redondeo
      dt1 <- valor_p_interno < input_alfa
      dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
      if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
        if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
          if (sum(dt1, dt2) == 1){
            frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test
            de proporciones. Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.
            </b>"
            
          } 
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
    
    SALIDA_ARMADA$resumen <- RESUMEN
    
    SALIDA_ARMADA$frase_estadistica <- frase_estadistica
    
    SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
    SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
      
      
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