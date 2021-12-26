
Test_1Q_TestDeUnaProporcion <- function(input_base = NULL, 
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
 prop_observada_externa <- round2(prop_observada, input_decimales)
 
 # Tipo de prueba
 if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
   if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
     if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
       
 # Estadistico
 estadistico_obs_interno <- (prop_observada - input_prop_ho)/error_estandard 
 estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
 


 # Valor p
      if(input_tipo_prueba == "two.sided") { 
        if (estadistico_obs_interno < 0) valor_p_interno <- pnorm(estadistico_obs_interno, 0, 1, lower.tail = T)
        if (estadistico_obs_interno >= 0) valor_p_interno <- pnorm(estadistico_obs_interno, 0, 1, lower.tail = F)
      
        valor_p_interno <- valor_p_interno*2
        
        } else
        if(input_tipo_prueba == "less") { 
          valor_p_interno <- pnorm(estadistico_obs_interno, 0, 1, lower.tail = T)
        
          } else
          if(input_tipo_prueba == "greater") {
            valor_p_interno <- pnorm(estadistico_obs_interno, 0, 1, lower.tail = F)
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente igual al valor bajo hipótesis (_mi_prop_esp_)."


        frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente igual al valor bajo hipótesis (_mi_prop_esp_)."
        
     
        frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente diferente al valor bajo hipótesis (_mi_prop_esp_)."
        }

        # Unilateral Izquierda
        {
          frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor o igual al valor bajo hipótesis (_mi_prop_esp_)."
          
          
          frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor o igual al valor bajo hipótesis (_mi_prop_esp_)."
          
          
          frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor al valor bajo hipótesis (_mi_prop_esp_)."
        }
        
        # Unilateral Derecha
        {
          frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor o igual al valor bajo hipótesis (_mi_prop_esp_)."
          
          
          frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente menor o igual al valor bajo hipótesis (_mi_prop_esp_)."
          
          
          frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La proporción observada para la categoría '_mi_categoria_' (_mi_prop_obs_) es 
                       estadísticamente mayor al valor bajo hipótesis (_mi_prop_esp_)."
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
      frase_estadistica <- gsub("_mi_prop_esp_", prop_esperada_externa, frase_estadistica)
      
       } # Fin Frases segun valor p
 
 
 # Frases Juego de Hipotesis
 {

   # Bilateral
   frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la proporción poblacional de la categoría '_mi_categoria_' es igual a _mi_prop_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la proporción poblacional de la categoría '_mi_categoria_' es diferente de _mi_prop_esp_."
     
   # Unilateral Izquierda
   frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la proporción poblacional de la categoría '_mi_categoria_' es mayor o igual a _mi_prop_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la proporción poblacional de la categoría '_mi_categoria_' es menor a _mi_prop_esp_."
   
   # Unilateral Derecha
   frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la proporción poblacional de la categoría '_mi_categoria_' es menor o igual a _mi_prop_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la proporción poblacional de la categoría '_mi_categoria_' es mayor a _mi_prop_esp_."
   
   
   
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
                   "Test", # 3
                   "Categiría Éxito",  # 4
                   "Proporción Observada",  # 5
                   "Proporción bajo hipótesis (esperada)",  # 6
                   "Tipo de prueba", # 7
                   "Estadístico (Z)",  # 8
                   "Valor p", # 9 
                   "Alfa", #10
                   "Decisión", #11 
                   "¿Existen diferencias entre la proporción observada y la 
                   proporción bajo hipótesis?" # 12
                   )
      RESUMEN <- matrix("--------", 1, length(nombres))
      colnames(RESUMEN) <- nombres
      
      RESUMEN[1, 1] <- colnames(input_base)[1]
      RESUMEN[1, 2] <- n_muestra
      RESUMEN[1, 3] <- "Test de una proporción"
      RESUMEN[1, 4] <- input_categoria_exito
      RESUMEN[1, 5] <-  prop_observada_externa
      RESUMEN[1, 6] <-  input_prop_ho 
      RESUMEN[1, 7] <- tipo_de_prueba
      RESUMEN[1, 8] <- estadistico_obs_externo
       RESUMEN[1, 9] <- valor_p_externo
       RESUMEN[1,10] <- input_alfa
       RESUMEN[1,11] <- decision
       RESUMEN[1,12] <- respuesta
      # 
     
      
   
     # frase_estadistica <- "Esta es la frase estadística"
    
    SALIDA_ARMADA <- list()
    
    SALIDA_ARMADA$resumen <- RESUMEN
    
    SALIDA_ARMADA$frase_estadistica <- frase_estadistica
    
    SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
    SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
      
      
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 



###########################################################

Test_1C_TestNormalidad_ShapiroWilk <- function(input_base = NULL,
                                    input_decimales = 2, 
                                    input_alfa = 0.05){
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  vector_base <- as.vector(input_base[,1])
  
  # Tamanio de la muestra
  n_muestra <- nrow(input_base)
  
  # Test de normalidad de Shapiro-Wilk
  test <- shapiro.test( x = vector_base)
  
  # Valores internos
  valor_p_interno <- test$p.value
  estadistico_obs_interno <- test$statistic
  
  
   #  # Valor p externo
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01" 
  
  # Valor estadistico externo
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  if (estadistico_obs_interno < 0.01) estadistico_obs_externo <- "<<0.01" 
  
  
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
  
  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"
  
  
  # Frases segun valor p
  {
    # Algun inconveniente
    frase0_v1 <- "No pudo obtenerse un valor p."
    
    # Frases
    {
      frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       La distribución de la variable '_mi_variable_' es 
                       estadísticamente igual a la distribución normal."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       La distribución de la variable '_mi_variable_' es 
                       estadísticamente igual a la distribución normal."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                      por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       La distribución de la variable '_mi_variable_' es 
                       estadísticamente diferente a la distribución normal."
    }
    
  
    
   
    # Seleccion de Frase Estadistica
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_estadistica <- frase0_v1 else
        if (valor_p_interno > input_alfa) frase_estadistica <- frase1_v1 else
          if (valor_p_interno == input_alfa) frase_estadistica <- frase1_v2 else
            if (valor_p_interno < input_alfa) frase_estadistica <- frase1_v3
    
    
    
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_variable_", colnames(input_base)[1], frase_estadistica)
  
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
  frase_juego_hipotesis <-  "<b>Hipótesis Nula (Ho):</b> la variable '_mi_variable_' posee distribución normal.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la variable '_mi_variable_' NO posee distribución normal."

    frase_juego_hipotesis <- gsub("_mi_variable_", colnames(input_base)[1], frase_juego_hipotesis)
    
    
    
    
    
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
            de normalidad de Shapiro-Wilk. Aumente la cantidad de decimales hasta 
            que desaparezca esta advertencia.
            </b>"
        
      } 
  # #######################################
  # 
  
  nombres <- c("Variable",  # 1
               "n",  #  2
               "Test", # 3
               "Estadístico (W)",  # 4
               "Valor p", # 5
               "Alfa", #6
               "Decisión", #7 
               "¿La variable analizada posee distribución normal?" # 8
  )
  
  tabla_resumen <- matrix("--------", 1, length(nombres))
  colnames(tabla_resumen) <- nombres
  
  tabla_resumen[1, 1] <- colnames(input_base)[1]
  tabla_resumen[1, 2] <- n_muestra
  tabla_resumen[1, 3] <- "Test de Normalidad (Shapiro-Wilk)"
  tabla_resumen[1, 4] <- estadistico_obs_externo
  tabla_resumen[1, 5] <- valor_p_externo
  tabla_resumen[1, 6] <- input_alfa
  tabla_resumen[1, 7] <- decision
  tabla_resumen[1, 8] <- respuesta
  # 
  
  
  
  # frase_estadistica <- "Esta es la frase estadística"
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
}


Test_1C_TestT_UnaMuestra <- function(input_base = NULL, 
                                        input_tipo_prueba = "two.sided", 
                                        input_media_ho = 0, 
                                        input_decimales = 2, 
                                        input_alfa = 0.05){
  

  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra <- nrow(input_base)
      
  # Test de Normalidad
  test_normalidad <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base,
                                     input_decimales = input_decimales,
                                     input_alfa = input_alfa)
  
  # Cumplimiento de normalidad
  cumplimiento_normalidad <- test_normalidad$tabla_resumen[1,8]
  
  
  # Test t
  test_t <- t.test(x = input_base, 
                   alternative = input_tipo_prueba, 
                   mu = input_media_ho,
                   paired = FALSE,
                   conf.level = confianza)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estadistico observado
  estadistico_obs_interno <- test_t$statistic
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estiamdores
  mu_esp_interna <- input_media_ho
  mu_esp_externa <- mu_esp_interna # Aproposito sin redondear
  media_obs_interna <- mean(input_base[,1])
  media_obs_externa <- round2(media_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- test_t$parameter
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- test_t$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    
    
    frase_estadistica <- gsub("_mi_variable_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_media_obs_", media_obs_externa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mu_esp_", mu_esp_externa, frase_estadistica)
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media problacional de la variable '_mi_variable_' es diferente de _mi_mu_esp_."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    
    frase_juego_hipotesis <- gsub("_mi_variable_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_mu_esp_", mu_esp_externa, frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
  nombres1 <- c("Variable", "n", "Test de Normalidad", "¿Es normal la variable?", 
              "¿Cumple los requisitos del 'Test t'?", "¿Es válido sacar conclusiones del 'Test t'?")
  tabla_requisitos <- matrix("--------", 1, length(nombres1))
  colnames(tabla_requisitos) <- nombres1
  
  
  
  tabla_requisitos[1,1] <- colnames(input_base)
  tabla_requisitos[1,2] <- n_muestra
  tabla_requisitos[1,3] <- "Shapiro-Wilk"
  tabla_requisitos[1,4] <- cumplimiento_normalidad
  tabla_requisitos[1,5] <- cumplimiento_normalidad
  tabla_requisitos[1,6] <- cumplimiento_normalidad
  }
    
  
  # Frase Requisitos
  {
  frase_inicial_requisitos <- "El test t para una muestra tiene como requisitos que la variable 
                      en estudio posea distribución normal. <br/>
                      Paralelamente a la generación del test t RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de la variable con el 
                      test de normalidad de Shapiro-Wilk."
  
  frase_no_requisitos <- "Para el pool de datos de la muestra la variable '_mi_variable_' no presenta distribución 
                          normal, por lo tanto <b><u>no es válido sacar conclusiones del test t</b></u> 
                          indistintamente de los valores obtenidos.<br/>
                          Para poder sacar conclusiones válidas con respesto a una medida de posición, debiera 
                          dirijirse al test de Wilcoxon (una muestra) donde se pone a prueba el valor de la mediana."
  
  frase_si_requisitos <-  "Para el pool de datos la variable '_mi_variable_' presenta distribución 
                          normal, por lo tanto <b><u>es válido sacar conclusiones del test t</b></u>."
  
  
  if(cumplimiento_normalidad == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
    if(cumplimiento_normalidad == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
      
  frase_requisitos <- gsub("_mi_variable_", colnames(input_base)[1], frase_requisitos)
  }
      
  # Tabla resumen  
  {
    
  nombres2 <- c("Variable",  # 1
               "n",  #  2
               "Test", # 3
               "Media muestral (valor observado)",  # 4
               "Media poblacional (Valor esperado bajo hipótesis)",  # 5
               "Tipo de prueba", # 6
               "Estadístico (t)",  # 7
               "Grados de Libertad", # 8
               "Valor p", # 9
               "Alfa", # 10
               "Decisión", #11
               "¿Existen diferencias entre la media muestral observada y la 
                   media poblacional esperada bajo hipótesis?" # 12
  )
  tabla_resumen <- matrix("--------", 1, length(nombres2))
  colnames(tabla_resumen) <- nombres2
  
  tabla_resumen[1, 1] <- colnames(input_base)[1]
  tabla_resumen[1, 2] <- n_muestra
  tabla_resumen[1, 3] <- "Test t (una muestra)"
  tabla_resumen[1, 4] <- media_obs_externa
  tabla_resumen[1, 5] <- mu_esp_externa
  tabla_resumen[1, 6] <- tipo_de_prueba
  tabla_resumen[1, 7] <- estadistico_obs_externo
  tabla_resumen[1, 8] <- gl_externo
  tabla_resumen[1, 9] <- valor_p_externo
  tabla_resumen[1,10] <- input_alfa
  tabla_resumen[1,11] <- decision
  tabla_resumen[1,12] <- respuesta
  # 
  
  }
  
  SALIDA_ARMADA <- list()

  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 




Test_1C_TestWilcoxon_UnaMuestra <- function(input_base = NULL, 
                                     input_tipo_prueba = "two.sided", 
                                     input_mediana_ho = 0, 
                                     input_decimales = 2, 
                                     input_alfa = 0.05){
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra <- nrow(input_base)
  
  
  # Test de Wilcoxon (una muestra)  
  test_w <- suppressWarnings(
    wilcox.test(x = input_base[,1], 
                alternative = input_tipo_prueba, 
                mu = input_mediana_ho,
                paired = FALSE,
                conf.level = confianza,
                exact = FALSE,
                correct = FALSE,
                conf.int = TRUE)
  )
  
 
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estimador observado
  estadistico_obs_interno <- test_w$statistic
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estiamdores
  mu_esp_interna <- input_mediana_ho
  mu_esp_externa <- mu_esp_interna # Aproposito sin redondear
  mediana_obs_interna <- median(input_base[,1])
  mediana_obs_externa <- round2(mediana_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- "No corresponde"
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- test_w$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de mediana poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    
    
    frase_estadistica <- gsub("_mi_variable_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mediana_obs_", mediana_obs_externa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mu_esp_", mu_esp_externa, frase_estadistica)
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la mediana poblacional de la variable '_mi_variable_' es igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la mediana problacional de la variable '_mi_variable_' es diferente de _mi_mu_esp_."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la mediana poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la mediana poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la mediana poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la mediana poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    
    frase_juego_hipotesis <- gsub("_mi_variable_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_mu_esp_", mu_esp_externa, frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  
  
   # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Test", # 3
                  "Mediana muestral (valor observado)",  # 4
                  "Mediana poblacional (Valor esperado bajo hipótesis)",  # 5
                  "Tipo de prueba", # 6
                  "Estadístico (V)",  # 7
                  "Grados de Libertad", # 8
                  "Valor p", # 9
                  "Alfa", # 10
                  "Decisión", #11
                  "¿Existen diferencias entre la mediana muestral observada y la 
                   mediana poblacional esperada bajo hipótesis?" # 12
    )
    tabla_resumen <- matrix("--------", 1, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[1]
    tabla_resumen[1, 2] <- n_muestra
    tabla_resumen[1, 3] <- "Test de Wilcoxon (una muestra)"
    tabla_resumen[1, 4] <- mediana_obs_externa
    tabla_resumen[1, 5] <- mu_esp_externa
    tabla_resumen[1, 6] <- tipo_de_prueba
    tabla_resumen[1, 7] <- estadistico_obs_externo
    tabla_resumen[1, 8] <- gl_externo
    tabla_resumen[1, 9] <- valor_p_externo
    tabla_resumen[1,10] <- input_alfa
    tabla_resumen[1,11] <- decision
    tabla_resumen[1,12] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  

  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 



Test_1C_TestChiCuadrado_UnaMuestra <- function(input_base = NULL, 
                                     input_tipo_prueba = "two.sided", 
                                     input_varianza_ho = 1, 
                                     input_decimales = 2, 
                                     input_alfa = 0.05){
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra <- nrow(input_base)
  
  # Grados de libertad
  gl_interno <- n_muestra - 1
  gl_externo <- gl_interno
  
  # Varianzas
  varianza_obs_interna <- var(input_base[,1])
  varianza_obs_externa <- round2(varianza_obs_interna, input_decimales)
  varianza_esp_interna <- input_varianza_ho
  varianza_esp_externa <- varianza_esp_interna # Sin redondear a proposito
  
  # Estadisticos
  estadistico_obs_interno <- (gl_interno*varianza_obs_interna)/varianza_esp_interna
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  # Test de Normalidad
  test_normalidad <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base,
                                                        input_decimales = input_decimales,
                                                        input_alfa = input_alfa)
  
  # Cumplimiento de normalidad
  cumplimiento_normalidad <- test_normalidad$tabla_resumen[1,8]
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  

  
  # Test Chi Cuadrado (una muestra)
  # Valor p
  if(input_tipo_prueba == "two.sided") { 
    
    # Calculamos un valor p hacia la cola mas pequenia
    valor_p_interno <- pchisq(q = varianza_obs_interna, 
                      df = gl_interno, 
                      ncp = 0,
                      lower.tail = T)
    
    # Si es mayor a 0.5, calculamos el valor p pero hacia la cola mas grande.
    if(valor_p_interno > 0.5) valor_p_interno <- pchisq(q = varianza_obs_interna, 
                                                        df = gl_interno, 
                                                        ncp = 0, 
                                                        lower.tail = F)
    
    
    # Y ahora multiplicamos por dos
    valor_p_interno <- valor_p_interno*2
    
  } else
    if(input_tipo_prueba == "less") { 
      valor_p_interno <- pchisq(q = varianza_obs_interna, 
                               df = gl_interno, 
                               ncp = 0, 
                               lower.tail = T)
      
    } else
      if(input_tipo_prueba == "greater") {
        valor_p_interno <- valor_p_interno <- pchisq(q = varianza_obs_interna, 
                                                     df = gl_interno, 
                                                     ncp = 0,
                                                     lower.tail = F)
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La varianza muestral observada (_mi_varianza_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de varianza poblacional esperado bajo hipótesis (_mi_varianza_esp_)."
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
    
    
    
    frase_estadistica <- gsub("_mi_variable_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_varianza_obs_", varianza_obs_externa, frase_estadistica)
    frase_estadistica <- gsub("_mi_varianza_esp_", varianza_esp_externa, frase_estadistica)
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la varianza poblacional de la variable '_mi_variable_' es igual a _mi_varianza_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la varianza problacional de la variable '_mi_variable_' es diferente de _mi_varianza_esp_."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la varianza poblacional de la variable '_mi_variable_' es mayor o igual a _mi_varianza_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la varianza poblacional de la variable '_mi_variable_' es menor a _mi_varianza_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la varianza poblacional de la variable '_mi_variable_' es menor o igual a _mi_varianza_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la varianza poblacional de la variable '_mi_variable_' es mayor a _mi_varianza_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    
    frase_juego_hipotesis <- gsub("_mi_variable_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_varianza_esp_", varianza_esp_externa, frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable", "n", "Test de Normalidad", "¿Es normal la variable?", 
                  "¿Cumple los requisitos del 'Test t'?", "¿Es válido sacar conclusiones del 'Test t'?")
    tabla_requisitos <- matrix("--------", 1, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    
    tabla_requisitos[1,1] <- colnames(input_base)
    tabla_requisitos[1,2] <- n_muestra
    tabla_requisitos[1,3] <- "Shapiro-Wilk"
    tabla_requisitos[1,4] <- cumplimiento_normalidad
    tabla_requisitos[1,5] <- cumplimiento_normalidad
    tabla_requisitos[1,6] <- cumplimiento_normalidad
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "El test Chi Cuadrado para una muestra tiene como requisitos que la variable 
                      en estudio posea distribución normal. <br/>
                      Paralelamente a la generación del test t RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de la variable con el 
                      test de normalidad de Shapiro-Wilk."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra la variable '_mi_variable_' no presenta distribución 
                          normal, por lo tanto <b><u>no es válido sacar conclusiones del test Chi Cuadrado (una muestra)</b></u> 
                          indistintamente de los valores obtenidos."
    
    frase_si_requisitos <-  "Para el pool de datos la variable '_mi_variable_' presenta distribución 
                          normal, por lo tanto <b><u>es válido sacar conclusiones del test Chi Cuadrado (una muestra)</b></u>."
    
    
    if(cumplimiento_normalidad == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_normalidad == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_variable_", colnames(input_base)[1], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Test", # 3
                  "Varianza muestral (valor observado)",  # 4
                  "Varianza poblacional (Valor esperado bajo hipótesis)",  # 5
                  "Tipo de prueba", # 6
                  "Estadístico (Chi Cuadrado)",  # 7
                  "Grados de Libertad", # 8
                  "Valor p", # 9
                  "Alfa", # 10
                  "Decisión", #11
                  "¿Existen diferencias entre la varianza muestral observada y la 
                   varianza poblacional esperada bajo hipótesis?" # 12
    )
    tabla_resumen <- matrix("--------", 1, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[1]
    tabla_resumen[1, 2] <- n_muestra
    tabla_resumen[1, 3] <- "Test Chi Cuadrado (una muestra)"
    tabla_resumen[1, 4] <- varianza_obs_externa
    tabla_resumen[1, 5] <- varianza_esp_externa
    tabla_resumen[1, 6] <- tipo_de_prueba
    tabla_resumen[1, 7] <- estadistico_obs_externo
    tabla_resumen[1, 8] <- gl_externo
    tabla_resumen[1, 9] <- valor_p_externo
    tabla_resumen[1,10] <- input_alfa
    tabla_resumen[1,11] <- decision
    tabla_resumen[1,12] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 

###############################


Test_2C_TestHomogeneidadDeVarianzas_Fisher <- function(input_base = NULL, 
                                                       input_tipo_prueba = "two.sided", 
                                                       input_cociente_ho = 1, 
                                                       input_decimales = 2, 
                                                       input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_cociente_ho
  if(is.null(input_cociente_ho)) input_cociente_ho <- 1
  if (input_cociente_ho != 1) input_cociente_ho <- 1
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para ratio = 1.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  vr1 <- na.omit(input_base[,1])
  vr2 <- na.omit(input_base[,2])
  
  # Varianzas
  var1_interna <- var(vr1)
  var1_externa <- round2(var1_interna, input_decimales)
  var2_interna <- var(vr2)
  var2_externa <- round2(var2_interna, input_decimales)
  
  varianzas_internas <- c(var1_interna, var2_interna)
  names(varianzas_internas) <- colnames(input_base)
  
  varianzas_externas <- round2(varianzas_internas, input_decimales)

  # Varianzas ordenadas de menor a mayor
  varianzas_ordenadas_externas <- sort(varianzas_externas, decreasing = FALSE)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra1 <- length(vr1)
  n_muestra2 <- length(vr2)
  n_fusion <- paste0(n_muestra1, " y ", n_muestra2)
  
  
  # Test de Normalidad 01
  test_normalidad_01 <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base[1],
                                                           input_decimales = input_decimales,
                                                           input_alfa = input_alfa)
  
  
  # Test de Normalidad 02
  test_normalidad_02 <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base[2],
                                                           input_decimales = input_decimales,
                                                           input_alfa = input_alfa)
  
  
  # Cumplimiento de normalidad
  cumplimiento_normalidad_01 <- test_normalidad_01$tabla_resumen[1,8]
  cumplimiento_normalidad_02 <- test_normalidad_02$tabla_resumen[1,8]
  
  # Cumplimiento General
  if(cumplimiento_normalidad_01 == "Si" && cumplimiento_normalidad_02 == "Si") cumplimiento_general <- "Si" else
    if(cumplimiento_normalidad_01 == "No" | cumplimiento_normalidad_02 == "No") cumplimiento_general <- "No"

  # Test F
  test_f <- var.test(x = vr1,
                     y = vr2,
                     ratio = input_cociente_ho, 
                     alternative = input_tipo_prueba, 
                     conf.level = confianza)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estimadores
  cociente_esp_interno <- input_cociente_ho
  cociente_esp_externo <- cociente_esp_interno # Aproposito sin redondear
  cociente_obs_interno <- test_f$estimate
  cociente_obs_externo <- round2(cociente_obs_interno, input_decimales)
  
  
  # Estadistico observado
  estadistico_obs_interno <- test_f$statistic 
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  
  
  # Grados de Libertad
  gl1_interno <- test_f$parameter[1]
  gl2_interno <- test_f$parameter[2]
  gl1_externo <- gl1_interno # A proposito va asi, sin redondear
  gl2_externo <- gl2_interno # A proposito va asi, sin redondear
  gl_fusion_interno <- paste0(gl1_interno, " y ", gl2_interno)
  gl_fusion_externo <- gl_fusion_interno # A proposito va asi, sin redondear
  
  # Valor p 
  valor_p_interno <- test_f$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
  
  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"
  
  
  # Frases segun valor p
  {
    
    
    # Nota de David: Se opto por el momento solo realizar la prueba de homogeneidad
    #                de varianzas de manera bilatereal para un valor de cociente (ratio) igual a 1.
    #                Si se amplia hacia formas unilaterales y a otros valores de ratio, habrá que 
    #                cambiar las frases de salida.
    
    # Algun inconveniente
    frase0_v1 <- "No pudo obtenerse un valor p."
    
 
      frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                     por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                     Las varianzas son estadísticamente homogéneas (homocedasticidad).<br/>
                     Las varianzas son estadísticamente iguales."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                    por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                    Las varianzas son estadísticamente homogéneas (varianzas homocedásticas).<br/>
                    Las varianzas son estadísticamente iguales."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                    por lo tanto <b><u>se rechaza la Ho</b></u>.<br/>
                    Las varianzas son estadísticamente heterogéneas (varianzas heterocedásticas).<br/>
                    Las varianzas son estadísticamente diferentes.<br/>
                    La varianza de la variable '_mi_variableMayor_' (_mi_varianzaMayor_) es estadísticamente 
                    mayor al valor de la varianza de la variable '_mi_variableMenor_' (_mi_varianzaMenor_).<br/>
                    La varianza de la variable '_mi_variableMenor_' (_mi_varianzaMenor_) es estadísticamente 
                    menor al valor de la varianza de la variable '_mi_variableMayor_' (_mi_varianzaMayor_).<br/>"
  
    
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
    
    
      frase_estadistica <- gsub("_mi_varianzaMenor_", varianzas_ordenadas_externas[1], frase_estadistica)
      frase_estadistica <- gsub("_mi_varianzaMayor_", varianzas_ordenadas_externas[2], frase_estadistica)
      frase_estadistica <- gsub("_mi_variableMenor_", names(varianzas_ordenadas_externas)[1], frase_estadistica)
      frase_estadistica <- gsub("_mi_variableMayor_", names(varianzas_ordenadas_externas)[1], frase_estadistica)
      
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)

    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> las varianzas de las variables '_mi_variable1_' y '_mi_variable2_' 
                                son homogéneas entre si (homocedasticidad de varianzas).<br/>
                               <b>Hipótesis Alternativa (Hi):</b> las varianzas de las variables '_mi_variable1_' y '_mi_variable2_' 
                                no son homogéneas entre si (heterocedasticidad de varianzas)."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    

    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable", "n", "Test de Normalidad", "¿Es normal la variable?", 
                  "¿Se cumplen los requisitos del 'Test de Homogeneidad de Varianzas de Fisher'?", "¿Es válido sacar conclusiones del 'Test de Homogeneidad de Varianzas de Fisher'?")
    tabla_requisitos <- matrix("--------", 2, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    
    tabla_requisitos[1,1] <- colnames(input_base)[1]
    tabla_requisitos[2,1] <- colnames(input_base)[2]
    tabla_requisitos[1,2] <- n_muestra1
    tabla_requisitos[2,2] <- n_muestra2
    tabla_requisitos[1,3] <- "Shapiro-Wilk"
    tabla_requisitos[2,3] <- "Shapiro-Wilk"
    tabla_requisitos[1,4] <- cumplimiento_normalidad_01
    tabla_requisitos[2,4] <- cumplimiento_normalidad_02
    tabla_requisitos[1,5] <- cumplimiento_general
    tabla_requisitos[1,6] <- cumplimiento_general
    
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "El test de Homogeneidad de Varianzas de Fisher tiene como requisitos que ambas variables 
                                  en estudio posean distribución normal. <br/>
                      Paralelamente a la generación del test de Homogeneidad de Varianzas de Fisher RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de ambas variables con el 
                      test de normalidad de Shapiro-Wilk."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra no se cumple que simultáneamente ambas variables 
                            ('_mi_variable1_' y '_mi_variable2_') presenten distribución normal, por lo 
                            tanto <b><u>no es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Fisher</b></u> indistintamente de los valores obtenidos."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra se cumple que simultáneamente ambas variables 
                            ('_mi_variable1_' y '_mi_variable2_') presenten distribución normal, por lo 
                            tanto <b><u>es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Fisher</b></u>."
    
    
    if(cumplimiento_general == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_general == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_variable1_", colnames(input_base)[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_variable2_", colnames(input_base)[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Test", # 3
                  "Varianza muestral (valor observado)",  # 4
                  "Cociente de varianzas (valor observado)", #5
                  "Cociente de varianzas (valor poblacional esperado bajo hipótesis)",  # 6
                  "Tipo de prueba", # 7
                  "Estadístico (F)",  # 8
                  "Grados de Libertad", # 9
                  "Valor p", # 10
                  "Alfa", # 11
                  "Decisión", #12
                  "¿Son las varianzas diferentes entre si?" # 13
    )
    
    tabla_resumen <- matrix("--------", 2, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[1]
    tabla_resumen[2, 1] <- colnames(input_base)[2]
    tabla_resumen[1, 2] <- n_muestra1
    tabla_resumen[2, 2] <- n_muestra2
    tabla_resumen[1, 3] <- "Test de homogeneidad de varianzas de Fisher"
    tabla_resumen[1, 4] <- var1_externa
    tabla_resumen[2, 4] <- var2_externa
    tabla_resumen[1, 5] <- cociente_obs_externo
    tabla_resumen[1, 6] <- cociente_esp_externo
    tabla_resumen[1, 7] <- tipo_de_prueba
    
    tabla_resumen[1, 8] <- estadistico_obs_externo
    tabla_resumen[1, 9] <- gl1_externo
    tabla_resumen[2, 9] <- gl2_externo
    tabla_resumen[1,10] <- valor_p_externo
    tabla_resumen[1,11] <- input_alfa
    tabla_resumen[1,12] <- decision
    tabla_resumen[1,13] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 


Test_2C_TestT_DosMuestras_Apareado <- function(input_base = NULL, 
                                     input_tipo_prueba = "two.sided", 
                                     input_media_ho = 0, 
                                     input_decimales = 2, 
                                     input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_media_ho
  if(is.null(input_media_ho)) input_media_ho <- 0
  if (input_media_ho != 0) input_media_ho <- 0
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para ratio = 1.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra <- nrow(input_base)
  vr_diferencia <- input_base[1] - input_base[2]
  
  # Test de Normalidad
  test_normalidad <- Test_1C_TestNormalidad_ShapiroWilk(input_base = vr_diferencia,
                                                        input_decimales = input_decimales,
                                                        input_alfa = input_alfa)
  
  # Cumplimiento de normalidad
  cumplimiento_normalidad <- test_normalidad$tabla_resumen[1,8]
  
  
  # Test t apareado
  the_test <- t.test(x = input_base[,1],
                   y = input_base[,2],
                   alternative = input_tipo_prueba, 
                   mu = input_media_ho,
                   paired = TRUE,
                   conf.level = confianza)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estadistico observado
  estadistico_obs_interno <- the_test$statistic
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estimadores
  mu_esp_interna <- input_media_ho
  mu_esp_externa <- mu_esp_interna # Aproposito sin redondear
  media_obs_interna <- mean(vr_diferencia[,1])
  media_obs_externa <- round2(media_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- the_test$parameter
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- the_test$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       La media muestral observada (_mi_media_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente igual a cero."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                        La media muestral observada (_mi_media_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente igual a cero."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                        La media muestral observada (_mi_media_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente distinta de cero."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    
    
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_media_obs_", media_obs_externa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mu_esp_", mu_esp_externa, frase_estadistica)
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de las diferencias las variables '_mi_variable1_' y '_mi_variable2_' es igual a cero.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de las diferencias las variables '_mi_variable1_' y '_mi_variable2_' es diferente a cero."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    
    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_mu_esp_", mu_esp_externa, frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable", "n", "Test de Normalidad", "¿Presenta distribución normal la variable diferencia?", 
                  "¿Se cumple el requisitos del 'Test t (Dos muestras apareadas)'?", "¿Es válido sacar conclusiones del 'Test t (Dos muestras apareadasa)'?")
    tabla_requisitos <- matrix("--------", 1, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    
    tabla_requisitos[1,1] <- paste0("Diferencia ('", colnames(input_base)[1], "' - '", colnames(input_base)[2], "')")
    tabla_requisitos[1,2] <- n_muestra
    tabla_requisitos[1,3] <- "Shapiro-Wilk"
    tabla_requisitos[1,4] <- cumplimiento_normalidad
    tabla_requisitos[1,5] <- cumplimiento_normalidad
    tabla_requisitos[1,6] <- cumplimiento_normalidad
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "A partir de las dos variables ingresadas RMedic calcula una nueva variable 
                                  que es la diferencia de cada par de datos, siendo cada valor de diferencia 
                                  el valor de la primera variable menos el valor de la segunda variable para cada 
                                  fila en que presentan datos en ambas variables. <br/>
                                  El test t para dos muestras apareadas se aplica finalmente sobre la variable 
                                  diferencia. El test tiene como requisitos que la variable diferencia presente 
                                  distribución normal. <br/> 
                                  Paralelamente a la generación del test t para dos muestras apareadas, RMedic 
                                  realiza a su vez la comprobación estadística de la normalidad de la variable diferencia 
                                  con el test de normalidad de Shapiro-Wilk."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra la variable diferencia no presenta distribución 
                          normal, por lo tanto <b><u>no es válido sacar conclusiones del test t para dos muestras apareadas </b></u> 
                          indistintamente de los valores obtenidos.<br/>
                          Para poder sacar conclusiones válidas con respesto a una medida de posición en la variable diferencia, debiera 
                          dirijirse al test de Wilcoxon (dos muestras apareadas) donde se pone a prueba el valor de la mediana de la diferencia."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra la variable diferencia presenta distribución 
                          normal, por lo tanto <b><u>es válido sacar conclusiones del test t para dos muestras apareadas</b></u>."
    
    
    if(cumplimiento_normalidad == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_normalidad == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_variable1_", colnames(input_base)[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_variable2_", colnames(input_base)[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Test", # 3
                  "Media muestral (valor observado)",  # 4
                  "Media poblacional (Valor esperado bajo hipótesis)",  # 5
                  "Tipo de prueba", # 6
                  "Estadístico (t)",  # 7
                  "Grados de Libertad", # 8
                  "Valor p", # 9
                  "Alfa", # 10
                  "Decisión", #11
                  "¿Existen diferencias entre la media muestral de las diferencias 
                  observada y la media poblacional esperada bajo hipótesis (cero)?" # 12
    )
    tabla_resumen <- matrix("--------", 1, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[1]
    tabla_resumen[1, 2] <- n_muestra
    tabla_resumen[1, 3] <- "Test t (dos muestras apareadas)"
    tabla_resumen[1, 4] <- media_obs_externa
    tabla_resumen[1, 5] <- mu_esp_externa
    tabla_resumen[1, 6] <- tipo_de_prueba
    tabla_resumen[1, 7] <- estadistico_obs_externo
    tabla_resumen[1, 8] <- gl_externo
    tabla_resumen[1, 9] <- valor_p_externo
    tabla_resumen[1,10] <- input_alfa
    tabla_resumen[1,11] <- decision
    tabla_resumen[1,12] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 


Test_2C_TestWilcoxon_DosMuestras_Apareado <- function(input_base = NULL, 
                                               input_tipo_prueba = "two.sided", 
                                               input_mediana_ho = 0, 
                                               input_decimales = 2, 
                                               input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_cociente_ho
  if(is.null(input_mediana_ho)) input_mediana_ho <- 0
  if (input_mediana_ho != 0) input_mediana_ho <- 0
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para mediana = 0.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra <- nrow(input_base)
  vr_diferencia <- input_base[1] - input_base[2]
  

  # Test Wilcoxon
  library("coin")
  the_test <- wilcox.test(x = input_base[,1],
                          y = input_base[,2], 
                          paired = TRUE, 
                          alternative = "two.sided",
                          conf.level = confianza,
                          exact = FALSE,
                          correct = FALSE,
                          conf.int = TRUE)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estadistico observado
  estadistico_obs_interno <- the_test$statistic
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estimadores
  mediana_esp_interna <- input_mediana_ho
  mediana_esp_externa <- mediana_esp_interna # Aproposito sin redondear
  mediana_obs_interna <- median(vr_diferencia[,1])
  mediana_obs_externa <- round2(mediana_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- "No corresponde" #the_test$parameter
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- the_test$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente igual a cero."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                        La mediana muestral observada (_mi_mediana_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente igual a cero."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                        La mediana muestral observada (_mi_mediana_obs_) de la diferencia entre las variables 
                       '_mi_variable1_' y '_mi_variable2_' es estadísticamente distinta de cero."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    
    
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mediana_obs_", mediana_obs_externa, frase_estadistica)
    frase_estadistica <- gsub("_mi_mediana_esp_", mediana_esp_externa, frase_estadistica)
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> la mediana poblacional de las diferencias las variables '_mi_variable1_' y '_mi_variable2_' es igual a cero.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la mediana poblacional de las diferencias las variables '_mi_variable1_' y '_mi_variable2_' es diferente a cero."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    
    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_mediana_esp_", mediana_esp_externa, frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_mediana_obs_", mediana_obs_externa, frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
 
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Test", # 3
                  "Mediana muestral (valor observado)",  # 4
                  "Mediana poblacional (Valor esperado bajo hipótesis)",  # 5
                  "Tipo de prueba", # 6
                  "Estadístico (V)",  # 7
                  "Grados de Libertad", # 8
                  "Valor p", # 9
                  "Alfa", # 10
                  "Decisión", #11
                  "¿Es la mediana muestral de las diferencias 
                  observadas distinta de cero?" # 12
    )
    tabla_resumen <- matrix("--------", 1, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- "Diferencia"
    tabla_resumen[1, 2] <- n_muestra
    tabla_resumen[1, 3] <- "Test Wilcoxon (dos muestras apareadas)"
    tabla_resumen[1, 4] <- mediana_obs_externa
    tabla_resumen[1, 5] <- mediana_esp_externa
    tabla_resumen[1, 6] <- tipo_de_prueba
    tabla_resumen[1, 7] <- estadistico_obs_externo
    tabla_resumen[1, 8] <- gl_externo
    tabla_resumen[1, 9] <- valor_p_externo
    tabla_resumen[1,10] <- input_alfa
    tabla_resumen[1,11] <- decision
    tabla_resumen[1,12] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  

  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function



Test_2C_TestHomogeneidadDeVarianzas_Bartlett <- function(input_base = NULL, 
                                                         input_decimales = 2, 
                                                         input_alfa = 0.05){
  
  
  
  
  
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para ratio = 1.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  vr1 <- na.omit(input_base[,1])
  vr2 <- na.omit(input_base[,2])
  
  # Varianzas
  var1_interna <- var(vr1)
  var1_externa <- round2(var1_interna, input_decimales)
  var2_interna <- var(vr2)
  var2_externa <- round2(var2_interna, input_decimales)
  
  varianzas_obs_internas <- c(var1_interna, var2_interna)
  names(varianzas_obs_internas) <- colnames(input_base)
  
  varianzas_obs_externas <- round2(varianzas_obs_internas, input_decimales)
  
  # Varianzas ordenadas de menor a mayor
  varianzas_ordenadas_externas <- sort(varianzas_obs_externas, decreasing = FALSE)
  
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  n_muestra1 <- length(vr1)
  n_muestra2 <- length(vr2)
  n_fusion <- paste0(n_muestra1, " y ", n_muestra2)
  VR <- c(vr1, vr2)
  FACTOR <- as.factor(c(rep("1", n_muestra1), rep("2", n_muestra2)))
  
  # Test de Normalidad 01
  test_normalidad_01 <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base[1],
                                                           input_decimales = input_decimales,
                                                           input_alfa = input_alfa)
  
  
  # Test de Normalidad 02
  test_normalidad_02 <- Test_1C_TestNormalidad_ShapiroWilk(input_base = input_base[2],
                                                           input_decimales = input_decimales,
                                                           input_alfa = input_alfa)
  
  
  # Cumplimiento de normalidad
  cumplimiento_normalidad_01 <- test_normalidad_01$tabla_resumen[1,8]
  cumplimiento_normalidad_02 <- test_normalidad_02$tabla_resumen[1,8]
  
  # Cumplimiento General
  if(cumplimiento_normalidad_01 == "Si" && cumplimiento_normalidad_02 == "Si") cumplimiento_general <- "Si" else
    if(cumplimiento_normalidad_01 == "No" | cumplimiento_normalidad_02 == "No") cumplimiento_general <- "No"
  
  
  # Test de Bartlett
  the_test <- bartlett.test(VR ~ FACTOR)
  
  
  
  
  # Estimadores
  estimadores_obs_internos <- c(var1_interna, var2_interna)
  estimadores_obs_externos <- c(var1_externa, var2_externa)
  
  
  # Estadistico observado
  estadistico_obs_interno <- the_test$statistic 
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  
  
  # Grados de Libertad
  gl_interno <- the_test$parameter
  gl_externo <- gl_interno # A proposito va asi, sin redondear
  
  # Valor p 
  valor_p_interno <- the_test$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
  
  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"
  
  
  # Frases segun valor p
  {
    
    
    # Algun inconveniente
    frase0_v1 <- "No pudo obtenerse un valor p."
    
    
    frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                     por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                     Las varianzas son estadísticamente homogéneas (homocedasticidad).<br/>
                     Las varianzas son estadísticamente iguales."
    
    
    frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                    por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                    Las varianzas son estadísticamente homogéneas (varianzas homocedásticas).<br/>
                    Las varianzas son estadísticamente iguales."
    
    
    frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                    por lo tanto <b><u>se rechaza la Ho</b></u>.<br/>
                    Las varianzas son estadísticamente heterogéneas (varianzas heterocedásticas).<br/>
                    Las varianzas son estadísticamente diferentes.<br/>
                    La varianza de la variable '_mi_variableMayor_' (_mi_varianzaMayor_) es estadísticamente 
                    mayor al valor de la varianza de la variable '_mi_variableMenor_' (_mi_varianzaMenor_).<br/>
                    La varianza de la variable '_mi_variableMenor_' (_mi_varianzaMenor_) es estadísticamente 
                    menor al valor de la varianza de la variable '_mi_variableMayor_' (_mi_varianzaMayor_).<br/>"
    
    
    
    # Seleccion de Frase Estadistica
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_estadistica <- frase0_v1 else
      
      if (valor_p_interno > input_alfa) frase_estadistica <- frase1_v1 else
        if (valor_p_interno == input_alfa) frase_estadistica <- frase1_v2 else
          if (valor_p_interno < input_alfa) frase_estadistica <- frase1_v3
    
    
    frase_estadistica <- gsub("_mi_varianzaMenor_", varianzas_ordenadas_externas[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_varianzaMayor_", varianzas_ordenadas_externas[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_variableMenor_", names(varianzas_ordenadas_externas)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variableMayor_", names(varianzas_ordenadas_externas)[1], frase_estadistica)
    
    
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    
    frase_juego_hipotesis <-  "<b>Hipótesis Nula (Ho):</b> las varianzas de ambas variables son 
                              estadístiamente iguales (homogeneidad de varianzas).<br/>
                               <b>Hipótesis Alternativa (Hi):</b> las varianzas de ambas variables son 
                               estadísticamente difernetes (heterogeneidad de varianzas)."
    
    
    
    
    
    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
 
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable", 
                  "n", 
                  "Test de Normalidad", 
                  "¿Es normal la variable?", 
                  "¿Se cumplen los requisitos del 'Test de Homogeneidad de Varianzas de Bartlett'?", 
                  "¿Es válido sacar conclusiones del 'Test de Homogeneidad de Varianzas de Bartlett'?")
    tabla_requisitos <- matrix("--------", 2, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    tabla_requisitos[1,1] <- colnames(input_base)[1]
    tabla_requisitos[2,1] <- colnames(input_base)[2]
    tabla_requisitos[1,2] <- n_muestra1
    tabla_requisitos[2,2] <- n_muestra2
    tabla_requisitos[1,3] <- "Shapiro-Wilk"
    tabla_requisitos[2,3] <- "Shapiro-Wilk"
    tabla_requisitos[1,4] <- cumplimiento_normalidad_01
    tabla_requisitos[2,4] <- cumplimiento_normalidad_02
    tabla_requisitos[1,5] <- cumplimiento_general
    tabla_requisitos[1,6] <- cumplimiento_general
    
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "El test de Homogeneidad de Varianzas de Bartlett tiene como requisitos que ambas variables 
                                  en estudio posean distribución normal. <br/>
                      Paralelamente a la generación del test de Homogeneidad de Varianzas de Bartlett, RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de ambas variables con el 
                      test de normalidad de Shapiro-Wilk."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra no se cumple que simultáneamente ambas variables 
                            ('_mi_variable1_' y '_mi_variable2_') presenten distribución normal, por lo 
                            tanto <b><u>no es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Bartlett</b></u> indistintamente de los valores obtenidos."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra se cumple que simultáneamente ambas variables 
                            ('_mi_variable1_' y '_mi_variable2_') presenten distribución normal, por lo 
                            tanto <b><u>es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Bartlett</b></u>."
    
    
    if(cumplimiento_general == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_general == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_variable1_", colnames(input_base)[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_variable2_", colnames(input_base)[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable",  # 1
                  "n",  #  2
                  "Varianza muestral (valor observado)", # 3
                  "Test",  # 4
                  "Estadístico (K^2)",  # 5
                  "Grados de Libertad", # 6
                  "Valor p", # 7
                  "Alfa", # 8
                  "Decisión", # 9
                  "¿Son las varianzas diferentes entre si?" # 10
    )
    
    tabla_resumen <- matrix("--------", 2, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[, 1] <- colnames(input_base)
    tabla_resumen[1, 2] <- n_muestra1
    tabla_resumen[2, 2] <- n_muestra2
    tabla_resumen[, 3] <- varianzas_obs_externas
      
    tabla_resumen[1, 4] <- "Test de homogeneidad de varianzas de Bartlett"
    tabla_resumen[1, 5] <- estadistico_obs_externo
    tabla_resumen[1, 6] <- gl_externo
    tabla_resumen[1,7] <- valor_p_externo
    tabla_resumen[1,8] <- input_alfa
    tabla_resumen[1,9] <- decision
    tabla_resumen[1,10] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function


###########################################################################



Test_QC_TestNormalidad_ShapiroWilk_Particionado <- function(input_base = NULL,
                                               input_decimales = 2, 
                                               input_alfa = 0.05){
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  VR <- input_base[,2]
  FACTOR <- input_base[,1]

  # Frases Juego de Hipotesis
  {
    frase_juego_hipotesis <-  "<b>Hipótesis Nula (Ho):</b> la variable '_mi_variable_' posee distribución normal.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la variable '_mi_variable_' NO posee distribución normal."
    
    frase_juego_hipotesis <- gsub("_mi_variable_", colnames(input_base)[2], frase_juego_hipotesis)
    
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Tamanio de la muestra
  n_muestra <- nrow(input_base)
  tabla_fa <- table(input_base[,1])
  cantidad_categorias <- length(tabla_fa)

  # Vamos a agregar al analisis general
  cantidad_analisis <- cantidad_categorias + 1
  
  # Test de normalidad de Shapiro-Wilk
  The_Test <- list()
  
  for (k in 1:cantidad_analisis){
    
    if(k <= cantidad_categorias) {
    vr_seleccionado <- as.data.frame(VR[FACTOR == names(tabla_fa)[k]])
    colnames(vr_seleccionado) <- names(tabla_fa)[k]
    } else {
      vr_seleccionado <- input_base[2]
    }
    
    
  The_Test[[k]] <- Test_1C_TestNormalidad_ShapiroWilk(input_base = vr_seleccionado,
                                     input_decimales = input_decimales, 
                                     input_alfa = input_alfa)
  

  }
  
  
  
  
  
 # Tabla Resumen
  {
 
  nombres <- colnames(The_Test[[1]]$tabla_resumen)
  
  tabla_resumen <- matrix("--------", cantidad_analisis, length(nombres))
  colnames(tabla_resumen) <- nombres
  
  for (k in 1:cantidad_analisis){
    
    tabla_resumen[k,] <- The_Test[[k]]$tabla_resumen
  }
  agregado1 <- c(rep(colnames(input_base)[2], cantidad_analisis))
  tabla_resumen <- cbind(agregado1, tabla_resumen)
  tabla_resumen[nrow(tabla_resumen), 2] <- c("---General---")
  
  colnames(tabla_resumen)[1] <- "Variable Numérica"
  colnames(tabla_resumen)[2] <- paste0("Categorías de la variable categórica '",
                                       colnames(input_base)[1], "'")
  
 
  }
  
  # frase_estadistica <- "Esta es la frase estadística"
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen

  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
}




Test_QC_TestHomogeneidadDeVarianzas_Fisher <- function(input_base = NULL, 
                                                       input_tipo_prueba = "two.sided", 
                                                       input_cociente_ho = 1, 
                                                       input_decimales = 2, 
                                                       input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_cociente_ho
  if(is.null(input_cociente_ho)) input_cociente_ho <- 1
  if (input_cociente_ho != 1) input_cociente_ho <- 1
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para ratio = 1.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  VR <- input_base[,2]
  FACTOR <- input_base[,1]
  
  # Varianzas
  varianzas_obs_internas <- tapply(VR, FACTOR, var)
  varianzas_obs_externas <- round2(varianzas_obs_internas, input_decimales)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  tabla_fa <- table(input_base[,1])
  n_grupo1 <- tabla_fa[1]
  n_grupo2 <- tabla_fa[2]
  n_fusion <- paste0(n_grupo1, " y ", n_grupo2)
  categorias <- names(tabla_fa)
  
  # Test de Normalidad Particionado
  test_normalidad_particionado <- Test_QC_TestNormalidad_ShapiroWilk_Particionado(input_base = input_base,
                                                           input_decimales = input_decimales,
                                                           input_alfa = input_alfa)
 
  

  
  # Cumplimiento de normalidad
  cumplimiento_normalidad_01 <- test_normalidad_particionado$tabla_resumen[1,9]
  cumplimiento_normalidad_02 <- test_normalidad_particionado$tabla_resumen[2,9]
  
  # Cumplimiento General
  if(cumplimiento_normalidad_01 == "Si" && cumplimiento_normalidad_02 == "Si") cumplimiento_general <- "Si" else
    if(cumplimiento_normalidad_01 == "No" | cumplimiento_normalidad_02 == "No") cumplimiento_general <- "No"
  
  # Test F
  test_f <- var.test(VR ~ FACTOR,
                     ratio = input_cociente_ho, 
                     alternative = input_tipo_prueba, 
                     conf.level = confianza)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estimadores
  cociente_esp_interno <- input_cociente_ho
  cociente_esp_externo <- cociente_esp_interno # Aproposito sin redondear
  cociente_obs_interno <- test_f$estimate
  cociente_obs_externo <- round2(cociente_obs_interno, input_decimales)
  
  
  # Estadistico observado
  estadistico_obs_interno <- test_f$statistic 
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  
  
  # Grados de Libertad
  gl1_interno <- test_f$parameter[1]
  gl2_interno <- test_f$parameter[2]
  gl1_externo <- gl1_interno # A proposito va asi, sin redondear
  gl2_externo <- gl2_interno # A proposito va asi, sin redondear
  gl_fusion_interno <- paste0(gl1_interno, " y ", gl2_interno)
  gl_fusion_externo <- gl_fusion_interno # A proposito va asi, sin redondear
  
  # Valor p 
  valor_p_interno <- test_f$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
  
  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"
  
  
  # Frases segun valor p
  {
    
    
    # Nota de David: Se opto por el momento solo realizar la prueba de homogeneidad
    #                de varianzas de manera bilatereal para un valor de cociente (ratio) igual a 1.
    #                Si se amplia hacia formas unilaterales y a otros valores de ratio, habrá que 
    #                cambiar las frases de salida.
    
    # Algun inconveniente
    frase0_v1 <- "No pudo obtenerse un valor p."
    
    
    frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       Las varianzas son estadísticamente homogéneas (varianzas homocedásticidad)."
    
    
    frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       Las varianzas son estadísticamente homogéneas (varianzas homocedásticas)."
    
    
    frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                       Las varianzas son estadísticamente heterogéneas (varianzas heterocedásticas)."
    
    
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
    
    
    
    frase_estadistica <- gsub("_mi_categoria1_", categorias[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria2_", categorias[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral <-  "<b>Hipótesis Nula (Ho):</b> las varianzas de la variable '_mi_variable1_' para las categorías 
                                '_mi_categoria1_' y '_mi_categoria2_' son homogéneas entre si (homocedasticidad de varianzas).<br/>
                               <b>Hipótesis Alternativa (Hi):</b> las varianzas de las variable '_mi_variable1_' para las 
                               categorías y '_mi_categoria1_' y '_mi_categoria2_' no son homogéneas entre si (heterocedasticidad de varianzas)."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis <- frase_juego_bilateral else
      if(input_tipo_prueba == "less") frase_juego_hipotesis <- frase_juego_izquierda else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis <- frase_juego_derecha
    

    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis)
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable Numérica",
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"),
                  "n", "Test de Normalidad", "¿Es normal la variable?", 
                  "¿Se cumplen los requisitos del 'Test de Homogeneidad de Varianzas de Fisher'?", "¿Es válido sacar conclusiones del 'Test de Homogeneidad de Varianzas de Fisher'?")
    tabla_requisitos <- matrix("--------", 2, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    # Fila 1 para la categoria 1
    tabla_requisitos[1,1] <- colnames(input_base)[2]
    tabla_requisitos[1,2] <- categorias[1]
    tabla_requisitos[1,3] <- n_grupo1
    tabla_requisitos[1,4] <- "Shapiro-Wilk"
    tabla_requisitos[1,5] <- cumplimiento_normalidad_01
    tabla_requisitos[1,6] <- cumplimiento_general
    tabla_requisitos[1,7] <- cumplimiento_general
    
    # Fila 2 para la categoria 2
    tabla_requisitos[2,1] <- colnames(input_base)[2]
    tabla_requisitos[2,2] <- categorias[2]
    tabla_requisitos[2,3] <- n_grupo2
    tabla_requisitos[2,4] <- "Shapiro-Wilk"
    tabla_requisitos[2,5] <- cumplimiento_normalidad_02
    # # tabla_requisitos[2,6] <- cumplimiento_general
    # # tabla_requisitos[1,7] <- cumplimiento_general
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "El test de Homogeneidad de Varianzas de Fisher es aplicable solo sobre dos categorías teniendo 
                                  ambas categorías distribución normal.<br/>
                      Paralelamente a la generación del test de Homogeneidad de Varianzas de Fisher RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de ambas categorías con el 
                      test de normalidad de Shapiro-Wilk."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra no se cumple que simultáneamente ambas categorías 
                            ('_mi_categoria1_' y '_mi_categoria2_') presenten distribución normal, por lo 
                            tanto <b><u>no es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Fisher</b></u> indistintamente de los valores obtenidos."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra se cumple que simultáneamente ambas categorías 
                            ('_mi_categoria1_' y '_mi_categoria2_') presenten distribución normal, por lo 
                            tanto <b><u>es válido sacar conclusiones del test 
                            de Homogeneidad de Varianzas de Fisher</b></u>."
    
    
    if(cumplimiento_general == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_general == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_categoria1_", categorias[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_categoria2_", categorias[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable Numérica",  # 1
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"), #2
                  "n",  #  3
                  "Test", # 4
                  "Varianza muestral (valor observado)",  # 5
                  "Cociente de varianzas (valor observado)", #6
                  "Cociente de varianzas (valor poblacional esperado bajo hipótesis)",  # 7
                  "Tipo de prueba", # 8
                  "Estadístico (F)",  # 9
                  "Grados de Libertad", # 10
                  "Valor p", # 11
                  "Alfa", # 12
                  "Decisión", #13
                  "¿Son las varianzas diferentes entre si?" # 14
    )
    
    tabla_resumen <- matrix("--------", 2, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[2]
    tabla_resumen[2, 1] <- colnames(input_base)[2]
    tabla_resumen[1, 2] <- categorias[1]
    tabla_resumen[2, 2] <- categorias[2]
    
    tabla_resumen[1, 3] <- n_grupo1
    tabla_resumen[2, 3] <- n_grupo2
    
    tabla_resumen[1, 4] <- "Test de homogeneidad de varianzas de Fisher"
    tabla_resumen[1, 5] <- varianzas_obs_externas[1]
    tabla_resumen[2, 5] <- varianzas_obs_externas[2]
    tabla_resumen[1, 6] <- cociente_obs_externo
    tabla_resumen[1, 7] <- cociente_esp_externo
    tabla_resumen[1, 8] <- tipo_de_prueba
    
    tabla_resumen[1, 9] <- estadistico_obs_externo
    tabla_resumen[1, 10] <- gl1_externo
    tabla_resumen[2, 10] <- gl2_externo
    tabla_resumen[1,11] <- valor_p_externo
    tabla_resumen[1,12] <- input_alfa
    tabla_resumen[1,13] <- decision
    tabla_resumen[1,14] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function



Test_QC_TestT_DosMuestras_Independientes <- function(input_base = NULL, 
                                               input_tipo_prueba = "two.sided", 
                                               input_media_ho = 0, 
                                               input_decimales = 2, 
                                               input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_media_ho
  if(is.null(input_media_ho)) input_media_ho <- 0
  if (input_media_ho != 0) input_media_ho <- 0
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para media de las
  # diferencias = 0.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  
  # n de cada grupo
  tabla_fa <- table(input_base[,1])
  categorias <- names(tabla_fa)
  n_grupo1 <- tabla_fa[1]
  n_grupo2 <- tabla_fa[2]
  
  # Objetos intermedios
  FACTOR <- input_base[,1]
  VR <- input_base[,2]


  # Medias
  medias_obs_internas <- tapply(VR, FACTOR, mean)
  medias_obs_externas <- round2(medias_obs_internas, input_decimales)
  
  # Medias ordenadas de menor a mayor
  medias_ordenadas_externas <- sort(medias_obs_externas, decreasing = FALSE)

  # Test de Homogeneidad
  test_homogeneidad <- Test_QC_TestHomogeneidadDeVarianzas_Fisher(input_base =input_base, 
                                             input_tipo_prueba = input_tipo_prueba, 
                                             input_cociente_ho = 0, 
                                             input_decimales = input_decimales, 
                                             input_alfa = input_alfa)
  
  
  # Test de Normalidad Particionado
  test_normalidad_particionado <- Test_QC_TestNormalidad_ShapiroWilk_Particionado( input_base = input_base,
                                                                         input_decimales = input_decimales,
                                                                         input_alfa = input_alfa)


  # Cumplimiento de normalidad y homogeneidad
  cumplimiento_normalidad_01 <- test_normalidad_particionado$tabla_resumen[1,9]
  cumplimiento_normalidad_02 <- test_normalidad_particionado$tabla_resumen[2,9]
  cumplimiento_homogeneidad <- test_homogeneidad$tabla_resumen[1,14]
  
  # Cumplimiento normalidad ambas
  if(cumplimiento_normalidad_01 == "Si"){
    
    if(cumplimiento_normalidad_02 == "Si") cumplimiento_normalidad_ambas <- "Si" else cumplimiento_normalidad_ambas <- "No"
  } else cumplimiento_normalidad_ambas <- "No"
  
  # Rejunte y decicion de todo
  rejunte <- c(cumplimiento_normalidad_01, cumplimiento_normalidad_02, cumplimiento_homogeneidad)
  rejunte[rejunte == "Si"] <- 1
  rejunte[rejunte == "No"] <- 0
  rejunte <- as.numeric(rejunte)
  
  # Cumplimiento General
  if(sum(rejunte) == length(rejunte)) cumplimiento_general <- "Si" else cumplimiento_general <- "No"
  

  # Test t (Dos muestras independientes)
  the_test <- t.test(VR ~ FACTOR,
                     alternative = input_tipo_prueba, 
                     mu = input_media_ho,
                     paired = FALSE,
                     conf.level = confianza,
                     var.equal = TRUE)
  
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estadistico observado
  estadistico_obs_interno <- the_test$statistic
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estimadores
  media_esp_interna <- input_media_ho
  media_esp_externa <- media_esp_interna # Aproposito sin redondear
  media_obs_interna <- medias_obs_internas[1] - medias_obs_internas[2] 
  media_obs_externa <- round2(media_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- the_test$parameter
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- the_test$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                        por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                    La diferencia entre las medias muestrales de ambos grupos es estadísticamente 
                    igual a cero.<br/>
                    Las medias muestrales de ambos grupos son estadísticamente iguales.<br/>
                    La media muestral de la categoría '_mi_categoria1_' es estadísticamente igual a 
                    la media muestral de la categoría '_mi_categoria2_'."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                    La diferencia entre las medias muestrales de ambos grupos es estadísticamente 
                    igual a cero.<br/>
                    Las medias muestrales de ambos grupos son estadísticamente iguales.<br/>
                    La media muestral de la categoría '_mi_categoria1_' es estadísticamente igual a 
                    la media muestral de la categoría '_mi_categoria2_'."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                    por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                    La diferencia entre las medias muestrales de ambos grupos es estadísticamente 
                    distinta de cero.<br/>
                    Las medias muestrales de ambos grupos son estadísticamente diferentes.<br/>
                    La media muestral de la categoría '_mi_categoria1_' (_mi_mediaMenor_) es estadísticamente distinta a 
                    la media muestral de la categoría '_mi_categoria2_' (_mi_mediaMayor_).<br/>
                    La media muestral de la categoria '_mi_categoriaMayor_' es estadísticamente mayor a 
                    la media de la categoría '_mi_categoriaMenor_'.<br/>
                    La media muestral de la categoria '_mi_categoriaMenor_' es estadísticamente menor a 
                    la media de la categoría '_mi_categoriaMayor_'."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    frase_estadistica <- gsub("_mi_mediaMenor_", medias_ordenadas_externas[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_mediaMayor_", medias_ordenadas_externas[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoriaMenor_", names(medias_ordenadas_externas)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoriaMayor_", names(medias_ordenadas_externas)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria1_", categorias[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria2_", categorias[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)


  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral1 <-  "<b>Hipótesis Nula (Ho):</b> la diferencia de las medias de las categorías '_mi_categoria1_' y '_mi_categoria2_' es igual a cero.<br/>
                               <b>Hipótesis Alternativa (Hi):</b>  la diferencia de las medias de las categorías '_mi_categoria1_' y '_mi_categoria2_' es distinta de cero."
    
    # Bilateral
    frase_juego_bilateral2 <-  "<b>Hipótesis Nula (Ho):</b> las medias de las categorías '_mi_categoria1_' y '_mi_categoria2_' son iguales.<br/>
                               <b>Hipótesis Alternativa (Hi):</b>  las medias de las categorías '_mi_categoria1_' y '_mi_categoria2_' son diferentes."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis1 <- frase_juego_bilateral1 else
      if(input_tipo_prueba == "less") frase_juego_hipotesis1 <- frase_juego_izquierda1 else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis1 <- frase_juego_derecha1
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis2 <- frase_juego_bilateral2 else
      if(input_tipo_prueba == "less") frase_juego_hipotesis2 <- frase_juego_izquierda2 else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis2 <- frase_juego_derecha2
    
    
    frase_juego_hipotesis1 <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis1)
    
    frase_juego_hipotesis2 <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis2)
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable Numérica", # 1
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"), # 2
                  "n", # 3
                  "Test de Normalidad utilizado",  # 4
                  "¿Presenta distribución normal la variable en la categoría?", # 5
                  "Test de Homogeneidad de Varianzas utilizado", # 6
                  "¿Son homogéneas las varianzas?", # 7
                  "¿Se cumplen todos los requisitos del 'Test t (dos muestras independientes)'?", #8
                  "¿Es válido sacar conclusiones del 'Test t (dos muestras independientes)'?" #9
                  )
    tabla_requisitos <- matrix("--------", 2, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    # Fila 1 para la categoria 1
    tabla_requisitos[1,1] <- colnames(input_base)[2]
    tabla_requisitos[1,2] <- categorias[1]
    tabla_requisitos[1,3] <- n_grupo1
    tabla_requisitos[1,4] <- "Shapiro-Wilk"
    tabla_requisitos[1,5] <- cumplimiento_normalidad_01
    tabla_requisitos[1,6] <- "Fisher"
    tabla_requisitos[1,7] <- cumplimiento_homogeneidad
    tabla_requisitos[1,8] <- cumplimiento_general
    tabla_requisitos[1,9] <- cumplimiento_general
    
    # Fila 2 para la categoria 2
    tabla_requisitos[2,1] <- colnames(input_base)[2]
    tabla_requisitos[2,2] <- categorias[2]
    tabla_requisitos[2,3] <- n_grupo2
    tabla_requisitos[2,4] <- "Shapiro-Wilk"
    tabla_requisitos[2,5] <- cumplimiento_normalidad_02
    # # tabla_requisitos[2,6] <- "Fisher"
    # # tabla_requisitos[2,7] <- cumplimiento_homogeneidad
    # # tabla_requisitos[2,8] <- cumplimiento_general
    # # tabla_requisitos[2,9] <- cumplimiento_general
  }
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "Paralelamente a la generación del test t para dos muestras independientes, RMedic 
                                  realiza a su vez la comprobación estadística de la normalidad para la variable numérica 
                                  en ambas categorías mediante el test de normalidad de Shapiro-Wilk; y realiza también un test de homogeneidad 
                                  de varianzas de Fisher. Para visualizar los resultados estadísticos de normalidad y homogeneidad seleccione cada 
                                  test en su pestaña."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra no se cumplen los requisitos de normalidad de ambas categorías y homogeneidad de varianzas 
                            que permiten la utilización del test t para dos muestras independientes, por lo tanto <b><u>no es válido sacar conclusiones del test t para dos muestras independientes </b></u> 
                          indistintamente de los valores obtenidos.<br/>
                          Para poder sacar conclusiones válidas sobre dos grupos con respesto a una medida de posición, debiera 
                          dirijirse al test de Wilcoxon (dos muestras independientes) donde se pone a prueba el valor de la diferencia de las medianas."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra se cumplen todos los requisitos del test t para 
                            dos muestras independientes, por lo tanto <b><u>es válido sacar conclusiones del test t 
                            para dos muestras apareadas</b></u>."
    
    
    if(cumplimiento_general == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_general == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)

    frase_requisitos <- gsub("_mi_categoria1_", categorias[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_categoria2_", categorias[2], frase_requisitos)
    frase_requisitos <- gsub("_mi_variable1_", colnames(input_base)[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_variable2_", colnames(input_base)[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable Numérica",  # 1
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"), #2
                  "n",  #  3
                  "Media muestral (valor observado)", # 4
                  "Test",  # 5
                  "Diferencia de medias (valor observado)", #6
                  "Diferencia de medias (valor poblacional esperado bajo hipótesis)",  # 7
                  "Tipo de prueba", # 8
                  "Estadístico (t)",  # 9
                  "Grados de Libertad", # 10
                  "Valor p", # 11
                  "Alfa", # 12
                  "Decisión", #13
                  "¿Son las dos medias diferentes entre si?" # 14
    )
    
    tabla_resumen <- matrix("--------", 2, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[2]
    tabla_resumen[2, 1] <- colnames(input_base)[2]
    tabla_resumen[1, 2] <- categorias[1]
    tabla_resumen[2, 2] <- categorias[2]
    
    tabla_resumen[1, 3] <- n_grupo1
    tabla_resumen[2, 3] <- n_grupo2
    
    tabla_resumen[1, 4] <- medias_obs_externas[1]
    tabla_resumen[2, 4] <- medias_obs_externas[2]
    
    tabla_resumen[1, 5] <- "Test t (Dos muestras independientes)"
    tabla_resumen[1, 6] <- media_obs_externa
    tabla_resumen[1, 7] <- media_esp_externa
    tabla_resumen[1, 8] <- tipo_de_prueba
    
    tabla_resumen[1, 9] <- estadistico_obs_externo
    tabla_resumen[1, 10] <-  gl_interno 
    tabla_resumen[1,11] <- valor_p_externo
    tabla_resumen[1,12] <- input_alfa
    tabla_resumen[1,13] <- decision
    tabla_resumen[1,14] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis1 <- frase_juego_hipotesis1
  
  SALIDA_ARMADA$frase_juego_hipotesis2 <- frase_juego_hipotesis2
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 




Test_QC_TestWilcoxon_DosMuestras_Independientes <- function(input_base = NULL, 
                                                     input_tipo_prueba = "two.sided", 
                                                     input_mediana_ho = 0, 
                                                     input_decimales = 2, 
                                                     input_alfa = 0.05){
  
  
  # Correccion necesaria para input_tipo_prueba
  if(is.null(input_tipo_prueba)) input_tipo_prueba <- "two.sided"
  if (input_tipo_prueba != "two.sided") input_tipo_prueba <- "two.sided"
  
  # Correccion necesaria para input_mediana_ho
  if(is.null(input_mediana_ho)) input_mediana_ho <- 0
  if (input_mediana_ho != 0) input_mediana_ho <- 0
  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para media de las
  # diferencias = 0.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------

  # Librerias  
  library("coin")
  
  # Aplicamos na.omit() y otros cambios en input_base
  input_base <- na.omit(input_base)
  input_base[,1] <- as.factor(input_base[,1])
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  
  # n de cada grupo
  tabla_fa <- table(input_base[,1])
  categorias <- names(tabla_fa)
  n_grupo1 <- tabla_fa[1]
  n_grupo2 <- tabla_fa[2]
  
  # Objetos intermedios
  FACTOR <- input_base[,1]
  VR <- input_base[,2]
  
  
  # Medianas
  medianas_obs_internas <- tapply(VR, FACTOR, median)
  medianas_obs_externas <- round2(medianas_obs_internas, input_decimales)
  
  # Medianas ordenadas de menor a mayor
  medianas_ordenadas_externas <- sort(medianas_obs_externas, decreasing = FALSE)
  

  

  
  # Test t (Dos muestras independientes)
  the_test <- wilcox_test(VR ~ FACTOR,
                     alternative = input_tipo_prueba, 
                     mu = input_mediana_ho,
                     distribution = "exact",
                     conf.level = confianza,
                     conf.int = TRUE)
  
# # # Salidas especiales de la funcion wilcox_test()
# expectation(the_test)[1]
# covariance(the_test)
# statistic(the_test)
# pvalue(the_test)
# variance(the_test)
  
  # Tipo de prueba
  if(input_tipo_prueba == "two.sided") tipo_de_prueba <- "Bilateral" else
    if(input_tipo_prueba == "less") tipo_de_prueba <- "Unilateral Izquierda" else
      if(input_tipo_prueba == "greater") tipo_de_prueba <- "Unilateral Derecha"
  
  
  # Estadistico observado
  estadistico_obs_interno <- statistic(the_test)
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  # Estimadores
  mediana_esp_interna <- input_mediana_ho
  mediana_esp_externa <- mediana_esp_interna # Aproposito sin redondear
  mediana_obs_interna <- medianas_obs_internas[1] - medianas_obs_internas[2] 
  mediana_obs_externa <- round2(mediana_obs_interna, input_decimales)
  
  
  # Grados de Libertad
  gl_interno <- "No corresponde"
  gl_externo <- gl_interno
  
  # Valor p 
  valor_p_interno <- pvalue(the_test)
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
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
                    por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                    La diferencia entre las medianas muestrales de ambos grupos es estadísticamente 
                    igual a cero.<br/>
                    Las medianas muestrales de ambos grupos son estadítsicamente iguales.<br/>
                    La mediana muestral de la categoría '_mi_categoria1_' es estadísticamente igual a 
                    la mediana muestral de la categoría '_mi_categoria2_'."
      
      
      frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                    La diferencia entre las medias muestrales de ambos grupos es estadísticamente 
                    igual a cero.<br/>
                    Las medianas muestrales de ambos grupos son estadítsicamente iguales.<br/>
                    La mediana muestral de la categoría '_mi_categoria1_' es estadísticamente igual a 
                    la mediana muestral de la categoría '_mi_categoria2_'."
      
      
      frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba bilateral.<br/>
                    La diferencia entre las medianas muestrales de ambos grupos es estadísticamente 
                    distinta de cero.<br/>
                    Las medianas muestrales de ambos grupos es estadísticamente iguales.<br/>
                    La mediana muestral de la categoría '_mi_categoria1_' (_mi_medianaMenor_) es estadísticamente distinta a 
                    la mediana muestral de la categoría '_mi_categoria2_' (_mi_medianaMayor_).<br/>
                    La mediana muestral de la categoria '_mi_categoriaMayor_' (_mi_medianaMayor_) 
                    es estadísticamente mayor a la mediana de la categoría '_mi_categoriaMenor_' (_mi_medianaMenor_).<br/>
                    La mediana muestral de la categoria '_mi_categoriaMenor_' (_mi_medianaMenor_) es 
                    estadísticamente menor a la mediana de la categoría '_mi_categoriaMayor_' (_mi_medianaMayor_)."
    }
    
    # Unilateral Izquierda
    {
      frase2_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mediana_esp_)."
      
      
      frase2_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La mediana muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de mediana poblacional esperado bajo hipótesis (_mi_mediana_esp_)."
      
      
      frase2_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral izquierda.<br/>
                       La media muestral observada (_mi_mediana_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
    }
    
    # Unilateral Derecha
    {
      frase3_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                          por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>no se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                        La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente igual al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
      
      
      frase3_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                        por lo tanto <b><u>se rechaza la Ho</b></u> de la prueba unilateral derecha.<br/>
                       La media muestral observada (_mi_media_obs_) de la variable '_mi_variable_' es 
                       estadísticamente diferente al valor de media poblacional esperado bajo hipótesis (_mi_mu_esp_)."
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
    
    frase_estadistica <- gsub("_mi_medianaMenor_", medianas_ordenadas_externas[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_medianaMayor_", medianas_ordenadas_externas[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoriaMenor_", names(medianas_ordenadas_externas)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoriaMayor_", names(medianas_ordenadas_externas)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria1_", categorias[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria2_", categorias[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    
    # Bilateral
    frase_juego_bilateral1 <-  "<b>Hipótesis Nula (Ho):</b> la diferencia de las medianas de las categorías '_mi_categoria1_' y '_mi_categoria2_' es igual a cero.<br/>
                               <b>Hipótesis Alternativa (Hi):</b>  la diferencia de las medianas de las categorías '_mi_categoria1_' y '_mi_categoria2_' es distinta de cero."
    
    # Bilateral
    frase_juego_bilateral2 <-  "<b>Hipótesis Nula (Ho):</b> las medianas de las categorías '_mi_categoria1_' y '_mi_categoria2_' son iguales.<br/>
                               <b>Hipótesis Alternativa (Hi):</b>  las medianas de las categorías '_mi_categoria1_' y '_mi_categoria2_' son diferentes."
    
    # Unilateral Izquierda
    frase_juego_izquierda <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es mayor o igual a _mi_mu_esp_.<br/>
                               <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es menor a _mi_mu_esp_."
    
    # Unilateral Derecha
    frase_juego_derecha <-  "<b>Hipótesis Nula (Ho):</b> la media poblacional de la variable '_mi_variable_' es menor o igual a _mi_mu_esp_.<br/>
                            <b>Hipótesis Alternativa (Hi):</b> la media poblacional de la variable '_mi_variable_' es mayor a _mi_mu_esp_."
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis1 <- frase_juego_bilateral1 else
      if(input_tipo_prueba == "less") frase_juego_hipotesis1 <- frase_juego_izquierda1 else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis1 <- frase_juego_derecha1
    
    
    
    if(input_tipo_prueba == "two.sided") frase_juego_hipotesis2 <- frase_juego_bilateral2 else
      if(input_tipo_prueba == "less") frase_juego_hipotesis2 <- frase_juego_izquierda2 else
        if(input_tipo_prueba == "greater") frase_juego_hipotesis2 <- frase_juego_derecha2
    
    
    frase_juego_hipotesis1 <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis1)
    frase_juego_hipotesis1 <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis1)
    
    frase_juego_hipotesis2 <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis2)
    frase_juego_hipotesis2 <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis2)
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
 
  
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable Numérica",  # 1
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"), #2
                  "n",  #  3
                  "Mediana muestral (valor observado)", # 4
                  "Test",  # 5
                  "Diferencia de medianas (valor observado)", #6
                  "Diferencia de medianas (valor poblacional esperado bajo hipótesis)",  # 7
                  "Tipo de prueba", # 8
                  "Estadístico (Z)",  # 9
                  "Grados de Libertad", # 10
                  "Valor p", # 11
                  "Alfa", # 12
                  "Decisión", #13
                  "¿Son las dos medianas diferentes entre si?" # 14
    )
    
    tabla_resumen <- matrix("--------", 2, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[1, 1] <- colnames(input_base)[2]
    tabla_resumen[2, 1] <- colnames(input_base)[2]
    tabla_resumen[1, 2] <- categorias[1]
    tabla_resumen[2, 2] <- categorias[2]
    
    tabla_resumen[1, 3] <- n_grupo1
    tabla_resumen[2, 3] <- n_grupo2
    
    
    tabla_resumen[1, 4] <- medianas_obs_externas[1]
    tabla_resumen[2, 4] <- medianas_obs_externas[2]
    
    tabla_resumen[1, 5] <- "Test Wilcoxon (Dos muestras independientes)"
    tabla_resumen[1, 6] <- mediana_obs_externa
    tabla_resumen[1, 7] <- mediana_esp_externa
    tabla_resumen[1, 8] <- tipo_de_prueba
    
    tabla_resumen[1, 9] <- estadistico_obs_externo
    tabla_resumen[1, 10] <-  gl_interno 
    tabla_resumen[1,11] <- valor_p_externo
    tabla_resumen[1,12] <- input_alfa
    tabla_resumen[1,13] <- decision
    tabla_resumen[1,14] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis1 <- frase_juego_hipotesis1
  
  SALIDA_ARMADA$frase_juego_hipotesis2 <- frase_juego_hipotesis2
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function 



Test_QC_TestHomogeneidadDeVarianzas_Bartlett <- function(input_base = NULL, 
                                                       input_decimales = 2, 
                                                       input_alfa = 0.05){
  
  

  

  
  ##########################################-----------------------------------------------------------------
  # La prueba por el momento sera solo para bilatal para ratio = 1.
  # Por eso la "correccion necesaria" que puse al inicio.
  ##########################################-----------------------------------------------------------------
  
  
  # Aplicamos na.omit()
  input_base <- na.omit(input_base)
  VR <- input_base[,2]
  FACTOR <- as.factor(input_base[,1])
  
  # Varianzas
  varianzas_obs_internas <- tapply(VR, FACTOR, var)
  varianzas_obs_externas <- round2(varianzas_obs_internas, input_decimales)
  
  # Algunos objetos necesarios
  confianza <- 1 - input_alfa
  tabla_fa <- table(input_base[,1])
  categorias <- names(tabla_fa)
  cantidad_categorias <- length(categorias)
  orden_categorias <- c(1:cantidad_categorias)
  
  # Test de Normalidad Particionado
  test_normalidad_particionado <- Test_QC_TestNormalidad_ShapiroWilk_Particionado(input_base = input_base,
                                                                                  input_decimales = input_decimales,
                                                                                  input_alfa = input_alfa)
  
  
  
  
  # Cumplimiento de normalidad individual
  cumplimiento_normalidad_individual <- test_normalidad_particionado$tabla_resumen[orden_categorias,9]

  # Cumplimiento de normalidad general
  if(sum(cumplimiento_normalidad_individual == "Si") == cantidad_categorias) {
    cumplimiento_normalidad_general <- "Si" 
     } else cumplimiento_normalidad_general <- "No"

    
  # Cumplimiento General de todo
  cumplimiento_general <- cumplimiento_normalidad_general
  
  
  # Test de Bartlett
  the_test <- bartlett.test(VR ~ FACTOR)
  
  
  
  
  # Estimadores
  estimadores_obs_internos <- varianzas_obs_internas
  estimadores_obs_externos <- varianzas_obs_externas

  
  # Estadistico observado
  estadistico_obs_interno <- the_test$statistic 
  estadistico_obs_externo <- round2(estadistico_obs_interno, input_decimales)
  
  
  
  
  # Grados de Libertad
  gl_interno <- the_test$parameter
  gl_externo <- gl_interno # A proposito va asi, sin redondear

  # Valor p 
  valor_p_interno <- the_test$p.value
  valor_p_externo <- round2(valor_p_interno, input_decimales)
  if (valor_p_interno < 0.01) valor_p_externo <- "<<0.01"
  
  
  
  # Frase
  if (valor_p_interno < input_alfa) decision <- "Rechazo Ho" else if (valor_p_interno >= input_alfa) decision <- "No rechazo Ho"
  
  # Respuesta
  if (valor_p_interno < input_alfa) respuesta <- "Si" else if (valor_p_interno >= input_alfa) respuesta <- "No"
  
  
  # Frases segun valor p
  {
    
 
    # Algun inconveniente
    frase0_v1 <- "No pudo obtenerse un valor p."
    
    
    frase1_v1 <-  "El valor p=_mi_valor_p_ es mayor que el valor de alfa=_mi_valor_alfa_ 
                   por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                   Las varianzas son estadísticamente homogéneas (varianzas homocedásticidad).<br/>
                   Las varianzas son estadísticamente iguales entre todas las categorías de la 
                   variable '_mi_variable2_."
    
    
    frase1_v2 <- "El valor p=_mi_valor_p_ es igual que el valor de alfa=_mi_valor_alfa_ 
                  por lo tanto <b><u>no se rechaza la Ho</b></u>.<br/>
                  Las varianzas son estadísticamente homogéneas (varianzas homocedásticidad).<br/>
                  Las varianzas son estadísticamente iguales entre todas las categorías de la 
                  variable '_mi_variable2_."
    
    
    frase1_v3 <- "El valor p=_mi_valor_p_ es menor que el valor de alfa=_mi_valor_alfa_ 
                  por lo tanto <b><u>se rechaza la Ho</b></u>.<br/>
                  Al menos una de las varianzas de una categoría es estadísticamente diferente del resto.<br/>
                  Al rechazarse la hipótesis nula del test de Bartlett solo se garantiza que son 
                  estadísticamente diferentes la varianza más grande y la más pequeña de todas las varianzas 
                  de las categorías."
    
    
    # Seleccion de Frase Estadistica
    if(is.na(valor_p_interno) | is.null(valor_p_interno)) frase_estadistica <- frase0_v1 else

        if (valor_p_interno > input_alfa) frase_estadistica <- frase1_v1 else
          if (valor_p_interno == input_alfa) frase_estadistica <- frase1_v2 else
            if (valor_p_interno < input_alfa) frase_estadistica <- frase1_v3
     
    
    frase_estadistica <- gsub("_mi_variable1_", colnames(input_base)[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_variable2_", colnames(input_base)[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria1_", categorias[1], frase_estadistica)
    frase_estadistica <- gsub("_mi_categoria2_", categorias[2], frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_p_", valor_p_externo, frase_estadistica)
    frase_estadistica <- gsub("_mi_valor_alfa_", input_alfa, frase_estadistica)
    
    
  } # Fin Frases segun valor p
  
  
  # Frases Juego de Hipotesis
  {
    

    frase_juego_hipotesis <-  "<b>Hipótesis Nula (Ho):</b> las varianzas de todas las categorías son 
                              estadístiamente iguales (homogeneidad de varianzas).<br/>
                               <b>Hipótesis Alternativa (Hi):</b> al menos una de las categorías presenta 
                               una varianza estadísticamente difernete del resto."
    
    
    
    
    
    frase_juego_hipotesis <- gsub("_mi_variable1_", colnames(input_base)[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_variable2_", colnames(input_base)[2], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_categoria1_", categorias[1], frase_juego_hipotesis)
    frase_juego_hipotesis <- gsub("_mi_categoria2_", categorias[2], frase_juego_hipotesis)
    
    
    
    
  } # Fin Frases Juego de Hipotesis
  
  
  # Frase por incontenientes de redondeo
  dt1 <- valor_p_interno < input_alfa
  dt2 <- round2(valor_p_interno, input_decimales) < input_alfa
  if (sum(dt1, dt2) == 2) frase_redondeo <- "" else
    if (sum(dt1, dt2) == 0) frase_redondeo <- "" else
      if (sum(dt1, dt2) == 1){
        frase_redondeo <- "<b><u>Advertencia:</u> En este set de datos 
            le recomendamos que aumente la cantidad de decimales ya que en este 
            caso el redondeo excesivo distorciona la interpretación correcta del test. 
            Aumente la cantidad de decimales hasta que esta advertencia 
            desaparezca.</b>"
        
      } 
  # #######################################
  # 
  
  # Tabla Requisitos
  {
    nombres1 <- c("Variable Numérica",
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"),
                  "n", 
                  "Test de Normalidad", 
                  "¿Es normal la variable?", 
                  "¿Se cumplen los requisitos del 'Test de Homogeneidad de Varianzas de Bartlett'?",
                  "¿Es válido sacar conclusiones del 'Test de Homogeneidad de Varianzas de Bartlett'?")
    tabla_requisitos <- matrix("--------", cantidad_categorias, length(nombres1))
    colnames(tabla_requisitos) <- nombres1
    
    
    # Fila 1 para la categoria 1
    tabla_requisitos[,1] <- rep(colnames(input_base)[2], cantidad_categorias)
    tabla_requisitos[,2] <- categorias
    tabla_requisitos[,3] <- tabla_fa
    tabla_requisitos[,4] <- rep("Shapiro-Wilk", cantidad_categorias)
    tabla_requisitos[,5] <- cumplimiento_normalidad_individual
    tabla_requisitos[1,6] <- cumplimiento_normalidad_general
    tabla_requisitos[1,7] <- cumplimiento_normalidad_general
    
}
  
  
  # Frase Requisitos
  {
    frase_inicial_requisitos <- "El test de Homogeneidad de Varianzas de Barlett es aplicable a dos o más 
                                categorías si todas las categorías presentan distribución normal.<br/>
                      Paralelamente a la generación del test de Homogeneidad de Varianzas de Barlett, RMedic 
                      realiza a su vez la comprobación estadística de la normalidad de todas las 
                      categorías con el test de normalidad de Shapiro-Wilk (Particionado)."
    
    frase_no_requisitos <- "Para el pool de datos de la muestra no se cumple que simultáneamente todas 
                            las categorías presenten distribución normal, por lo tanto <b><u>no es válido 
                            sacar conclusiones del test de Homogeneidad de Varianzas de Bartlett</b></u> 
                            indistintamente de los valores obtenidos."
    
    frase_si_requisitos <-  "Para el pool de datos de la muestra se cumple que simultáneamente todas 
                            las categorías presentan distribución normal, por lo tanto <b><u>es válido 
                            sacar conclusiones del test de Homogeneidad de Varianzas de Bartlett</b></u>."
    
    
    if(cumplimiento_general == "No") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_no_requisitos) else
      if(cumplimiento_general == "Si") frase_requisitos <- paste0(frase_inicial_requisitos, "<br/>", frase_si_requisitos)
    
    frase_requisitos <- gsub("_mi_categoria1_", categorias[1], frase_requisitos)
    frase_requisitos <- gsub("_mi_categoria2_", categorias[2], frase_requisitos)
  }
  
  # Tabla resumen  
  {
    
    nombres2 <- c("Variable Numérica",  # 1
                  paste0("Categorías de la variable '", colnames(input_base)[1], "'"), #2
                  "n",  #  3
                  "Varianza muestral (valor observado)", # 4
                  "Test",  # 5
                  "Estadístico (K^2)",  # 6
                  "Grados de Libertad", # 7
                  "Valor p", # 8
                  "Alfa", # 9
                  "Decisión", #10
                  "¿Al menos una de las varianzas es diferente?" # 11
    )
    
    tabla_resumen <- matrix("--------", cantidad_categorias, length(nombres2))
    colnames(tabla_resumen) <- nombres2
    
    tabla_resumen[, 1] <- rep(colnames(input_base)[2], cantidad_categorias)
    tabla_resumen[, 2] <- categorias
    tabla_resumen[, 3] <- tabla_fa
    tabla_resumen[, 4] <- varianzas_obs_externas
    
    tabla_resumen[1, 5] <- "Test de homogeneidad de varianzas de Bartlett"
    tabla_resumen[1, 6] <- estadistico_obs_externo

    
    tabla_resumen[1, 7] <- gl_externo
    tabla_resumen[1, 8] <- valor_p_externo
    tabla_resumen[1, 9] <- input_alfa
    tabla_resumen[1,10] <- decision
    tabla_resumen[1,11] <- respuesta
    # 
    
  }
  
  SALIDA_ARMADA <- list()
  
  SALIDA_ARMADA$tabla_requisitos <- tabla_requisitos
  
  SALIDA_ARMADA$frase_requisitos <- frase_requisitos
  
  SALIDA_ARMADA$tabla_resumen <- tabla_resumen
  
  SALIDA_ARMADA$frase_estadistica <- frase_estadistica
  
  SALIDA_ARMADA$frase_redondeo <- frase_redondeo
  
  SALIDA_ARMADA$frase_juego_hipotesis <- frase_juego_hipotesis
  
  
  # Returno Exitoso
  return(SALIDA_ARMADA)
  
  
  
} # Fin function







