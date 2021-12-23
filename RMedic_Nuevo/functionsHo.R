
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
      RESUMEN <- matrix(NA, 1, length(nombres))
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
  
  tabla_resumen <- matrix(NA, 1, length(nombres))
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
  tabla_requisitos <- matrix(NA, 1, length(nombres1))
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
  tabla_resumen <- matrix(NA, 1, length(nombres2))
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
    tabla_resumen <- matrix(NA, 1, length(nombres2))
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
                                     input_varianza_ho = 0, 
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
  estadistico_obs_interno <- varianza_obs_interna
  estadistico_obs_externo <- round2(varianza_obs_interna, input_decimales)
  
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
    tabla_requisitos <- matrix(NA, 1, length(nombres1))
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
    tabla_resumen <- matrix(NA, 1, length(nombres2))
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



