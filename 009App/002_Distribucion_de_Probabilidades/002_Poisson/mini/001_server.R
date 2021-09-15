# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot


# Hacemos reactivo la aplicación a los cambios que realiza el usuario
data <- reactive({
  
  # # # Valores introducidos en la pagina por el usuario
  intro <- input$x
  intro <- as.character(intro)
  
  # # # Valores esperados y observados
  # Esperamos que ingrese números... y algún separador, si
  # es que el usuario quiere que se sumen por ejemplo la probabilidad de 2,4,5.
  esp <- c(0:9,",",":") 
  obs <- strsplit(intro,"")[[1]] 
  
  # # # Separadores correctos posibles
  sep1 <- ","
  sep2 <- ":"
  
  
  # # # Deteccion... Comparando esperado y obs
  # La deteccion lo que hara es ver si todos los elementos ingresados por el 
  # usuario son los permitidos.
  
  dt <- rep(FALSE, length(obs))
  
  # # # Comparacion entre obs y esp
  # A cada elemento "observado" lo compara con cada elemento "esperado".
  for(adi in 1:length(obs)) {
    for(das in 1:length(esp)) {
      
      if (obs[adi] == esp[das])   dt[adi] <- TRUE
      
    } # Fin for das
  } # Fin for adi
  #########################################################
  
  
  # # # Determinacion de accion: HAGO.
  # HAGO por defecto es FALSE... 
  # Y cambia a TRUE si se tienen todos los requerimientos.
  HAGO <- FALSE
  if (sum(as.numeric(dt)) == length(dt)) HAGO <- TRUE
  
  # # # Deteccion de separadores
  
  # Separamos lo introducido por el usuario por el separador 1.
  # Y contamos en cuantas partes se cortó.
  # Como es el 1er corte que se hace... solo habrá una lista... 
  # por eso se agrego el dep1 <- dep1[[1]]
  dep1 <- strsplit(intro,",")
  dep1 <- dep1[[1]]
  cuantos_dep1 <- rep(NA, length(dep1))
  for (nike in 1:length(dep1)) cuantos_dep1[nike] <- length(dep1[[nike]])
  
  # Ahora, cada elemento anterior... vemos si se puede partir
  # con el el 2do separador
  # Ahora... podrá haber varios pedazos...
  dep2 <- strsplit(dep1,":")
  cuantos_dep2 <- rep(NA, length(dep2))
  for (nike in 1:length(dep2)) cuantos_dep2[nike] <- length(dep2[[nike]])
  
  # Vemos como se separa en cada caso...
  # Donde son iguales, es donde hay una coma... (TRUE)
  # Donde son diferentes, hay dos puntos... (FALSE)
  pureza <- cuantos_dep1 == cuantos_dep2
  
  # Objeto que contendra la secuencia de numeros correspondiente
  numeros <- NA
  
  # Vamos buscando cada valor cuando hay coma...
  # Cuando hay punto y coma, generamos una secuencia...
  for (ron in 1:length(pureza)) {
    
    # Si hay una "coma"
    if (pureza[ron] == TRUE) numeros <- c(numeros, dep2[[ron]])
    
    # Si hay "dos puntos"
    else numeros <- c(numeros, c(dep2[[ron]][1]:dep2[[ron]][2]))
    
  } # Fin for pureza
  ########################################################################
  
  
  # Borramos el NA inicial, que era para generar el objeto...
  numeros <- na.omit(numeros)
  
  # Volvemos numerico al vector
  numeros <- as.numeric(numeros)
  
  # Damos salida a los numeros seleccionados
  numeros
  
}) # Fin data
#########################################################################



# Grafico 1: Probabilidades de Poisson
output$distPlot1 <- renderPlot({
  
  # Llamamos a los numeros que ingreso el usuario...
  numeros <- data()
  
  # Si los numeros de x mayores a 0... 
  # Y el labda es mayor a 0
  # Generara el grafico
  if (min(numeros) >= 0 & input$lambda >= 0) {
    
    # Datos Poisson 
    ###########
    {
      
      ###  
      # Valores de la variable que el usuaro pidio
      # Como la distribucion Poissos es para cualquier x...
      # Lo que hace es calcular hasta 10 valores siguientes del x mas
      # grande que haya pedido... asi el grafico queda mejor
      valores_x <- c(0:(max(numeros)+10))
      
      # Probabilidades de Poisson
      prob <- dpois(valores_x,  input$lambda)
      
      # Probabilidades acumuladas de Poisson
      prob_acum <- cumsum(prob)
      
      
      ###  
    } # Fin Datos Poisson
    ###########################
    
    
    # # #Parametros Graficos
    
    # Guardamos las opciones graficas por defecto     
    opar <- par()
    
    # Cambiamos algunos terminos
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    # Creamos el grafico propiamente dicho
    plot(valores_x, prob, ylim=c(0,1), main="Distribución Poisson", axes=FALSE, xlab=c("Casos Favorables (x)"),  ylab=c("Probabilidad"), col=input$variable_color, cex.main=2, cex.lab=2)
    
    # Le agregamos unos ejes...
    axis(2,  las=1, cex.axis=2)
    axis(1, valores_x ,las=1, cex=1, cex.axis=2)
    
    # Le agregamos los bastoncitos de probabilidad
    segments(x0= valores_x, y0= 0, x1= valores_x, y1= prob, col= input$variable_color, lwd=5)
    
    # Restauramos los parametros graficos
    par <- opar
  } # Fin if  x <= n
  #######################################################################################################################
  
}) # Fin Plot 1
##################################################################################################################################



# Gráfico 2: Probabilidades Acumuladas de Poisson
output$distPlot2 <- renderPlot({
  
  # Llamamos a los numeros que ingreso el usuario...
  numeros <- data()
  
  # Si los numeros de x mayores a 0... 
  # Y el labda es mayor a 0
  # Generara el grafico
  if (min(numeros) >= 0 & input$lambda >= 0) {   
    ##################################################################
    
    # Datos Poisson
    {
      ###  
      
      # Valores de la variable x...
      # se le agrega un +10... para que calcule hasta 10 valores
      # siguientes... asi el grafico queda mejor
      valores_x <- c(0:(max(numeros)+10))
      
      # Probabilidades de Poisson
      prob <- dpois(valores_x, input$lambda)
      
      # Probabilidades acumuladas
      prob_acum <- cumsum(prob)
      
      ###  
    } # Fin Datos Poisson
    ###########################
    
    
    # Guardamos los parametros graficos por defecto      
    opar <- par()
    
    # Hacemos algunos cambios a los parametros graficos
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    # Creamos el grafico de probabilidades acumuladas
    plot(valores_x, prob_acum, ylim=c(0,1), main="Distribución Poisson Acumulada", axes=FALSE, xlab=c("Casos Favorables (x)"), ylab=c("Prob. Acum"), col=input$variable_color, cex.main=2, cex.lab=2)
    
    # Agragamos unos ejes
    axis(2,  las=1, cex.axis=2)
    axis(1, valores_x ,las=1, cex=1, cex.axis=2)
    
    # Insertamos los bastones de probabilidad
    segments(x0= valores_x, y0= 0, x1= valores_x, y1= prob_acum, col= input$variable_color, lwd=5)
    
    # Reestablecemos los parametros graficos iniciales
    par <- opar
  } # Fin if  x <= n
  #######################################################################################
}) # Fin Plot 2
################################################################################################################




# Tabla de Parámetros y Afines
sliderValues1 <- reactive({
  
  numeros <- data()
  # La forma de cargar los datos... podría ser está... 
  # Hay que ver si sirve así o no...
  lala <- paste("p(x=", numeros, ")", sep="")
  
  # Por ahora... lo vamos a dejar, de la forma "clasica..."
  
  # Creamos un data.frame
  data.frame(
    Lambda = c(input$lambda), # Valor de Lambda
    x=c(as.character(numeros)), # Los numeros ingresados por el usuario 
    prob = round(c(dpois(numeros,  input$lambda)), input$decimales), # Los valores de prob solo de los que pidio el usuario
    prob.cum= round(cumsum(c(dpois(numeros,  input$lambda))), input$decimales), # Prob. acum. de los valores que pidio el usuario
    stringsAsFactors=FALSE)
  #######################################################
}) # FIn sliderValues1

output$values1 <- renderTable({
  sliderValues1()
})
# Fin Tabla
##################################################  



# Tabla de Probabilidades
sliderValues2 <- reactive({
  
  numeros <- data()
  
  if ( min(numeros) >= 0 & input$lambda >= 0) {
    
    # Datos Poisson 
    ###########
    {
      ###  
      valores_x <- c(0:(max(numeros)+10))
      prob <- dpois(valores_x, input$lambda)
      prob_acum <- cumsum(prob)
      
      
      ###  
    } # Fin Datos Poisson
    ###########################
    
    # Compose data frame
    data.frame(
      x = valores_x,
      Prob = round(prob, input$decimales),
      ProbAcum = round(prob_acum, input$decimales),
      stringsAsFactors=FALSE)
    
  } # Fin if
  #######################################################
})  # FIn sliderValues1


output$values2 <- renderTable({
  sliderValues2()
})
# Fin Tabla
##################################################  




# Texto 1  - Valor Alfa
output$text1 <- renderText({ 
  
  numeros <- data()
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (min(numeros) >= 0) paste("" ,sep="")
  else paste("Valor x incorrecto. No puede ser un valor negativo")  
})
# Fin Texto 1
###################################################################



# Texto 2  - Valor Alfa
output$text2 <- renderText({ 
  
  numeros <- data()
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (input$lambda < 0) paste("Valor lambda incorrecto. Debe ser un valor mayor o igual a 0." ,sep="")
  else paste("", sep="")
})
# Fin Texto 2
##########################################################################







# # # La AYUDA
output$ex5 <- renderUI({
  if (!input$ex5_visible) return()
  
  
  
  helpText("En \"Eventos Favorables (x)\" Coloque los valores de la variable para los que desea calcular
           una probabilidad.", br(),
           "Coloque 0 para calcular la probabilidad de tener 0 eventos.", br(),br(),
           "Si desea calcular probabilidades de un grupo en particular,
           utilice las comas. Por ejemplo: 1,3,5", br(),br(),
           "Si desea calcular probabilidades en un rango puede utilizar los dos puntos.
           Por ejemplo, la expresión 1,2,3,4,5 es equivalente a escribir 1:5 
           para obtener la probabilidad entre 1 y 5 eventos.")
})
###########################################  




# # # Opciones de Ecuacion
output$ex6 <- renderUI({
  if (!input$ex6_visible) return()
  
  withMathJax("La fórmula de la Distribución Poisson es: $$P(X=x)= \\frac{\\lambda^{x}*e^{-x}}{x!}$$", 
              "Esto se lee como... Dada una variable  (\"X\" mayúscula), la probabilidad de un valor 
              en particular de dicha variable  (\"x\" minúscula) se obtiene como... el producto de...",
              br(),
              "la probabilidad del evento favorable (p) elevado a la \"x\", por la probabilidda de
              que no ocurra el evento favorable (q) elevado a la \"n-x\", multiplicado por 
              la combinatorio de \"x\" en \"n\".")
})
###############################################################



# # # Opción de que se desarrolle un ejercicio...
output$ex7 <- renderUI({
  if (!input$ex7_visible) return()
  
  texto_01 <- c("Observando la fórmula general, lo que nos queda es cambiar las letras por los
                valores particulares, y luego resolver.",
                "De esta forma tenemos la ecuación general:")
  
  
  formula_general <- "$$P(X=x)= \\frac{\\lambda^{x}*e^{-x}}{x!}$$"
  
  
  numeros <- data()
  
  decimales <- input$decimales
  lambda <- input$lambda
  x <- numeros[1] 
  e <- exp(1)
  e <- round(e, decimales)
  
  # Valor 1... lambda elevado a la x
  valor1 <- lambda^x
  valor1 <- round(valor1, decimales)
  
  
  # Valor 2... e a la -x
  valor2 <- e^(-x)
  valor2 <- round(valor2, decimales)
  
  # Valor 3... factorial de x
  valor3 <- factorial(x)
  valor3 <- round(valor3, decimales)
  
  # Resultado
  resultado <- (valor1*valor2)/valor3
  resultado <- round(resultado, decimales)
  
  texto_02 <- paste("Sabemos que $$\\lambda=", lambda, ",  x=", x, "$$, Debemos
                    realizar este calculo para cada valor de \"x\". Reemplazamos y se resuelve de la siguiente manera:", sep="")
  
  "$$P(X=x)= \\frac{\\lambda^{x}*e^{-x}}{x!}$$"
  
  formula_particular_1 <- paste("$$P(X=",x,")=\\frac{",lambda,"^{",x,"}*",e,"^{(","-",x,")}}{{",x,"!}}$$", sep="")
  
  # formula_particular_2 <- paste("$$P(X=",x,")=\\frac{",valor1,"*",valor2,"}{", valor3,"}$$", sep="")
  
  formula_particular_3 <- paste("$$P(X=",x,")=", resultado, "$$", sep="")
  
  # Son numeros muuuuuy grandes... directamente... de la exposicion pasamos al resultado...
  unificacion_resolucion <- paste(formula_particular_1,  formula_particular_3)
  
  texto_03 <- paste("Resumiendo, el valor de probabilidad para x=",x, " es   ", resultado, ".", sep="")
  
  withMathJax(texto_01, formula_general, texto_02, unificacion_resolucion, texto_03)
})
######################################################################





# # # Opción que se desarrolle todo lo que el usuario pidió...
output$ex8 <- renderUI({
  if (!input$ex8_visible) return()
  
  texto_01 <- c("Observando la fórmula general, lo que nos queda es cambiar las letras por los
                valores particulares, y luego resolver.",
                "De esta forma tenemos la ecuación general:")
  
  
  formula_general <- "$$P(X=x)= \\frac{\\lambda^{x}*e^{-x}}{x!}$$"
  
  
  texto_02 <- paste("Debemos realizar este calculo para cada valor de \"x\", y con cada valor obtenido
                    se construye la \"Tabla de Probabilidades Completa\". Reemplazamos en la 
                    fórmula, y resolvemos en cada caso. A continuación, cada calculo detallado
                    de los valores de \"x\" indicados en el manú:", sep="")
  
  # texto_02 <- c(texto_02, "br()")  
  
  # Mini Analisis
  {
    numeros <- data()
    
    decimales <- input$decimales
    lambda <- input$lambda
    
    e <- exp(1)
    e <- round(e, decimales)
    
    valores_x_indicados <- numeros
    prob_indicados <- dpois(valores_x_indicados,  lambda)
    prob_indicados <- round(prob_indicados, decimales)
    
    prob_acum_indicados <- cumsum(prob_indicados)
    
    
    SNIPER <- NA
    
    for (dota in 1:length(numeros)) {
      
      # Valor de x para cada caso
      x <- numeros[dota] 
      
      # Valor 1... lambda elevado a la x
      valor1 <- lambda^x
      valor1 <- round(valor1, decimales)
      
      
      # Valor 2... e a la -x
      valor2 <- e^(-x)
      valor2 <- round(valor2, decimales)
      
      # Valor 3... factorial de x
      valor3 <- factorial(x)
      valor3 <- round(valor3, decimales)
      
      
      # # # Calculo a mano de cada probabilidad  
      #   resultado <- valor1*valor2*combinatorio
      #   resultado <- round(resultado, decimales)
      
      # # # Le chanto el valor que obtuvo el soft
      # Esto es por que hay diferencias en los decimales
      # entre los calculos a mano, y los que arroja el soft...
      # Entonces.. le hago como que los calculo a mano, pero despues le muestro el resultado del soft
      
      # Resultado
      resultado <- (valor1*valor2)/valor3
      resultado <- round(resultado, decimales)
      
      
      # minitexto <- paste("$$Cálculo para P(X=", x,")$$", sep="")
      
      
      "$$P(X=x)= \\frac{\\lambda^{x}*e^{-x}}{x!}$$"
      
      formula_particular_1 <- paste("$$P(X=",x,")=\\frac{",lambda,"^{",x,"}*",e,"^{(","-",x,")}}{{",x,"!}}$$", sep="")
      
      # formula_particular_2 <- paste("$$P(X=",x,")=\\frac{",valor1,"*",valor2,"}{", valor3,"}$$", sep="")
      
      formula_particular_3 <- paste("$$P(X=",x,")=", resultado, "$$", sep="")
      
      unificacion_resolucion <- paste(formula_particular_1, formula_particular_3)
      
      fin <- c("$$#################################################################$$")
      
      
      texto_03 <- paste("Resumiendo, el valor de probabilidad para x=",x, " es   ", resultado, ".", sep="")
      
      
      SNIPER <- c( SNIPER, unificacion_resolucion, texto_03, fin)
      
      remove(unificacion_resolucion, texto_03)
      
      
      ###
    } # Fin for dota
    ############################################################
    
    SNIPER <- SNIPER[2:length(SNIPER)]
    
  } # Fin Minianalisis...
  ###############################################################
  
  
  withMathJax(texto_01, formula_general, texto_02, fin, br(),SNIPER)
})
######################################################################


