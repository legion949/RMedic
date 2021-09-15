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
##########################################################################

# Declaramos cada error posible

# ERROR1 - Valor p menor a 0
ERROR1 <- reactive({
  
  valor_p <- intro$p
  
  if (valor_p < 0) FALSE else TRUE
  
}) # Fin ERROR1
###########################################################################


# ERROR2 - Valor p mayor a 1
ERROR <- reactive({
  
  # # # Listado de errores
  # 1) Valor p no numerico
  # 2) Valorp menor a 0
  # 3) Valor p mayaor a 1
  # 4) Valores de x no numericos
  # 5) Valores de n no numericos
  # 6) Valos de x mayores a n
  # 7) Valores de redondeo negativos
  # 8) Valores de redondeo no numericos
  
  
  
  # Ingresa el valor p
  valor_p <- intro$p
  
  
  
  # Por defecto, si no hay errores, queda en 0
  
  
  
  error <- 0
  
  # Si el valor p no es numerico... es un 1
  if (!is.numeric(valor_p)) error <- 1  else {
    
    # Si el valor p es menor a 0... es un 2
    if( valor_p < 0) error <- 2 else{
      
      # Si el valor p es mayor a 1... es un 3
      if (valor_p > 1) error <- 3
      
    } # Fin else 2
  } # Fin else 1
  
}) # Fin ERROR1
###########################################################################



# # # Grafico 1: Frec. Relativas de una Distribución Binomial
output$distPlot1 <- renderPlot({
  
  # Llamamos a los numeros que ingreso el usuario...
  numeros <- data()
  
  # Si los numeros de x son menores que el n... 
  # Y el valor p esta entre 0 y 1...
  # Generara el grafico
  if (max(numeros) <= input$n & input$p<=1 & input$p>=0) {
    
    # Datos Binomial 
    ###########
    {
      ###  
      
      # Todo el rango de valores de la variables, desde 0 hasta n
      valores_x <- c(0:input$n)
      
      # Probabilidades de cada valor de x
      prob <- dbinom(valores_x, input$n, input$p)
      
      # Probabilidades acumuladas
      prob_acum <- cumsum(prob)
      
      
      ###  
    } # Fin Datos Binomial
    ###########################
    
    
    # # # Grafico 1     
    
    # Guardamos las opciones gráficas anteriores
    opar <- par()
    
    # Modificamos los margenes y otras cosas que no me acuerdo...
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    # Creamos el grafico en si...
    plot(valores_x, prob, ylim=c(0,1), main="Distribución Binomial", axes=FALSE, xlab=c("Eventos Favorables (x)"),  ylab=c("Probabilidad"), col=input$variable_color, cex.main=2, cex.lab=2)
    
    # Agregamos ejes con caracteristicas particulares
    axis(2,  las=1, cex.axis=2)
    axis(1, valores_x ,las=1, cex=1, cex.axis=2)
    
    segments(x0= valores_x, y0= 0, x1= valores_x, y1= prob, col= input$variable_color, lwd=5)
    # 
    # # Agregamos cada palito de probabilidad de cada valor de x.
    # for(i in 1:length(valores_x)){
    #   segments(x0= valores_x[i], y0= 0, x1= valores_x[i], y1= prob[i], col= input$variable_color, lwd=5)
    #   # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución Normal Estándard", add=T)
    # } # Fin for i
    # ###########################################################################
    # 
    # Reintegramos las opciones graficas iniciales
    par <- opar
    
  } # Fin if  x <= n
  
  ###################
  # IMPORTANTE
  # Faltaría agregar algo para cuando los elementos están mal puestos
  # En vez de que genere el grafico... ponga un imagen... o algo...
  # que le indique qué es lo que está mal de los datos que ingreso.
  ##############################################################################
}) # Fin Plot 1
#################################################################################



# Gráfico 2: Frec. Relativas Acumuladas
output$distPlot2 <- renderPlot({
  
  # Llamamos a los numeros que ingreso el usuario...
  numeros <- data()
  
  # Si los numeros de x son menores que el n... 
  # Y el valor p esta entre 0 y 1...
  # Generara el grafico de probabilidades acumuladas
  if (max(numeros)  <= input$n & input$p<=1 & input$p>=0) {
    
    
    # Datos Binomial 
    ######################
    {
      ###  
      # Valores de la variable... va desde 0 hasta "n".
      valores_x <- c(0:input$n)
      
      # Valores de probabilidad de cada valor posible de la bariable
      prob <- dbinom(valores_x, input$n, input$p)
      
      # Probabilidades acumuladas
      prob_acum <- cumsum(prob)
      
      ###  
    } # Fin Datos Binomial
    ###########################
    
    # Guardamos las opciones graficas por defecto.    
    opar <- par()
    
    # Modificamos las opciones graficas... no me acuerdo que hace cada una...
    par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0))
    
    # Creamos el grafico en si...
    plot(valores_x, prob_acum, ylim=c(0,1), main="Distribución Binomial Acumulada", axes=FALSE, xlab=c("Eventos Favorables (x)"), ylab=c("Prob. Acum"), col=input$variable_color, cex.main=2, cex.lab=2)
    
    # Le agregamos los ejes modificados
    axis(2,  las=1, cex.axis=2)
    axis(1, valores_x ,las=1, cex=1, cex.axis=2)
    
    segments(x0= valores_x, y0= 0, x1= valores_x, y1= prob_acum, col= input$variable_color, lwd=5)
    # # Agregamos los palitos de cada probabilidad
    # for(i in 1:length(valores_x)){
    #   segments(x0= valores_x[i], y0= 0, x1= valores_x[i], y1= prob_acum[i], col= input$variable_color, lwd=5)
    #   # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución Normal Estándard", add=T)
    # } # Fin for i
    # ##############################################################################################
    
    # Volvemos a poner las opciones por defecto...
    par <- opar
  } # Fin if  x <= n
  
  ###################
  # IMPORTANTE
  # Faltaría agregar algo para cuando los elementos están mal puestos
  # En vez de que genere el grafico... ponga un imagen... o algo...
  # que le indique qué es lo que está mal de los datos que ingreso.
  ##############################################################################
  
  
}) # Fin Plot 2
#################################################################################




# Tabla de Parámetros y Afines
# Tenga el p, q, n , todos los x que pidio...
# los valores de probabilidad, y las probabilidades acumuladas...

sliderValues1 <- reactive({
  
  # Llamamos a los numeros... 
  # Asi se regenera esta tabla
  numeros <- data()
  
  # Armamos un data.frame con todo lo que hace falta
  data.frame(
    p = c(as.character(round(input$p, input$decimales))), # valor p
    q=c(as.character(round(1-input$p, input$decimales))), # valor q
    n=c(as.character(input$n)), # n
    x=c(as.character(data())), # los valores de x que pidio el usuario
    prob = c(as.character(round(c(dbinom(data(), input$n, input$p)), input$decimales))), # las probabilidades de lo que pidio
    prob.acum = c(as.character(cumsum(round(c(dbinom(data(), input$n, input$p)), input$decimales)))), # las probabilidades acumuladas de lo que pidio
    stringsAsFactors=FALSE)
  
  #######################################################
}) # FIn sliderValues1
####################################################

# Avi
output$Values1 <- renderTable({
  sliderValues1()
})
# Fin Tabla
##################################################  



# Tabla de Probabilidades Totales
# Tiene todos los valores de la variable... desde 0 hasta n
# Y todas las probabilidades acumuladas...
sliderValues2 <- reactive({
  
  # Llamamos a los numeros que ingreso el usuario
  numeros <- data()
  
  
  if (max(numeros) <= input$n & input$p<=1 & input$p>=0) {
    
    # Datos Binomial 
    ###########
    {
      ###  
      
      # Todos los valores posibles... desde 0 hasta n
      valores_x <- c(0:input$n)
      
      # Los valores de probabilidad
      prob <- dbinom(valores_x, input$n, input$p)
      prob <- round(prob, input$decimales)
      
      # Los valores de probabilidad acumulados
      prob_acum <- cumsum(prob)
      
      ###  
    } # Fin Datos Binomial
    ###########################
    
    
    # Creamos un data.frame
    data.frame(
      x = valores_x,   # Todos los valores desde 0 hasta n
      Prob = as.character(round(prob, input$decimales)),  # Todos los valores de probabilidad
      ProbAcum = as.character(round(prob_acum, input$decimales)), # Todos los valores de prob. acum.
      stringsAsFactors=FALSE)
    
  } # Fin if
  #############################################################################
  
  
  ###################
  # IMPORTANTE
  # Faltaría agregar algo para cuando los elementos están mal puestos
  # En vez de que genere el grafico... ponga un imagen... o algo...
  # que le indique qué es lo que está mal de los datos que ingreso.
  ##############################################################################
  
  
})  # FIn sliderValues1
#######################################################################

# Activamos la tabla anterior (me parece)
output$Values2 <- renderTable({
  sliderValues2()
})
# Fin Tabla
##################################################  




# Texto 1  - Valor Alfa
output$text1 <- renderText({ 
  
  
  # # # Pedimos los numeros que ingreso el usuario...
  numeros <- data()
  
  # Si maximo de x es menor o igual que el n... Esta todo OK.
  # Entonces no sale ningún cartel.
  if (max(numeros) <= input$n) paste("" ,sep="")
  # Pero si no se cumple lo anterior... se le da un cartel de aviso.
  else paste("Valor de x o n incorrectos. El valor x no puede ser mayor que n.")  
})
# Fin Texto 1
##########################################################################################



# Texto 2  - Valor de p
output$text2 <- renderText({ 
  
  # Si el valor de probabilidad es menor a 0, le sale un cartel...
  if (input$p < 0) paste("Valor p incorrecto. Está indicado un valor menor a 0. El valor p debe ser un número entre 0 y 1." ,sep="")
  
  # Si el valor de probabilidad es mayor a 1... le sale otro cartel
  else if (input$p > 1) paste("Valor p incorrecto. Está indicado un número mayor a 1. El valor p debe ser un número entre 0 y 1.", sep="")  
  
  # Si el valor de probabilidad está bien... no sale cartel alguno...
  else paste("", sep="")
})
# Fin Texto 2
###########################################################################################


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
  
  withMathJax("La fórmula de la Distribución Binomial es: $$P(X=x)=p^x*q^{(n-x)}*\\binom{n}{x}$$", 
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
  
  
  formula_general <- "$$P(X=x)=p^x*q^{(n-x)}*\\binom{n}{x}$$"
  
  
  numeros <- data()
  
  decimales <- input$decimales
  n <- input$n
  x <- numeros[1] 
  p <- round(as.numeric(input$p), decimales)
  q <- round( (1 - as.numeric(input$p)), decimales)
  combinatorio <- choose(n,x)
  valor1 <- p^x
  valor1 <- round(valor1, decimales)
  
  
  
  valor2 <- q^(n-x)
  valor2 <- round(valor2, decimales)
  
  resultado <- valor1*valor2*combinatorio
  resultado <- round(resultado, decimales)
  
  texto_02 <- paste("Sabemos que $$n=", n, ",  x=", x, ",  p=", p, ",   q=1-p=", q, "$$, Debemos
                    realizar este calculo para cada valor de \"x\". Reemplazamos y se resuelve de la siguiente manera:", sep="")
  
  
  formula_particular_1 <- paste("$$P(X=",x,")=",p,"^{",x,"}*",q,"^{(",n,"-",x,")}*\\binom{",n,"}{",x,"}$$", sep="")
  
  formula_particular_2 <- paste("$$P(X=",x,")=",p,"^{",x,"}*",q,"^{",n-x,"}*", combinatorio,"$$", sep="")
  
  formula_particular_3 <- paste("$$P(X=",x,")=",valor1,"*",valor2,"*", combinatorio,"$$", sep="")
  
  formula_particular_4 <- paste("$$P(X=",x,")=", resultado, "$$", sep="")
  
  unificacion_resolucion <- paste(formula_particular_1, formula_particular_2, formula_particular_3, formula_particular_4)
  
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
  
  
  formula_general <- "$$P(X=x)=p^x*q^{(n-x)}*\\binom{n}{x}$$"
  
  
  texto_02 <- paste("Debemos realizar este calculo para cada valor de \"x\", y con cada valor obtenido
                    se construye la \"Tabla de Probabilidades Completa\". Reemplazamos en la 
                    fórmula, y resolvemos en cada caso. A continuación, cada calculo detallado
                    de los valores de \"x\" indicados en el manú:", sep="")
  
  # texto_02 <- c(texto_02, "br()")  
  
  # Mini Analisis
  {
    numeros <- data()
    
    decimales <- input$decimales
    n <- input$n
    p <- round(as.numeric(input$p), decimales)
    q <- round( (1 - as.numeric(input$p)), decimales)
    
    valores_x_indicados <- numeros
    prob_indicados <- dbinom(valores_x, n, p)
    prob_indicados <- round(prob_indicados, decimales)
    
    prob_acum_indicados <- cumsum(prob)
    
    
    SNIPER <- NA
    
    for (dota in 1:length(numeros)) {
      
      x <- numeros[dota] 
      combinatorio <- choose(n,x)
      valor1 <- p^x
      valor1 <- round(valor1, decimales)
      
      
      
      valor2 <- q^(n-x)
      valor2 <- round(valor2, decimales)
      
      # # # Calculo a mano de cada probabilidad  
      #   resultado <- valor1*valor2*combinatorio
      #   resultado <- round(resultado, decimales)
      
      # # # Le chanto el valor que obtuvo el soft
      # Esto es por que hay diferencias en los decimales
      # entre los calculos a mano, y los que arroja el soft...
      # Entonces.. le hago como que los calculo a mano, pero despues le muestro el resultado del soft
      
      resultado <- prob_indicados[dota]  
      
      
      # minitexto <- paste("$$Cálculo para P(X=", x,")$$", sep="")
      
      
      
      formula_particular_1 <- paste("$$P(X=",x,")=",p,"^{",x,"}*",q,"^{(",n,"-",x,")}*\\binom{",n,"}{",x,"}$$", sep="")
      
      formula_particular_2 <- paste("$$P(X=",x,")=",p,"^{",x,"}*",q,"^{",n-x,"}*", combinatorio,"$$", sep="")
      
      # formula_particular_3 <- paste("$$P(X=",x,")=",valor1,"*",valor2,"*", combinatorio,"$$", sep="")
      
      formula_particular_3 <- c() # Lo saque, por que sino, los numeros muy chiquitos de prbo
      # No los puedo poner.
      
      formula_particular_4 <- paste("$$P(X=",x,")=", resultado, "$$", sep="")
      
      unificacion_resolucion <- paste(formula_particular_1, formula_particular_2, formula_particular_3, formula_particular_4)
      
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
