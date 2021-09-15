# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot

# Reactive expression to compose a data frame containing all of
# the values

# Tabla de Valores z y probabilidad

z_izq_interno <- reactive({
  
  if(!is.null(input$aver1)) {
    
    if (input$aver1 == TRUE) {
      zi <- -500 
      } else  zi <- input$z_izq
  }
  
})

z_der_interno <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      zd <- 500 
    } else  zd <- input$z_der
  }
  
})

z_izq_externo <- reactive({
  
  if(!is.null(input$aver1)) {
    
    if (input$aver1 == TRUE) {
      zi <- "-Inf"
    } else  zi <- input$z_izq
  }
  
})


z_der_externo <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      zd <- "+Inf" 
    } else  zd <- input$z_der
  }
  
})


# observe(  cat(z_izq_interno(), "\n"))
# observe(  cat(z_der_interno(), "\n"))

# Valor de Probabilidad

prob_interno <- reactive({
  
  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  
  
  if (z_izq <= z_der) {
    
    p2 <- pnorm(z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p1 <- pnorm(z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    prob <- p2 - p1
    prob <- round(prob, decimales)
    
    prob
  
  }
})  


sliderValues <- reactive({

  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (z_izq <= z_der) {
    
   
    prob <- probabilidad
    
    # Compose data frame
    data.frame(
      Descripción = c("Zi", 
                      "Zd",
                      "Probabilidad"),
      Valores = as.character(c(z_izq_externo(), 
                               z_der_externo(),
                               prob)), 
      stringsAsFactors=FALSE)
  } # Fin if <
  else {
    # Compose data frame
    data.frame(
      Descripción = c("Zi", 
                      "Zd",
                      "Probabilidad"),
      Valores = as.character(c("Valores Incorrectos", 
                               "Valores Incorrectos",
                               "Valores Incorrectos")), 
      stringsAsFactors=FALSE)
    
  } # FIn else
  #######################################################
}) 

observe(
output$values <- renderTable(align= "c", digits=input$decimales,{
  sliderValues()
}))

# Fin Tabla
##################################################  

valores_z_extremos <- c(-5,5)
cantidad <- 400
cantidad_desvio <- max(valores_z_extremos)

# Inicio: Gráfico de la Distribucion Normal
output$distPlot <- renderPlot({
  ### x    <- faithful[, 2]  # Old Faithful Geyser data
  ### bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  
  
  
  x <- seq(valores_z_extremos[1], valores_z_extremos[2], length=cantidad)
  y <- dnorm(x,mean=0, sd=1)
  x_mod <- x

  
  # Determino los valores que quiero que salgan en el eje X de la normal estandard
  x_standard <- valores_z_extremos[1]:valores_z_extremos[2]
  
  
  # Parametros Graficos
  opar <- par()
  par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0)) 
  

  detalle <- c(valores_z_extremos[1]:valores_z_extremos[2])
  valores_x <- detalle

  valores_x_mod <- valores_x
  valores_x_mod[1] <- "-Inf"
  valores_x_mod[length(valores_x_mod)] <- "+Inf"

  
  # Generamos la grafica de la distribucion normal
  curve(dnorm(x, 0, 1), xlim= valores_z_extremos, xlab="Valores de Z", ylab="Frecuencia Relativa",
        main="Distribución Normal Estándard", ylim=c(0,0.5), axes= F, col=input$color)
  axis(2,  las=1)
  axis(1, c(x_standard), labels=valores_x_mod ,las=1)
  
  zi <- z_izq_interno()
  zd <- z_der_interno()
  
  li <- zi
  ld <- zd
  
  cat(li, "\n", ld, "\n")
  x_pintada <- seq(li, ld, by= 0.001)
  #      dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
  #      x_pintada <- x_pintada[dt_x]
  y_pintada <- dnorm(x_pintada)
  
  segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$color)

  
    par <- opar

  
  # FillCurve(input$range[1],input$range[2])
  
 # FillCurve(input$z_izq,input$z_der, Nmin=valores_z_extremos[1], Nmax=valores_z_extremos[2], z_ext=valores_z_extremos)
  
  
  
})
# Fin Gráfico
###############################################



# Texto de EmeRgencia
output$ER1 <- renderText({ 
  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (z_izq > z_der) {
    paste("WARNING!!! El valor Zi debe ser menor o igual al valor Zd", sep="")
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################


# Texto de EmeRgencia
output$ER2 <- renderText({ 
  if (input$z_izq < input$z_der) {
    if (input$z_izq < valores_z_extremos[1] & input$z_der < valores_z_extremos[1]) {
      paste("Los valores Z indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
    else if (input$z_izq < valores_z_extremos[1] & input$z_der >= valores_z_extremos[1]) {
      paste("El valor Zi queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (input$z_izq <= valores_z_extremos[2] & input$z_der > valores_z_extremos[2]) {
      paste("El valor Zd queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (input$z_izq > valores_z_extremos[2] & input$z_der > valores_z_extremos[2]) {
      paste("Los valores Z indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################



# Texto 1
output$text1 <- renderText({ 
  
  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (z_izq < z_der) paste("Tú has seleccionado el rango de valores Z entre ", z_izq_externo(), " y ", z_der_externo(), " .", sep="")
  
  # Si el valor izquierdo y derecho son iguales... Puede ser Correcto
  else if (input$z_izq == input$z_der) paste("Los valores Z izquierdo y derecho son iguales.", sep="")
  
  # Si el valor izquierdo es mayor que el derecho... INCORRECTO
  else if (input$z_izq > input$z_der) paste("Atención: el valor z izquierdo no puede ser mayor al z derecho", sep="")
})
# Fin Texto 1
##############



# Texto 2
output$text2 <- renderText({ 
  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (z_izq < z_der) {
   prob <- probabilidad

    paste("El valor de probabilidad obtenido es ",prob, ".",sep="")
  } # Fin if <
  else if (z_izq == z_der ) paste("El valor de probabilidad es 0 (cero).",sep="")
})
# Fin Texto 2
##############


# Texto 3
output$text3 <- renderText({ 
  
  z_izq <- z_izq_interno()
  z_der <- z_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  if (z_izq < z_der) {
  
    
    
    p1 <-   paste("Para obtener el valor de probabilidad en un rango realizamos:", sep="")
    p2 <-   paste("$$p(",z_izq_externo(),"<Z<",z_der_externo(),")= p(Z<",z_der_externo(),") - p(Z<",z_izq_externo(),")$$", sep="")
    
   
    aver_prob_der <- pnorm(z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    aver_prob_der <- round(aver_prob_der, decimales)

    aver_prob_izq <- pnorm(z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    aver_prob_izq <- round(aver_prob_izq, decimales)
    
    
    # LA1 <- c("$$p(Z<",z_der,")=", p2, "$$")
    p3 <-   paste("$$p(",z_izq_externo(),"<Z<",z_der_externo(),")=", aver_prob_der, " - ", aver_prob_izq, "$$", sep="")
    
    p4 <-   paste("$$p(",z_izq_externo(),"<Z<",z_der_externo(),")=", probabilidad, "$$", sep="")
    
        
    frase_total <- paste(p1, p2, p3, p4)
    
  } # Fin if <
  else paste("",sep="")
})
# Fin Texto 3
##############





# Texto 5
output$text5 <- renderText({ 
  if (input$z_izq < input$z_der) {
    
    p2 <- pnorm(input$z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p2 <- round(p2, 4)
    
    LA1 <- c("1) p(Z<",input$z_der,")=", p2)
    
    paste(LA1, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 5
##############


# Texto 6
output$text6 <- renderText({ 
  if (input$z_izq < input$z_der) {
    
    p1 <- pnorm(input$z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    p1 <- round(p1, 4)
    
    LA2 <- c("2) p(Z<",input$z_izq,")=", p1)
    
    paste(LA2, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 6
##############


# Texto 7
output$text7 <- renderText({ 
  if (input$z_izq < input$z_der) {
    
    p2 <- pnorm(input$z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p1 <- pnorm(input$z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    p1 <- round(p1, 4)
    p2 <- round(p2, 4)
    
    p_result <- p2 - p1
    p_result <- round(p_result,4)
    
    LA3 <- c("3) p(",input$z_izq,"<Z<",input$z_der, ")= ","p(Z<",input$z_der,") -","p(Z<",input$z_izq,")", "= ", p2, "-", p1, "= ", p_result ,sep="")
    
    paste(LA3, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 7
##############

# Texto 8
output$ex4 <- renderUI({
  if (input$z_izq < input$z_der) {
    p2 <- pnorm(input$z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    
    
    withMathJax(sprintf(" $$p(Z \\leq %.03f ) = %.04f$$", input$z_der, p2))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 8
########################


# Texto 9
output$ex5 <- renderUI({
  if (input$z_izq < input$z_der) {
    # invalidateLater(5000, session)
    p1 <- pnorm(input$z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    withMathJax(sprintf(" $$p(Z \\leq %.03f ) = %.04f$$", input$z_izq, p1))
    
    
    
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 9
########################






# Texto 10
output$ex6 <- renderUI({
  if (input$z_izq < input$z_der) {
    # invalidateLater(5000, session)
    p2 <- pnorm(input$z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p1 <- pnorm(input$z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    
    withMathJax(sprintf(" $$p(%.03f\\leq    Z \\leq %.03f ) = %.04f - %.04f = %.04f$$", input$z_izq,input$z_der, p2, p1, (p2-p1)))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 10
########################


# Texto 11
output$ex7 <- renderUI({
  if (input$z_izq < input$z_der) {
    # # invalidateLater(5000, session)
    # p2 <- pnorm(input$z_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    # p1 <- pnorm(input$z_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    # 
    
  #  withMathJax(sprintf("En el libro de Bioestadística 1, en el capítulo 5 llamado \"Variables Aleatorias I\" página 71 en adelante se desarrolla sobre la Distribución Normal, de allí se obtiene la siguiente ecuación: $$p(z_i\\leq  Z \\leq z_d ) = p(Z \\leq z_d) - p(Z \\leq z_i )$$"))
    
    withMathJax(sprintf("$$p(z_i\\leq  Z \\leq z_d ) = p(Z \\leq z_d) - p(Z \\leq z_i )$$"))
  } # Fin if
  else withMathJax(sprintf(""))
})
# Fin Texto 11
########################