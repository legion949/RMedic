# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot

# Reactive expression to compose a data frame containing all of
# the values
# 
# output$t_izq <- renderUI({
#   
#   valor1 <- pt((0.05/2), input$df)
#   valor1 <- round(valor1, input$decimales)
#   
#   numericInput("t_izq", "Valor t izquierdo (ti):", min=-1e20,  max=1e20, step= 0.01, value=-1.96)
#   
# })
# 
# output$t_der <- renderUI({
#   
#   valor2 <- pt((1-(0.05/2)), input$df)
#   valor2 <- round(valor2, input$decimales)
#   
#   
#   numericInput("t_der", "Valor t izquierdo (td):", min=-1e20,  max=1e20, step= 0.01, value=1.96)
#   
# })



# Tabla de Valores z y probabilidad

t_izq_interno <- reactive({
  
  if(!is.null(input$aver1)) {
    
    if (input$aver1 == TRUE) {
      ti <- -500 
      } else  ti <- input$t_izq
  }
  
})

t_der_interno <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      td <- 500 
    } else  td <- input$t_der
  }
  
})

t_izq_externo <- reactive({
  
  if(!is.null(input$aver1)) {
    
    if (input$aver1 == TRUE) {
      ti <- "-Inf"
    } else  ti <- input$t_izq
  }
  
})


t_der_externo <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      td <- "+Inf" 
    } else  td <- input$t_der
  }
  
})


# observe(  cat(t_izq_interno(), "\n"))
# observe(  cat(t_der_interno(), "\n"))

# Valor de Probabilidad

prob_interno <- reactive({
  
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  gl <- input$df
  
  if (t_izq <= t_der) {
    
    p2 <- pt(t_der, gl)  
    p1 <- pt(t_izq, gl) 
    
    prob <- p2 - p1
    prob <- round(prob, decimales)
    
    prob
  
  }
})  


sliderValues <- reactive({
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (t_izq <= t_der) {
    
   
    prob <- probabilidad
    
    # Compose data frame
    data.frame(
      Descripción = c("ti", 
                      "td",
                      "Probabilidad"),
      Valores = as.character(c(t_izq_externo(), 
                               t_der_externo(),
                               prob)), 
      stringsAsFactors=FALSE)
  } # Fin if <
  else {
    # Compose data frame
    data.frame(
      Descripción = c("ti", 
                      "td",
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

valores_t_extremos <- c(-5,5)
cantidad <- 400


# Inicio: Gráfico de la Distribucion t
output$distPlot <- renderPlot({
  ### x    <- faithful[, 2]  # Old Faithful Geyser data
  ### bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  
  
  x <- seq(valores_t_extremos[1], valores_t_extremos[2], length=cantidad)
  y <- dt(x, df=input$df)
  x_mod <- x
  
  # # Parametros Graficos
  # opar <- par()
  # par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0)) 
  

  detalle <- c(valores_t_extremos[1]:valores_t_extremos[2])
  valores_x <- detalle

  valores_x_mod <- valores_x
  valores_x_mod[1] <- "-Inf"
  valores_x_mod[length(valores_x_mod)] <- "+Inf"

  valores_y <- seq(0, 0.4, by=0.1)
  
  plot(x_mod, y, xlab="Valores de t", 
       ylab="Frecuencias Relativas", 
       main="Distribución t Estándard", 
       axes=F, col="white",
       ylim=c(0,max(valores_y)))
  
  #  Forma de la grafica
  for(i in 1:(length(x_mod)-1)){
    segments(x0= x_mod[i], y0= y[i], x1= x_mod[i+1], y1= y[i+1], col=input$color)
    # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución t Estándard", add=T)
    
  } # Fin for i
  
  
  # Coloracion
  x1 <- t_izq_interno()
  x2 <- t_der_interno()
  
  if (input$aver1 == TRUE) x1 <- min(x)
  if (input$aver2 == TRUE) x2 <- max(x)
  
  s <- seq(from= x1, to= x2, by= 0.001)
  for(i in 1:length(s)){
    segments(x0= s[i], y0= 0, x1= s[i], y1= dt(s[i], df=input$df), col= input$color)
    # curve(dnorm(x, mean, sd),xlim= c(Nmin, Nmax), xlab="Valores de Z", ylab="Frecuencia Relativa", main="Distribución t Estándard", add=T)

  } # Fin for i
  
  axis(2,  valores_y, labels=valores_y, las=1)
  axis(1, c(valores_x), labels=valores_x_mod ,las=1)
  
  
    # par <- opar
    # 
  
  # FillCurve(input$range[1],input$range[2])
  
 # FillCurve(input$t_izq,input$t_der, Nmin=valores_t_extremos[1], Nmax=valores_t_extremos[2], z_ext=valores_t_extremos)
  
  
  
})
# Fin Gráfico
###############################################



# Texto de EmeRgencia
output$ER1 <- renderText({ 
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (t_izq > t_der) {
    paste("WARNING!!! El valor ti debe ser menor o igual al valor td", sep="")
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################


# Texto de EmeRgencia
output$ER2 <- renderText({ 
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (t_izq > t_der) {
    
    if (t_izq < valores_t_extremos[1] & t_der < valores_t_extremos[1]) {
      paste("Los valores t indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
    else if (t_izq < valores_t_extremos[1] & t_der < valores_t_extremos[2]) {
      paste("El valor ti queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (t_izq <= valores_t_extremos[2] & t_der > valores_t_extremos[2]) {
      paste("El valor td queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (t_izq > valores_t_extremos[2] & t_der > valores_t_extremos[2]) {
      paste("Los valores t indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################



# Texto 1
output$text1 <- renderText({ 
  
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  gl <- input$df
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (t_izq < t_der) paste("Tú has seleccionado el rango de valores t entre ",
                           t_izq_externo(), " y ", t_der_externo(),
                           " con ", gl, " grados de libertad.", sep="")
  
  # Si el valor izquierdo y derecho son iguales... Puede ser Correcto
  else if (t_izq == t_der) paste("Los valores t izquierdo y derecho son iguales.", sep="")
  
  # Si el valor izquierdo es mayor que el derecho... INCORRECTO
  else if (t_izq > t_der) paste("Atención: el valor t izquierdo no puede ser mayor al t derecho", sep="")
})
# Fin Texto 1
##############



# Texto 2
output$text2 <- renderText({ 
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (t_izq < t_der) {
   prob <- probabilidad

    paste("El valor de probabilidad obtenido es ",prob, ".",sep="")
  } # Fin if <
  else if (t_izq == t_der ) paste("El valor de probabilidad es 0 (cero).",sep="")
})
# Fin Texto 2
##############


# Texto 3
output$text3 <- renderText({ 
  
  t_izq <- t_izq_interno()
  t_der <- t_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  if (t_izq < t_der) {
  
    
    
    p1 <-   paste("Para obtener el valor de probabilidad en un rango realizamos:", sep="")
    p2 <-   paste("$$p(",t_izq_externo(),"<t<",t_der_externo(),")= p(t<",t_der_externo(),") - p(t<",t_izq_externo(),")$$", sep="")
    
   
    aver_prob_der <- pnorm(t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    aver_prob_der <- round(aver_prob_der, decimales)

    aver_prob_izq <- pnorm(t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
    aver_prob_izq <- round(aver_prob_izq, decimales)
    
    
    # LA1 <- c("$$p(Z<",t_der,")=", p2, "$$")
    p3 <-   paste("$$p(",t_izq_externo(),"<Z<",t_der_externo(),")=", aver_prob_der, " - ", aver_prob_izq, "$$", sep="")
    
    p4 <-   paste("$$p(",t_izq_externo(),"<Z<",t_der_externo(),")=", probabilidad, "$$", sep="")
    
        
    frase_total <- paste(p1, p2, p3, p4)
    
  } # Fin if <
  else paste("",sep="")
})
# Fin Texto 3
##############





# Texto 5
output$text5 <- renderText({ 
  if (input$t_izq < input$t_der) {
    
    p2 <- pnorm(input$t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p2 <- round(p2, 4)
    
    LA1 <- c("1) p(Z<",input$t_der,")=", p2)
    
    paste(LA1, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 5
##############


# Texto 6
output$text6 <- renderText({ 
  if (input$t_izq < input$t_der) {
    
    p1 <- pnorm(input$t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    p1 <- round(p1, 4)
    
    LA2 <- c("2) p(Z<",input$t_izq,")=", p1)
    
    paste(LA2, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 6
##############


# Texto 7
output$text7 <- renderText({ 
  if (input$t_izq < input$t_der) {
    
    p2 <- pnorm(input$t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p1 <- pnorm(input$t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    p1 <- round(p1, 4)
    p2 <- round(p2, 4)
    
    p_result <- p2 - p1
    p_result <- round(p_result,4)
    
    LA3 <- c("3) p(",input$t_izq,"<Z<",input$t_der, ")= ","p(Z<",input$t_der,") -","p(Z<",input$t_izq,")", "= ", p2, "-", p1, "= ", p_result ,sep="")
    
    paste(LA3, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 7
##############

# Texto 8
output$ex4 <- renderUI({
  if (input$t_izq < input$t_der) {
    p2 <- pnorm(input$t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    
    
    withMathJax(sprintf(" $$p(Z \\leq %.03f ) = %.04f$$", input$t_der, p2))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 8
########################


# Texto 9
output$ex5 <- renderUI({
  if (input$t_izq < input$t_der) {
    # invalidateLater(5000, session)
    p1 <- pnorm(input$t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    withMathJax(sprintf(" $$p(Z \\leq %.03f ) = %.04f$$", input$t_izq, p1))
    
    
    
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 9
########################






# Texto 10
output$ex6 <- renderUI({
  if (input$t_izq < input$t_der) {
    # invalidateLater(5000, session)
    p2 <- pnorm(input$t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    p1 <- pnorm(input$t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    
    
    withMathJax(sprintf(" $$p(%.03f\\leq    Z \\leq %.03f ) = %.04f - %.04f = %.04f$$", input$t_izq,input$t_der, p2, p1, (p2-p1)))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 10
########################


# Texto 11
output$ex7 <- renderUI({
  if (input$t_izq < input$t_der) {
    # # invalidateLater(5000, session)
    # p2 <- pnorm(input$t_der, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)  
    # p1 <- pnorm(input$t_izq, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
    # 
    
  #  withMathJax(sprintf("En el libro de Bioestadística 1, en el capítulo 5 llamado \"Variables Aleatorias I\" página 71 en adelante se desarrolla sobre la Distribución t, de allí se obtiene la siguiente ecuación: $$p(z_i\\leq  Z \\leq z_d ) = p(Z \\leq z_d) - p(Z \\leq z_i )$$"))
    
    withMathJax(sprintf("$$p(t_i\\leq  t \\leq t_d ) = p(t \\leq t_d) - p(t \\leq t_i )$$"))
  } # Fin if
  else withMathJax(sprintf(""))
})
# Fin Texto 11
########################