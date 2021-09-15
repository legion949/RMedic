# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot




chi_izq_interno <- reactive({
  
    chi_izq <- input$chi_izq
  })
  
chi_der_interno <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      chi_der <- 500 
    } else  chi_der <- input$chi_der
  }
  
})


chi_izq_externo <- reactive({
  
  chi_izq <- input$chi_izq
  
})


chi_der_externo <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      chi_der <- "+Inf" 
    } else  chi_der <- input$chi_der
  }
  
})

# Tabla de Valores z y probabilidad


prob_interno <- reactive({
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  gl <- input$df
  decimales <- input$decimales
  
  
  if (chi_izq <= chi_der) {
    
    p2 <- pchisq(chi_der, df=gl)  
    p1 <- pchisq(chi_izq, df=gl) 
    
    prob <- p2 - p1
    prob <- round(prob, decimales)
    
    prob
    
  }
})  



sliderValues <- reactive({
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  gl <- input$df
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (chi_izq <= chi_der) {

    
    # Compose data frame
    data.frame(
      Descripción = c("Grados de Libertad", 
                      "Chi izq", 
                      "Chi der",
                      "Probabilidad"),
      Valores = as.character(c(gl, 
                               chi_izq, 
                               chi_der_externo(),
                               probabilidad)), 
      stringsAsFactors=FALSE)
  } # Fin if <
  else {
    # Compose data frame
    data.frame(
      Descripción = c("Grados de Libertad", 
                      "Chi izq", 
                      "Chi der",
                      "Probabilidad"),
      Valores = as.character(c("Valores Incorrectos",
                               "Valores Incorrectos", 
                               "Valores Incorrectos",
                               "Valores Incorrectos")), 
      stringsAsFactors=FALSE)
    
  } # FIn else
  #######################################################
}) 

output$values <- renderTable({
  sliderValues()
})
# Fin Tabla
##################################################  

valores_chi_extremos <- c(0,40)
cantidad <- 400


# Inicio: Gráfico de la Distribucion Chi Cuadrado
output$distPlot <- renderPlot({
  ### x    <- faithful[, 2]  # Old Faithful Geyser data
  ### bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  if(chi_der > 40 ) chi_der <- valores_chi_extremos[2]
  
    gl <- input$df
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  x <- seq(valores_chi_extremos[1], valores_chi_extremos[2], length=cantidad)
  y <- dchisq(x, df=input$df)
  
  # Determino los valores que quiero que salgan en el eje X de la t
  # x_standard <- valores_chi_extremos[1]:valores_chi_extremos[2]
  x_standard <- valores_chi_extremos[1]:valores_chi_extremos[2]
  
  # Parametros Graficos
  opar <- par()
  par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0)) 
  
  
  # Generamos la grafica de la distribucion t
  curve(dchisq(x, df=input$df ),xlim= valores_chi_extremos, xlab="Valores Chi", ylab="Frecuencia Relativa",
        main="Distribución Chi Cuadrado", 
        ylim=c(0,0.2), 
        axes= F)
  axis(2,  las=1, cex.axis=2)
  #axis(1, c(x_standard), labels=x_standard ,las=1)
  axis(1, c(x_standard),las=1, cex=1, cex.axis=2)
  
  # Vamos con la pintada
  li <- chi_izq
  ld <- chi_der
  
  cat(li, "\n", ld, "\n")
  x_pintada <- seq(li, ld, by= 0.001)
  #      dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
  #      x_pintada <- x_pintada[dt_x]
  y_pintada <- dchisq(x_pintada, df=input$df )
  
  segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$variable)  
  
  
  curve(dchisq(x, df=input$df ),xlim= valores_chi_extremos, xlab="Valores Chi", ylab="Frecuencia Relativa",
        main="Distribución Chi Cuadrado", 
        ylim=c(0,0.2), 
        add=T)
  
  
  par <- opar
  
  
})
# Fin Gráfico
###############################################



# Texto de EmeRgencia
output$ER1 <- renderText({ 
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  gl <- input$df
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (chi_izq > chi_der) {
    paste("WARNING!!! El valor Chi izq debe ser menor o igual al valor Chi der", sep="")
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################


# Texto de EmeRgencia
output$ER2 <- renderText({
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  gl <- input$df
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (chi_izq < chi_der) {
    if (chi_izq < valores_chi_extremos[1] & chi_der < valores_chi_extremos[1]) {
      paste("Los valores Chi indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
    else if (chi_izq < valores_chi_extremos[1] & chi_der >= valores_chi_extremos[1]) {
      paste("El valor Chi izq queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (chi_izq <= valores_chi_extremos[2] & chi_der > valores_chi_extremos[2]) {
      paste("El valor Chi der queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (chi_izq > valores_chi_extremos[2] & chi_der > valores_chi_extremos[2]) {
      paste("Los valores Chi indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################



# Texto 1
output$text1 <- renderText({ 
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  gl <- input$df
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (chi_izq < chi_der) paste("Tú has seleccionado el rango de valores Chi entre ", 
                           chi_izq_externo(), " y ", chi_der_externo(), 
                           " con ", gl, " grados de libertad.", sep="")
  
  # Si el valor izquierdo y derecho son iguales... Puede ser Correcto
  else if (chi__izq == chi_der) paste("Los valores Chi izquierdo y derecho son iguales.", sep="")
  
  # Si el valor izquierdo es mayor que el derecho... INCORRECTO
  else if (chi__izq > chi_der) paste("Atención: el valor Chi izquierdo no puede ser mayor al Chi derecho", sep="")
})
# Fin Texto 1
##############



# Texto 2
output$text2 <- renderText({ 
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  gl <- input$df
  
  if (chi_izq < chi_der) {
    prob <- probabilidad
    
    paste("El valor de probabilidad obtenido es ", probabilidad, ".",sep="")
  } # Fin if <
  else if (chi_izq == chi_der ) paste("El valor de probabilidad es 0 (cero).",sep="")
})
# Fin Texto 2
##############


# Texto 3
output$text3 <- renderText({ 
  
  
  chi_izq <- chi_izq_interno()
  chi_der <- chi_der_interno()
  decimales <- input$decimales
  probabilidad <- prob_interno()
  gl <- input$df
  
  if (chi_izq < chi_der) {
    
    
    
    p1 <-   paste("Para obtener el valor de probabilidad en un rango realizamos:", sep="")
    p2 <-   paste("$$p(",chi_izq_externo(),"<\\chi^{2}<", chi_der_externo(),")= p(\\chi^{2}<",chi_der_externo(),") - p(\\chi^{2}<",chi_izq_externo(),")$$", sep="")
    

  #  p2 <- pchisq(chi_der, df=gl)  
  #  p1 <- pchisq(chi_izq, df=gl) 
    
        
    aver_prob_der <- pchisq(chi_der, df=gl)
    aver_prob_der <- round(aver_prob_der, decimales)
    
    aver_prob_izq <- pchisq(chi_izq, df=gl)
    aver_prob_izq <- round(aver_prob_izq, decimales)
    
    
    # LA1 <- c("$$p(Z<",z_der,")=", p2, "$$")
    p3 <-   paste("$$p(",chi_izq_externo(),"<\\chi^{2}<",chi_der_externo(),")=", aver_prob_der, " - ", aver_prob_izq, "$$", sep="")
    
    p4 <-   paste("$$p(",chi_izq_externo(),"<\\chi^{2}<",chi_der_externo(),")=", probabilidad, "$$", sep="")
    
    
    frase_total <- paste(p1, p2, p3, p4)
    
  } # Fin if <
  else paste("",sep="")
})
# Fin Texto 3
##############


# 
# 
# 
# # Texto 4
# output$text4 <- renderText({ 
#   if (input$chi_izq < input$chi_der) {
#     paste("p(Chi izq<//chi<Chi der)= p(CHI<Chi der) - p(CHI<Chi izq)", sep="")
#     
#   } # Fin if <
#   else paste("", sep="")
# })
# # Fin Texto 4
# ##############
# 
# 
# # Texto 5
# output$text5 <- renderText({ 
#   if (input$chi_izq < input$chi_der) {
#     
#     p2 <- pchisq(input$chi_der, df=input$df)  
#     p2 <- round(p2, 4)
#     
#     LA1 <- c("1) p(CHI<",input$chi_der,")=", p2)
#     
#     paste(LA1, sep="")
#   } # Fin if <
#   else paste("", sep="")
# })
# # Fin Texto 5
# ##############
# 
# 
# # Texto 6
# output$text6 <- renderText({ 
#   if (input$chi_izq < input$chi_der) {
#     
#     p1 <- pchisq(input$chi_izq, df=input$df) 
#     
#     p1 <- round(p1, 4)
#     
#     LA2 <- c("2) p(CHI<",input$chi_izq,")=", p1)
#     
#     paste(LA2, sep="")
#   } # Fin if <
#   else paste("", sep="")
# })
# # Fin Texto 6
# ##############
# 
# 
# # Texto 7
# output$text7 <- renderText({ 
#   if (input$chi_izq < input$chi_der) {
#     
#     p2 <- pchisq(input$chi_der, df=input$df)  
#     p1 <- pchisq(input$chi_izq, df=input$df) 
#     
#     p1 <- round(p1, 4)
#     p2 <- round(p2, 4)
#     
#     p_result <- p2 - p1
#     p_result <- round(p_result,4)
#     
#     LA3 <- c("3) p(",input$chi_izq,"<CHI<",input$chi_der, ")= ","p(CHI<",input$chi_der,") -","p(CHI<",input$chi_izq,")", "= ", p2, "-", p1, "= ", p_result ,sep="")
#     
#     paste(LA3, sep="")
#   } # Fin if <
#   else paste("", sep="")
# })
# # Fin Texto 7
# ##############
# 
# # Texto 8
# output$ex4 <- renderUI({
#   if (input$chi_izq < input$chi_der) {
#     p2 <- pchisq(input$chi_der, df=input$df)  
#     
#     
#     withMathJax(sprintf(" $$p(\\chi^{2} \\leq %.03f ) = %.04f$$", input$chi_der, p2))
#   } # Fin if
#   else paste("", sep="")
# })
# # Fin Texto 8
# ########################
# 
# 
# # Texto 9
# output$ex5 <- renderUI({
#   if (input$chi_izq < input$chi_der) {
#     # invalidateLater(5000, session)
#     p1 <- pchisq(input$chi_izq, df=input$df) 
#     
#     
#     withMathJax(sprintf(" $$p(\\chi^{2} \\leq %.03f ) = %.04f$$", input$chi_izq, p1))
#   } # Fin if
#   else paste("", sep="")
# })
# # Fin Texto 9
# ########################
# 
# 
# # Texto 10
# output$ex6 <- renderUI({
#   if (input$chi_izq < input$chi_der) {
#     # invalidateLater(5000, session)
#     p2 <- pchisq(input$chi_der, df=input$df)  
#     p1 <- pchisq(input$chi_izq, df=input$df) 
#     
#     
#     withMathJax(sprintf(" $$p(%.03f\\leq    \\chi^{2} \\leq %.03f ) = %.04f - %.04f = %.04f$$", input$chi_izq,input$chi_der, p2, p1, (p2-p1)))
#   } # Fin if
#   else paste("", sep="")
# })
# # Fin Texto 10
# ########################
# 
# 
# # Texto 11
output$ex7 <- renderUI({
  if (input$chi_izq < input$chi_der) {
    # invalidateLater(5000, session)
    p2 <- pchisq(input$chi_der, df=input$df)
    p1 <- pchisq(input$chi_izq, df=input$df)


    withMathJax(sprintf("En el libro de Bioestadística 1, en el capítulo 5 llamado \"Variables Aleatorias I\" página 71 en adelante se desarrolla sobre la Distribución Normal, de allí se obtiene la siguiente ecuación: $$p(\\chi^{2}_i\\leq  \\chi^{2} \\leq \\chi^{2}_d ) = p(\\chi^{2} \\leq \\chi^{2}_d) - p(\\chi^{2} \\leq \\chi^{2}_i )$$"))
  } # Fin if
  else withMathJax(sprintf(""))
})
# # Fin Texto 11
# ########################