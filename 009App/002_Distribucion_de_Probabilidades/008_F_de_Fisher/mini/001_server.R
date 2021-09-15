
# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot

# Reactive expression to compose a data frame containing all of
# the values




f_izq_interno <- reactive({
  
  f_izq <- input$f_izq
})

f_der_interno <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      f_der <- 500 
    } else  f_der <- input$f_der
  }
  
})


f_izq_externo <- reactive({
  
  f_izq <- input$f_izq
  
})


f_der_externo <- reactive({
  
  if(!is.null(input$aver2)) {
    
    if (input$aver2 == TRUE) {
      f_der <- "+Inf" 
    } else  f_der <- input$f_der
  }
  
})



prob_interno <- reactive({
  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  
  
  if (f_izq <= f_der) {
    
    p2 <- pf(f_der , df1=glA, df2=glB)  
    p1 <- pf(f_izq , df1=glA, df2=glB) 
    
    prob <- p2 - p1
    prob <- round(prob, decimales)
    
    prob
    
  }
})  



# Tabla de Valores f y probabilidad

sliderValues <- reactive({
  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  if (f_izq <= f_der) {
    # Compose data frame
    data.frame(
      Descripción = c("F izq", 
                      "F der",
                      "GL Muestra 'A'",
                      "GL Muestra 'B'",
                      "Probabilidad"),
      Valores = as.character(c(f_izq, 
                               f_der_externo(),
                               glA, 
                               glB,
                               probabilidad)), 
      stringsAsFactors=FALSE)
  } # Fin if <
  else {
    # Compose data frame
    data.frame(
      Descripción = c("F izq", 
                      "F der",
                      "GL Muestra 'A'",
                      "GL Muestra 'B'",
                      "Probabilidad"),
      Valores = as.character(c("Valores Incorrectos", 
                               "Valores Incorrectos",
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




# Inicio: Gráfico de la Distribucion F
output$distPlot <- renderPlot({
  ### x    <- faithful[, 2]  # Old Faithful Geyser data
  ### bins <- seq(min(x), max(x), length.out = input$bins + 1)

  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
    
  valores_f_extremos <- c(0,17)
  cantidad <- 400
  
  
  
  x <- seq(valores_f_extremos[1], valores_f_extremos[2], length=cantidad)
  y <- df(x,glA, glB)
  
  # Determino los valores que quiero que salgan en el eje X de la t
  # x_standard <- valores_chi_extremos[1]:valores_chi_extremos[2]
  x_standard <- valores_f_extremos[1]:valores_f_extremos[2]
  
  opar <- par()
  par(mai=c(1,3,1,1), mar=(c(5,9,4,2)+0.1), mgp=c(4,1,0)) 
  
  
  # Generamos la grafica de la distribucion t
  curve(df(x, df1=input$dfA,  df2=input$dfB ),xlim= valores_f_extremos, xlab="Valores F", ylab="Frecuencia Relativa",
        main="Distribución F de Fisher", 
        ylim=c(0,0.7), 
        axes=F, cex.main=2, cex.lab=2)
  
  axis(2,  las=1, cex.axis=2)
  #axis(1, c(x_standard), labels=x_standard ,las=1)
  axis(1, c(x_standard),las=1, cex=1, cex.axis=2)
  
  
  # Coloreamos...
  # Vamos con la pintada
  li <- f_izq
  ld <- f_der
  
  cat(li, "\n", ld, "\n")
  x_pintada <- seq(li, ld, by= 0.001)
  #      dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
  #      x_pintada <- x_pintada[dt_x]
  y_pintada <- df(  x_pintada, df1=input$dfA,  df2=input$dfB )
  
  segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$variable)  
 
  # Remarcamos la grafica
  # Generamos la grafica de la distribucion t
  curve(df(x, df1=input$dfA,  df2=input$dfB ),xlim= valores_f_extremos, xlab="Valores F", ylab="Frecuencia Relativa",
        main="Distribución F de Fisher", 
        ylim=c(0,0.7), 
        add=T)
  
  par <- opar
  
})
# Fin Gráfico
###############################################






# Texto de EmeRgencia
output$ER1 <- renderText({ 
  if (input$f_izq > input$f_der) {
    paste("WARNING!!! El valor F izq debe ser menor o igual al valor F der", sep="")
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################


# Texto de EmeRgencia
output$ER2 <- renderText({ 
  
  valores_f_extremos <- c(0,(round(max(input$f_izq,input$f_der, 10),0)+5))
  
  if (input$f_izq < input$f_der) {
    if (input$f_izq < valores_f_extremos[1] & input$f_der < valores_f_extremos[1]) {
      paste("Los valores F indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
    else if (input$f_izq < valores_f_extremos[1] & input$f_der >= valores_f_extremos[1]) {
      paste("El valor F izq queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (input$f_izq <= valores_f_extremos[2] & input$f_der > valores_f_extremos[2]) {
      paste("El valor F der queda fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin 1er If else 
    else if (input$f_izq > valores_f_extremos[2] & input$f_der > valores_f_extremos[2]) {
      paste("Los valores F indicados quedan fuera del rango del gráfico, pero los cálculos son correctos.", sep="")
    } # Fin if <
  } # Fin if <
  else paste("") 
})
# Fin Texto 3
#####################



# Texto 1
output$text1 <- renderText({ 
  
  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  
  
  # Si el valor izquierdo es menor que el derecho... CORRECTO
  if (f_izq < f_der) paste("Tú has seleccionado el rango de valores F entre ", 
                                 f_izq, " y ", f_der_externo(), 
                                 " con glA:",glA , " y glB:", glB, ".", sep="")
  
  # Si el valor izquierdo y derecho son iguales... Puede ser Correcto
  else if (f_izq == f_der) paste("Los valores F izquierdo y derecho son iguales.", sep="")
  
  # Si el valor izquierdo es mayor que el derecho... INCORRECTO
  else if (f_izq > f_der) paste("El valor F izquierdo, debe ser menor que el valor F derecho.", sep="")
})
# Fin Texto 1
##############



# Texto 2
output$text2 <- renderText({ 
  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  
  if (f_izq < f_der) {
    
    paste("El valor de probabilidad obtenido es ",probabilidad, ".",sep="")
  } # Fin if <
  else if (input$f_izq == input$f_der ) paste("El valor de probabilidad es 0 (cero).",sep="")
})
# Fin Texto 2
##############



# Texto 3
output$text3 <- renderText({ 
  
  
  f_izq <- f_izq_interno()
  f_der <- f_der_interno()
  glA <- input$dfA
  glB <- input$dfB
  decimales <- input$decimales
  probabilidad <- prob_interno()
  
  
  
  if (f_izq < f_der) {
    
    
    
    p1 <-   paste("Para obtener el valor de probabilidad en un rango realizamos:", sep="")
    p2 <-   paste("$$p(",f_izq_externo(),"<F<", f_der_externo(),")= p(F<",f_der_externo(),") - p(F<",f_izq_externo(),")$$", sep="")
    
    
    #  p2 <- pchisq(f_der, df=gl)  
    #  p1 <- pchisq(f_izq, df=gl) 
    

      
    aver_prob_der <- pf(f_der , df1=glA, df2=glB)
    aver_prob_der <- round(aver_prob_der, decimales)
    
    aver_prob_izq <- pf(f_izq , df1=glA, df2=glB) 
    aver_prob_izq <- round(aver_prob_izq, decimales)
    
    
    # LA1 <- c("$$p(Z<",z_der,")=", p2, "$$")
    p3 <-   paste("$$p(",f_izq_externo(),"<F<",f_der_externo(),")=", aver_prob_der, " - ", aver_prob_izq, "$$", sep="")
    
    p4 <-   paste("$$p(",f_izq_externo(),"<F<",f_der_externo(),")=", probabilidad, "$$", sep="")
    
    
    frase_total <- paste(p1, p2, p3, p4)
    
  } # Fin if <
  else paste("",sep="")
})
# Fin Texto 3
##############


# Texto 4
output$text4 <- renderText({ 
  if (input$f_izq < input$f_der) {
    paste("p(F_i<//F<F_d)= p(F< F_d) - p(F<F_i)", sep="")
    
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 4
##############


# Texto 5
output$text5 <- renderText({ 
  if (input$f_izq < input$f_der) {
    
    p2 <- pf(input$f_der , df1=input$dfA, df2=input$dfB )  
    p2 <- round(p2, 4)
    
    LA1 <- c("1) p(F<",input$f_der,")=", p2)
    
    paste(LA1, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 5
##############


# Texto 6
output$text6 <- renderText({ 
  if (input$f_izq < input$f_der) {
    
    p1 <- pf(input$f_izq , df1=input$dfA, df2=input$dfB ) 
    
    p1 <- round(p1, 4)
    
    LA2 <- c("2) p(F<",input$f_izq,")=", p1)
    
    paste(LA2, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 6
##############


# Texto 7
output$text7 <- renderText({ 
  if (input$f_izq < input$f_der) {
    
    p2 <- pf(input$f_der , df1=input$dfA, df2=input$dfB )  
    p1 <- pf(input$f_izq , df1=input$dfA, df2=input$dfB ) 
    
    p1 <- round(p1, 4)
    p2 <- round(p2, 4)
    
    p_result <- p2 - p1
    p_result <- round(p_result,4)
    
    LA3 <- c("3) p(",input$f_izq,"<CHI<",input$f_der, ")= ","p(CHI<",input$f_der,") -","p(CHI<",input$f_izq,")", "= ", p2, "-", p1, "= ", p_result ,sep="")
    
    paste(LA3, sep="")
  } # Fin if <
  else paste("", sep="")
})
# Fin Texto 7
##############

# Texto 8
output$ex4 <- renderUI({
  if (input$f_izq < input$f_der) {
    p2 <- pf(input$f_der , df1=input$dfA, df2=input$dfB )  
    
    
    withMathJax(sprintf(" $$p(F \\leq %.03f ) = %.04f$$", input$f_der, p2))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 8
########################


# Texto 9
output$ex5 <- renderUI({
  if (input$f_izq < input$f_der) {
    # invalidateLater(5000, session)
    p1 <- pf(input$f_izq , df1=input$dfA, df2=input$dfB ) 
    
    
    withMathJax(sprintf(" $$p(F \\leq %.03f ) = %.04f$$", input$f_izq, p1))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 9
########################


# Texto 10
output$ex6 <- renderUI({
  if (input$f_izq < input$f_der) {
    # invalidateLater(5000, session)
    p2 <- pf(input$f_der , df1=input$dfA, df2=input$dfB )  
    p1 <- pf(input$f_izq , df1=input$dfA, df2=input$dfB ) 
    
    
    withMathJax(sprintf(" $$p(%.03f\\leq    F \\leq %.03f ) = %.04f - %.04f = %.04f$$", input$f_izq,input$f_der, p2, p1, (p2-p1)))
  } # Fin if
  else paste("", sep="")
})
# Fin Texto 10
########################


# Texto 11
output$ex7 <- renderUI({
  if (input$f_izq < input$f_der) {
    # invalidateLater(5000, session)
    p2 <- pf(input$f_der , df1=input$dfA, df2=input$dfB )  
    p1 <- pf(input$f_izq , df1=input$dfA, df2=input$dfB ) 
    
    
  #  withMathJax(sprintf("En el libro de Bioestadística 1, en el capítulo 6 llamado \"Variables Aleatorias II\" página 80 en adelante se desarrolla sobre la Distribución F, de allí se obtiene la siguiente ecuación: $$p(F_i\\leq  F \\leq F_d ) = p(F \\leq F_d) - p(F \\leq F_i )$$"))
    withMathJax(sprintf("$$p(F_i\\leq  F \\leq F_d ) = p(F \\leq F_d) - p(F \\leq F_i )$$"))
    
     } # Fin if
  else withMathJax(sprintf(""))
})
# Fin Texto 11
########################


