# Expression that generates a histogram. The expression is
# wrapped in a call to renderPlot to indicate that:
#
#  1) It is "reactive" and therefore should be automatically
#     re-executed when inputs change
#  2) Its output type is a plot


output$gmenu3 <- renderUI({
  
    if (!is.null(input$df)) if (!is.null(input$forma)) if (!is.null(input$opciones)){
    
    
    
    
    df <- input$df
    
    
    probabilidad_base <- 0.95
    alfa <- 1 - probabilidad_base
    
    
    t_izq1 <- qt((alfa/2), df)
    t_izq1<- round(t_izq1, 2)
    t_der1 <- qt((1- (alfa/2)), df)
    t_der1 <- round(t_der1, 2)
    
    t_izq2 <- t_izq1
    t_der2 <- t_der1
    
    t_der1 <- t_izq1
        
    valor_variable_izq <- 0
    valor_variable_der <- 0
    # valor_variable_izq <- t_izq1*(desvio/sqrt(df)) + media
    # valor_variable_izq <- round(valor_variable_izq, 2)
    # 
    # valor_variable_der <- t_der*(desvio/sqrt(df)) + media
    # valor_variable_der <- round(valor_variable_der, 2)
    
    
    conditionalPanel("1 == 1",
    conditionalPanel("input.opciones == 'opc1'",
                     conditionalPanel('input.forma == "menor"',
                                      numericInput("var1", "Valor de la Variable:", min=-1e20,  max=1e20, step= 0.01, value= valor_variable_izq)),
                     
                     conditionalPanel('input.forma == "mayor"',
                                      numericInput("var2", "Valor de la Variable:", min=-1e20,  max=1e20, step= 0.01, value= valor_variable_izq)),
                     
                     conditionalPanel('input.forma == "entre"',
                                      numericInput("var3", "Valor Izquierdo", min=-1e20,  max=1e20, step= 0.01, value= valor_variable_izq),
                                      numericInput("var4", "Valor Derecho:", min=-1e20,  max=1e20, step= 0.01, value= valor_variable_der))  
    ),
    conditionalPanel("input.opciones == 'opc2'",
                     conditionalPanel('input.forma == "menor"',
                                      numericInput("t_der1", "Valor t:", min=-1e20,  max=1e20, step= 0.01, value= t_izq1)),
                     
                     conditionalPanel('input.forma == "mayor"',
                                      numericInput("t_izq1", "Valor t:", min=-1e20,  max=1e20, step= 0.01, value= t_der1)),
    
                     conditionalPanel('input.forma == "entre"',
                                      numericInput("t_izq2", "Valor t izquierdo:", min=-1e20,  max=1e20, step= 0.01, value= t_izq2),
                                      numericInput("t_der2", "Valor t derecho:", min=-1e20,  max=1e20, step= 0.01, value= t_der2))
                     
                     
    ),
    conditionalPanel("input.opciones == 'opc3'",
                      numericInput("probabilidad", "Probabilidad (Un valor entre 0 y 1)", min=0,  max=1, step= 0.01, value= probabilidad_base))  
    )
  } else return(NULL)
})



# Marco para todos los graficos...
valores_t_extremos <- c(-4,4)
cantidad <- 400
cantidad_desvio <- max(valores_t_extremos)
#######################################




# Grafico 2
output$distPlot222 <- renderPlot({
  ### x    <- faithful[, 2]  # Old Faithful Geyser data
  ### bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
     if (!is.null(input$df)) if (!is.null(input$forma)) {
    
  
  
  # Detalles varios
  
  
  #  cantidad <- 400
  
  gl <- input$df
  # cantidad <- 400
  
  # x <- seq(-4, 4, length=cantidad)
  x <- seq(valores_t_extremos[1], valores_t_extremos[2], length=cantidad)
  y <- dt(x, df=gl)
  
  # Determino los valores que quiero que salgan en el eje X de la t
  x_standard <- valores_t_extremos[1]:valores_t_extremos[2]
  
  
  # Generamos la grafica de la distribucion t
  curve(dt(x, df= gl),xlim= valores_t_extremos, xlab="Valores de t", ylab="Frecuencia Relativa",
        main="Distribución t", ylim=c(0,0.5), axes= F)
  axis(2,  las=1)
  axis(1, c(x_standard), labels=x_standard ,las=1)
  
  
  ti <- -10
  td <- 10
  
  li <-  ti
  ld <-  td  

  
  candado_interno <- TRUE
  
  
  if (input$opciones == 'opc2'){
                   
  if (input$forma == "menor") if (!is.null(input$t_der1)) {
    ld <- input$t_der1
    candado_interno <- FALSE
  }
  
  if (input$forma == "mayor") if (!is.null(input$t_izq1)) {
    li <- input$t_izq1
    candado_interno <- FALSE
  }
  
  if (input$forma == "entre") {
    if (!is.null(input$t_izq2)) if (!is.null(input$t_der2)) {
      candado_interno <- FALSE
      li <- input$t_izq2
      ld <- input$t_der2
    }
  }
  }
  

  if (input$opciones == 'opc3') if (!is.null(input$probabilidad)){
    
    probabilidad <- input$probabilidad
    
    if (input$forma == "menor")  {
      ld <- qt(probabilidad, gl)
      candado_interno <- FALSE
    }
    
    if (input$forma == "mayor") {
      cambio <- 1 - probabilidad
      li <- qt(cambio, gl)
      candado_interno <- FALSE
    }
    
    if (input$forma == "entre") {
      
      alfita <- 1- probabilidad
      alfita2 <- alfita/2
      
        candado_interno <- FALSE
        li <- qt(alfita2, gl)
        ld <- qt((1-alfita2), gl)
      
    }
  }
  
  
    if (candado_interno == FALSE) {  

     
      cat(li, "\n", ld, "\n")
      x_pintada <- seq(li, ld, by= 0.001)
      #      dt_x <- sum(as.numeric(x_pintada >= valores_z_extremos[1]) + as.numeric(x_pintada <= valores_z_extremos[2])) == 2 
      #      x_pintada <- x_pintada[dt_x]
      y_pintada <- dt(x_pintada, df= gl)
      
      segments(x0= x_pintada, y0= 0, x1= x_pintada, y1= y_pintada, col= input$variable)    

    
      
      # Generamos la grafica de la distribucion t de nuevo, para que pinte el borde negro otra vez
      curve(dt(x, df= gl),xlim= valores_t_extremos, xlab="Valores de t", ylab="Frecuencia Relativa",
            main="Distribución t", ylim=c(0,0.5), add=T)
    #  axis(2,  las=1)
    #  axis(1, c(x_standard), labels=x_standard ,las=1)
      
    
  }
  
  }
  
})




# Tabla de Valores z y probabilidad

sliderValues1 <- reactive({
  
  aver <- c(input$mu, sqrt(input$sigma_cuad), input$sigma_cuad)
  aver <- round(aver, 2)
  
  dim(aver) <- c(1, length(aver))
  colnames(aver) <- c("Mu", "Desvío", "Varianza")
  
  
  aver
  
  #######################################################
}) 

output$values111 <- renderTable({
  sliderValues1()
})



sliderValues2 <- reactive({
  
  if (!is.null(input$decimales)) {
    decimales <- input$decimales
  nombres1 <- c("Valor t menores que...", "Probabilidad", "Porcentaje")
  nombres2 <- c("Valor t mayores que...", "Probabilidad", "Porcentaje")
  nombres3 <- c("t izquierdo", "t derecho", "Probabilidad", "Porcentaje")
  
  candado222 <- TRUE
  
  if (input$opciones == 'opc2') if (!is.null(input$df)){
    
    if (input$forma == "menor") if (!is.null(input$t_der1)) {
   
      probabilidad <- pt(input$t_der1,input$df)
      probabilidad <- round(probabilidad, decimales)
        porcentaje <- probabilidad*100
        porcentaje <- paste0(porcentaje, "%")
      
      tabla <- as.data.frame(matrix(NA, 1, length(nombres1)))
      colnames(tabla) <- nombres1
      tabla[1, ] <- c(round(input$t_der1, decimales), probabilidad, porcentaje)     
      candado222 <- FALSE
    }
  
  
  
  if (input$forma == "mayor") if (!is.null(input$t_izq1)) {
    
    probabilidad <- 1 - pt(input$t_izq1,input$df)
    probabilidad <- round(probabilidad, decimales)
    porcentaje <- probabilidad*100
    porcentaje <- paste0(porcentaje, "%")
    
    tabla <- as.data.frame(matrix(NA, 1, length(nombres2)))
    colnames(tabla) <- nombres2
    tabla[1, ] <- c(round(input$t_izq1, decimales), probabilidad, porcentaje)          
    candado222 <- FALSE
  }
    
    
    
    if (input$forma == "entre") if (!is.null(input$t_izq2)) if (!is.null(input$t_der2)) {
      
      
      
      tabla <- as.data.frame(matrix(NA, 1, length(nombres3)))
      colnames(tabla) <- nombres3
      miniprob <- pt(input$t_der2,input$df) - pt(input$t_izq2,input$df)
      miniprob <- round(miniprob, decimales)
      porcentaje <- miniprob*100
      porcentaje <- paste0(porcentaje, "%")
      
      tabla[1, ] <- c(round(input$t_izq2, decimales),  input$t_der2, miniprob, porcentaje)           
      candado222 <- FALSE
    }
    
   if (candado222 == FALSE) tabla
  }
}
  
  
  
  
  
  
  
  #######################################################
}) 

sliderValues3 <- reactive({
  
  if (!is.null(input$decimales)) {
  
    decimales <- input$decimales  
  nombres1 <- c("Valor t menores que...", "Probabilidad", "Porcentaje")
  nombres2 <- c("Valor t mayores que...", "Probabilidad", "Porcentaje")
  nombres3 <- c("t izquierdo", "t derecho", "Probabilidad", "Porcentaje")
  
  candado222 <- TRUE
  
if (input$opciones == 'opc3') if (!is.null(input$df)) if (!is.null(input$probabilidad)){
  
  probabilidad <- input$probabilidad
  probabilidad <- round(probabilidad, decimales)
  porcentaje <- probabilidad*100
  porcentaje <- paste0(porcentaje, "%")

  gl <- input$df
  
  t1 <- qt(probabilidad, gl)
  t1 <- round(t1, decimales)
  t2 <- t1
  
  miniprob <- (1- probabilidad)/2
  t3 <- qt(miniprob, gl)
  t3 <- round(t3, decimales)
  t4 <- qt((1-miniprob), gl)
  t4 <- round(t4, decimales)
  
  if (input$forma == "menor")  {
    
    tabla <- as.data.frame(matrix(NA, 1, length(nombres1)))
    colnames(tabla) <- nombres1
    tabla[1, ] <- c(t1, probabilidad, as.character(porcentaje))     
    candado222 <- FALSE
  }
  
  
  
  if (input$forma == "mayor")  {
    
    tabla <- as.data.frame(matrix(NA, 1, length(nombres2)))
    colnames(tabla) <- nombres2
    tabla[1, ] <- c(t2, probabilidad, as.character(porcentaje))          
    candado222 <- FALSE
  }
  
  
  
  if (input$forma == "entre")  {
    
    tabla <- as.data.frame(matrix(NA, 1, length(nombres3)))
    colnames(tabla) <- nombres3
    tabla[1, ] <- c(t3, t4, probabilidad, as.character(porcentaje))           
    candado222 <- FALSE
  }
  
  if (candado222 == FALSE) tabla
}

  
  }
})


output$values222 <- renderTable(digits = 4,{

  if (!is.null(input$opciones)) {
  
   if (input$opciones == "opc2") sliderValues2()
    
  #  if (input$opciones == "opc3") sliderValues3()
    
  }
})


output$values333 <- renderTable(digits = 4,{
  
  if (!is.null(input$opciones)) {
    
  #  if (input$opciones == "opc2") sliderValues2()
    
      if (input$opciones == "opc3") sliderValues3()
    
  }
})




# Fin Tabla
##################################################  


output$text <- renderText({
  
  
  if (!is.null(input$forma))  {
    
    
    # Detalles varios
    
    
    #  cantidad <- 400
    
    
    
    li <- -10000000
    ld <- 10000000
    
    if (input$forma == "menor") ld <- input$var1
    
    if (input$forma == "mayor") li <- input$var2
    
    if (input$forma == "entre") {
      li <- input$var3
      ld <- input$var4
    }
    
    li <- as.numeric(as.character(li))
    ld <- as.numeric(as.character(ld))
    
    prob2 <- pnorm(ld, media, desvio)
    prob1 <- pnorm(li, media, desvio)
    prob <- prob2 - prob1
    prob <- round(prob, 2)
    
    if (input$forma == "menor") frase <- paste0("La probabilidad de pacientes con valores menores a ", input$var1, " es de <b>", prob, "</b>.")
    
    if (input$forma == "mayor") frase <- paste0("La probabilidad de pacientes con valores mayores a ", input$var2, " es de <b>", prob, "</b>.")
  
    if (input$forma == "entre") frase <- paste0("La probabilidad de pacientes con valores entre ", input$var3, " y ", input$var4, " es de <b>", prob, "</b>.")
    
    frase  
      
  } else return(NULL)
  
})



# Fin Texto 10
########################
