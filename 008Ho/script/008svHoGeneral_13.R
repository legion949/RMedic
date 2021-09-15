


# # # # #
# 2C - 13 - Regresion Logistica Simple

BASE_RegLogSV3 <- reactive({
  
  if (paso_BASE(BASE_goku())) {
    MINI <- BASE_goku()
    
    
    ver1 <- is.numeric(as.vector(as.matrix(MINI[,1])))
    ver2 <- is.numeric(as.vector(as.matrix(MINI[,2])))
    niveles1 <- levels(as.factor(as.character(as.vector(as.matrix(MINI[,1])))))
    niveles2 <- levels(as.factor(as.character(as.vector(as.matrix(MINI[,2])))))
    
    cantidad1 <- length(niveles1)
    cantidad2 <- length(niveles2)
    
    # Si ambas variables son numericas...
    
    if (ver1 == TRUE && ver2 == TRUE) {
      
      # # Si ambas variables tienen solo dos valores...  
      if (cantidad1 == 2 && cantidad2 == 2) {
        
        #      MINI[,1] <- as.factor(as.character(MINI[,1]))
        #      MINI[,2] <- as.factor(as.character(MINI[,2]))
        
      }
      
      # Si la primera columna tiene dos valores... y la 2da tiene mas
      if (cantidad1 == 2 && cantidad2 > 2) {
        
        # Cambiamos el orden de las variables...
        # Asi primero queda la VR y 2do la variable de dos niveles
        MINI <- MINI[,c(2,1)]
        
        
        # Hacemos factor a la 2da columna
        #     MINI[,2] <- as.factor(as.character(MINI[,2]))
        
      }
      
      # Si la 1ra columna tiene mas de dos valores... y la 2ra tiene dos
      if (cantidad1 > 2 && cantidad2 == 2) {
        
        # Ya tenemos 1ro a VR y 2do a la categorica.
        # No hace falta cambiar el orden...
        
        # Hacemos factor a la 2da columna
        #      MINI[,2] <- as.factor(as.character(MINI[,2]))
        
      }
      
    } # Fin si todo esta bien  
    
    MINI
    
  }
  
  
  
})




# Detalle Eje (X)
output$MODdetalle1_ho_2q_13 <- renderUI({ 
  
 # if(paso_BASE(BASE_RegLogSV3()) && ncol(BASE_RegLogSV3()) == 2) { 

    niveles <- levels(as.factor(as.character(as.vector(as.matrix(BASE_RegLogSV3()[,1])))))
    ver_x <- is.numeric(as.vector(as.matrix(BASE_RegLogSV3()[,1])))
    cantidad_x <- length(niveles)
    
    if (ver_x == TRUE && cantidad_x == 2) {
      selectInput("detalle1_ho_2q_13", "Referencia '0' (X)", 
                  choices = levels(as.factor(BASE_RegLogSV3()[,1]))
      )
      
 #   } else return(NULL)
  } else return(NULL)
})


# Detalle Eje (Y)
output$MODdetalle2_ho_2q_13 <- renderUI({ 
  
 # if(paso_BASE(BASE_RegLogSV3()) && ncol(BASE_RegLogSV3()) == 2) {
    
    selectInput("detalle2_ho_2q_13", "Referencia '0' (Y)", 
                choices = levels(as.factor(BASE_RegLogSV3()[,2])))
    
    
 # } else return(NULL)
})


output$ho_2c_13 <- renderUI({
  
 # if(paso_BASE(BASE_RegLogSV3()) && ncol(BASE_RegLogSV3()) == 2) {
    
    div(
     uiOutput("MODdetalle1_ho_2q_13"),
     uiOutput("MODdetalle2_ho_2q_13")
      
    )
    
 # } else return(NULL)
  
})


# Reg Log
Test13_2C_RegLog <- reactive({
  
 # if(paso_BASE(BASE_RegLogSV3()) && ncol(BASE_RegLogSV3()) == 2 && sum(tipo_var_goku() == "Numérica") == 2) {
#      if(!is.null(input$detalle2_ho_2q_13) && sum(levels(as.factor(BASE_RegLogSV3()[,2])) == input$detalle2_ho_2q_13) == 1) {
        if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
              
              
              DATOS <- BASE_RegLogSV3()  
              
              
          
              DATOS[,2] <- as.factor(as.character(DATOS[,2]))
              DECIMALES <- decimales_goku()
              ALFA <- input$alfa_ho
              
              
              
              REF_X <- input$detalle1_ho_2q_13
              REF_Y <- input$detalle2_ho_2q_13
              
              ANALISIS <- RegLogS_MASTER( input_datos = DATOS, 
                                          input_decimales = DECIMALES, 
                                          input_alfa = ALFA,
                                          input_contingencia = FALSE,
                                          input_cero_ref_x= REF_X,
                                          input_cero_ref_y= REF_Y)
              
              
              
              ANALISIS
              
 #       } else return(NULL)
#      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})





# Tabla Referencia de Categorias para Reg Log
observe( output$tabla01_ho_2c_13 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if(!is.null(Test13_2C_RegLog())) {
    
    TABLA <- Test13_2C_RegLog()$SALIDA_ARMADA$TABLA_REFERENCIA
    if (!is.null(TABLA)) {
      colnames(TABLA)[2] <- "Referencia Estándard"
      TABLA
    } else return(NULL)
  } else return(NULL)
}))


# Tabla de Regresion RegLog
observe( output$tabla02_ho_2c_13 <- renderTable(rownames = T, digits=decimales_goku(), {
  
  if(!is.null(Test13_2C_RegLog())) {
    Test13_2C_RegLog()$SALIDA_ARMADA$TABLA_REGRESION
  } else return(NULL)
}))


# Tabla de Regresion RegLog
observe( output$tabla03_ho_2c_13 <- renderTable(rownames = T, digits=decimales_goku(), {
  
  if(!is.null(Test13_2C_RegLog())) {
    Test13_2C_RegLog()$SALIDA_ARMADA$RESUMEN
  } else return(NULL)
}))



# Odd Ratio de RegLog
observe( output$oddratio_ho_2c_13 <- renderText({
  
  if(!is.null(Test13_2C_RegLog())) {
    pendiente <-   as.numeric(as.character(Test13_2C_RegLog()$SALIDA_ARMADA$TABLA_REGRESION[2,1]))
    calculo <- round2(exp(pendiente), decimales_goku())
    
    texto <- paste0("Odd Ratio = exp(", pendiente, ") = ", calculo)
    texto
  } else return(NULL)
}))



# Frase1 pendiente RegLog
output$frase01_pendiente_ho_2c_13 <- renderUI({
  if(!is.null(Test13_2C_RegLog())) {
    HTML(Test13_2C_RegLog()$SALIDA_ARMADA$FRASE_PENDIENTE)
  } else return(NULL)
})


# Frase2 ordenada RegLog
output$frase02_ordenada_ho_2c_13 <- renderUI({
  if(!is.null(Test13_2C_RegLog())) {
    HTML(Test13_2C_RegLog()$SALIDA_ARMADA$FRASE_ORDENADA)
  } else return(NULL)
})


# Grafico RegLog
output$plot_ho_2c_13 <- renderPlot({ 
  
  if(!is.null(Test13_2C_RegLog())) {
    
    MINI <- BASE_goku()  
    
    COL_X <- colnames(MINI)[1]
    COL_Y <- colnames(MINI)[2]
    
    
    
    X_FINAL <- Test13_2C_RegLog()$GRAFICO$X_FINAL
    Y_FINAL <- Test13_2C_RegLog()$GRAFICO$Y_FINAL
    
    MyData_X <- Test13_2C_RegLog()$GRAFICO$MyData_X
    PREDICCION_Y <- Test13_2C_RegLog()$GRAFICO$PREDICCION_Y
    
    
    plot(X_FINAL, Y_FINAL, 
         #main=input$columna_graf_torta,
         col= "red",
         xlab= COL_X,   
         ylab= COL_Y)
    #     xlim=c(min(X_FINAL), max(X_FINAL)))
    
    lines(MyData_X, PREDICCION_Y)     
    #       
    
    # MINI <- c(1:10)
    
  } else return(NULL)
  
}, width = 400, height = 400)





Opc_ho_2c_13 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Regresión Logística Simple"),
 
        h3("Selección de Conversión"),
            uiOutput("ho_2c_13"), br(),
      
        h3("Tabla de Referencias"),
           tableOutput("tabla01_ho_2c_13"), br(),
        
         h3("Gráfico"),
         plotOutput("plot_ho_2c_13"), br(),
           
         h3("Tabla de Regresión Logística Simple"),
         tableOutput("tabla02_ho_2c_13"), br(),
           
         h3("Odd Ratio"),
         textOutput("oddratio_ho_2c_13"), br(),
  
         h3("Pendiente"),
         uiOutput("frase01_pendiente_ho_2c_13"), br(),
         h3("Ordenada"),
         uiOutput("frase02_ordenada_ho_2c_13")
        
    )
    
    
    
  } else return(NULL)
  
})

