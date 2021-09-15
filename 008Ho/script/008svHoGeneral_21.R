


# # # # #
# QC - 21 - RegLog



output$ho_qc_21 <- renderUI({
  
  selectInput("detalle2_ho_2q_21", "Referencia '0' (Y)", 
              choices = levels(as.factor(BASE_goku()[,2])))
  
})





# Reg Log
Test21_qc_RegLog <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
      
      
      
      DATOS <- BASE_goku()
      
      
      
      DATOS[,2] <- as.factor(as.character(DATOS[,2]))
      DECIMALES <- decimales_goku()
      ALFA <- input$alfa_ho
      
      
      
      REF_X <- NULL
      REF_Y <- input$detalle2_ho_2q_21
      
      ANALISIS <- RegLogS_MASTER( input_datos = DATOS, 
                                  input_decimales = DECIMALES, 
                                  input_alfa = ALFA,
                                  input_contingencia = FALSE,
                                  input_cero_ref_x= REF_X,
                                  input_cero_ref_y= REF_Y)
      
      
      
      ANALISIS
      
    
            } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})








# Tabla Referencia de Categorias para Reg Log
observe( output$tabla01_ho_qc_21 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if(!is.null(Test21_qc_RegLog())) {
    
    TABLA <- Test21_qc_RegLog()$SALIDA_ARMADA$TABLA_REFERENCIA
    if (!is.null(TABLA)) {
      colnames(TABLA)[2] <- "Referencia Estándard"
      TABLA
    } else return(NULL)
  } else return(NULL)
}))


# Tabla de Regresion RegLog
observe( output$tabla02_ho_qc_21 <- renderTable(rownames = TRUE, digits=decimales_goku(), {
  
  if(!is.null(Test21_qc_RegLog())) {
    Test21_qc_RegLog()$SALIDA_ARMADA$TABLA_REGRESION
  } else return(NULL)
}))


# Tabla de Regresion RegLog
observe( output$tabla03_ho_qc_21 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if(!is.null(Test21_qc_RegLog())) {
    Test21_qc_RegLog()$SALIDA_ARMADA$RESUMEN
  } else return(NULL)
}))



# Odd Ratio de RegLog
observe( output$oddratio_ho_qc_21 <- renderText({
  
  if(!is.null(Test21_qc_RegLog())) {
    pendiente <-   as.numeric(as.character(Test21_qc_RegLog()$SALIDA_ARMADA$TABLA_REGRESION[2,1]))
    calculo <- round2(exp(pendiente), decimales_goku())
    
    texto <- paste0("Odd Ratio = exp(", pendiente, ") = ", calculo)
    texto
  } else return(NULL)
}))



# Frase1 pendiente RegLog
output$frase01_pendiente_ho_qc_21 <- renderUI({
  if(!is.null(Test21_qc_RegLog())) {
    HTML(Test21_qc_RegLog()$SALIDA_ARMADA$FRASE_PENDIENTE)
  } else return(NULL)
})


# Frase2 ordenada RegLog
output$frase02_ordenada_ho_qc_21 <- renderUI({
  if(!is.null(Test21_qc_RegLog())) {
    HTML(Test21_qc_RegLog()$SALIDA_ARMADA$FRASE_ORDENADA)
  } else return(NULL)
})


# Grafico RegLog
output$plot_ho_qc_21 <- renderPlot({ 
  
  if(!is.null(Test21_qc_RegLog())) {
    
    MINI <- BASE_goku()  
    
    COL_X <- colnames(MINI)[1]
    COL_Y <- colnames(MINI)[2]
    
    
    
    X_FINAL <- Test21_qc_RegLog()$GRAFICO$X_FINAL
    Y_FINAL <- Test21_qc_RegLog()$GRAFICO$Y_FINAL
    
    MyData_X <- Test21_qc_RegLog()$GRAFICO$MyData_X
    PREDICCION_Y <- Test21_qc_RegLog()$GRAFICO$PREDICCION_Y
    
    
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





Opc_ho_qc_21 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Regresión Logística Simple"),
  
        h3("Selección de Ordenamiento"),
           uiOutput("ho_qc_21"), br(),
      
      h3("Tabla de Conversión"),
           tableOutput("tabla01_ho_qc_21"), br(),
    
      
       h3("Gráfico"),
       plotOutput("plot_ho_qc_21", width = "400px", height = "400px"), br(),
      
                h3("Tabla de Regresión Logística Simple"),
               tableOutput("tabla02_ho_qc_21"),
               textOutput("oddratio_ho_qc_21"),
               #  h3("Análisis de Regresión Logística Simple"),
               # tableOutput("reglogs_RESUMEN")
        
      h3("Pendiente"),
      uiOutput("frase01_pendiente_ho_qc_21"), br(), 
      h3("Ordenada"),
      uiOutput("frase02_ordenada_ho_qc_21"), br()
      
    )
    
    
    
  } else return(NULL)
  
})


