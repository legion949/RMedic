


# # # # #
# 2Q - 06 - Regresion Logistica

output$ho_2q_06 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    div(
      selectInput("detalle1_ho_2q_06", "Referencia '0' (X)", 
                  choices = levels(as.factor(BASE_goku()[,1]))),
      selectInput("detalle2_ho_2q_06", "Referencia '0' (Y)", 
                  choices = levels(as.factor(BASE_goku()[,2])))
      
      
    )
    
  } else return(NULL)
  
})


# Reg Log para 2 variables Categoricas
Test06_2Q_RegLog <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Categórica") == 2) {
    if(!is.null(input$detalle1_ho_2q_06) && sum(levels(as.factor(BASE_goku()[,1])) == input$detalle1_ho_2q_06) == 1) {
      if(!is.null(input$detalle2_ho_2q_06) && sum(levels(as.factor(BASE_goku()[,2])) == input$detalle2_ho_2q_06) == 1) {
        if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            RegLogS_MASTER( input_datos = BASE_goku(), 
                            input_decimales = decimales_goku(), 
                            input_alfa = input$alfa_ho,
                            input_contingencia = FALSE,
                            input_cero_ref_x= input$detalle1_ho_2q_06,
                            input_cero_ref_y= input$detalle2_ho_2q_06)
              
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})
#######################################################





# Tabla Referencia de Categorias para Reg Log
observe( output$tabla01_ho_2q_06 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if(!is.null(Test06_2Q_RegLog())) {
    
    TABLA <- Test06_2Q_RegLog()$SALIDA_ARMADA$TABLA_REFERENCIA
    if (!is.null(TABLA)) {
      colnames(TABLA)[2] <- "Referencia Estándard"
      TABLA
    } else return(NULL)
  } else return(NULL)
}))



# Tabla de Regresion RegLog
observe( output$tabla02_ho_2q_06 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if(!is.null(Test06_2Q_RegLog())) {
      Test06_2Q_RegLog()$SALIDA_ARMADA$TABLA_REGRESION
  } else return(NULL)
}))


# Odd Ratio de RegLog
observe( output$oddratio_ho_2q_06 <- renderText({
  
  if(!is.null(Test06_2Q_RegLog())) {
    pendiente <-   as.numeric(as.character(Test06_2Q_RegLog()$SALIDA_ARMADA$TABLA_REGRESION[2,1]))
    calculo <- round2(exp(pendiente), decimales_goku())
    
    texto <- paste0("Odd Ratio = exp(", pendiente, ") = ", calculo)
    texto
  } else return(NULL)
}))



# Frase1 pendiente RegLog
output$frase01_pendiente_ho_2q_06 <- renderUI({
  if(!is.null(Test06_2Q_RegLog())) {
    HTML(Test06_2Q_RegLog()$SALIDA_ARMADA$FRASE_PENDIENTE)
  } else return(NULL)
})


# Frase2 ordenada RegLog
output$frase02_ordenada_ho_2q_06 <- renderUI({
  if(!is.null(Test06_2Q_RegLog())) {
    HTML(Test06_2Q_RegLog()$SALIDA_ARMADA$FRASE_ORDENADA)
  } else return(NULL)
})



# Grafico RegLog
output$plot_ho_2q_06 <- renderPlot({ 
  
  if(!is.null(Test06_2Q_RegLog())) {
      
      MINI <- BASE_goku()  
      
      COL_X <- colnames(MINI)[1]
      COL_Y <- colnames(MINI)[2]
      
      
      
      X_FINAL <- Test06_2Q_RegLog()$GRAFICO$X_FINAL
      Y_FINAL <- Test06_2Q_RegLog()$GRAFICO$Y_FINAL
      
      MyData_X <- Test06_2Q_RegLog()$GRAFICO$MyData_X
      PREDICCION_Y <- Test06_2Q_RegLog()$GRAFICO$PREDICCION_Y
      
      
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





Opc_ho_2q_06 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
        h3("Referencias de Conversión"), br(),
               uiOutput("ho_2q_06"), br(),
        
        h3("Tabla de Referencias"), br(),
               tableOutput("tabla01_ho_2q_06"), br(),
        
               h3("Gráfico"), br(),
               plotOutput("plot_ho_2q_06", width = "400px", height = "400px"), br(),
        
               h3("Tabla de Regresión Logística Simple"), br(),
               tableOutput("tabla02_ho_2q_06"), br(),
        
               h3("Odd Ratio"), br(),
               textOutput("oddratio_ho_2q_06"), br(),
        
               h3("Pendiente"), br(),
               uiOutput("frase01_pendiente_ho_2q_06"), br(),
               
               h3("Ordenada"), br(),
               uiOutput("frase02_ordenada_ho_2q_06")
    )
    
    
    
  } else return(NULL)
  
})



