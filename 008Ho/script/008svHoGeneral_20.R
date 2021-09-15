


# # # # #
# QC - 20 - KW




# Test de Diferencia de Proporciones
Test20_QC_KW  <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        
        
        DATOS <- BASE_goku()
        
        
        DATOS[,2] <- as.factor(as.character(DATOS[,2]))
        DECIMALES <- decimales_goku()
        ALFA <- input$alfa_ho
        
        
        
        ANALISIS <- KW_MASTER( input_datos = DATOS,
                               input_decimales = DECIMALES,
                               input_alfa = ALFA)
        
        
        
        ANALISIS
        
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})



# Salida resumen de KW
observe( output$tabla01_ho_qc_20 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test20_QC_KW())){
    Test20_QC_KW()$SALIDA_ARMADA$RESUMEN_KW
  } else return(NULL)
}))



# Salida TABLA DUNN
observe( output$tabla02_ho_qc_20 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test20_QC_KW())){
    Test20_QC_KW()$SALIDA_ARMADA$TABLA_GRUPOS
  } else return(NULL)
}))



# FRASE1 KW
output$frase01_ho_qc_20 <- renderUI({
  if (!is.null(Test20_QC_KW())){
    HTML( Test20_QC_KW()$SALIDA_ARMADA$FRASE_KW)
  } else return(NULL)
})



# FRASE3 Dunn
output$frase02_ho_qc_20 <- renderUI({
  if (!is.null(Test20_QC_KW())){
    HTML( Test20_QC_KW()$SALIDA_ARMADA$FRASE_GRUPOS)
  } else return(NULL)
})



# Grafico Boxplot para ANOVA
output$plot_ho_qc_20 <- renderPlot({ 
  
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 ) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        MINI <- BASE_goku()
        
        
        
        VR <- MINI[,1]
        
        FACTOR <- as.factor(MINI[,2])
        
        
        boxplot(VR ~ FACTOR, 
                #main=input$columna_graf_torta,
                col= "red",
                xlab= "Grupos",   
                ylab= "Variable")
        
        
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
})





Opc_ho_qc_20 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("Test de Kruskal-Wallis"),
   
  
                 h3("Resumen del Test de Kruskal-Wallis"),
                 tableOutput("tabla01_ho_qc_20"),
                 uiOutput("frase01_ho_qc_20"),
                 br(),
                 h3("Grupos Estadísticos"),
                 tableOutput("tabla02_ho_qc_20"),
                 uiOutput("frase02_ho_qc_20"), br(),
      
      h3("Gráfico Boxplot"),
      plotOutput("plot_ho_qc_20", width = "400px", height = "400px")
      
    )
    
  } else return(NULL)
  
})




