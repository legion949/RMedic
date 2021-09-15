



# # # # #
# QC - 16 - Homogeneidad de Varianzas de Bartlett




# Test de Homogeneidad de Varianzas
Test16_QC_Homogeneidad <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
        
        
        
        DATOS <- BASE_goku() 
        
        
        DECIMALES <- decimales_goku()
        ALFA <- input$alfa_ho
        
        
        
        ANALISIS <- BARTLETT_MASTER( input_datos = DATOS,
                                     input_decimales = DECIMALES,
                                     input_alfa = ALFA,
                                     input_ingreso="anova")
        
        ANALISIS
        
        
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})



# Salida de valores resumen Homogeneidad de Bartlett
observe( output$tabla_ho_qc_16 <- renderTable(rownames = F, digits=decimales_goku(), {
  
  if (!is.null(Test16_QC_Homogeneidad())){
    Test16_QC_Homogeneidad()$SALIDA_ARMADA[[2]]
  } else return(NULL)
}))


# Frase1 de Homogeneidad de Varianzas de Bartlett
output$frase01_ho_qc_16 <- renderUI({
  if (!is.null(Test16_QC_Homogeneidad())){
    HTML(Test16_QC_Homogeneidad()$SALIDA_ARMADA[[1]])
  } else return(NULL)
})

# Frase2 de Homogeneidad de Varianzas de Bartlett
output$frase02_ho_qc_16 <- renderUI({
  if (!is.null(Test16_QC_Homogeneidad())){
    HTML(Test16_QC_Homogeneidad()$SALIDA_ARMADA[[3]])
  } else return(NULL)
})

# Grafico Boxplot par Homogeneidad de Varianzas
output$plot_ho_qc_16 <- renderPlot({ 
  
  
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








Opc_ho_qc_16 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
        h3("Test de Homogeneidad de Varianzas de Bartlett"),
               tableOutput("tabla_ho_qc_16"),
               uiOutput("frase01_ho_qc_16"),
               uiOutput("frase02_ho_qc_16"), br(),
        
        h3("Gráfico Boxplot"),
        plotOutput("plot_ho_qc_16", width = "400px", height = "400px")
        
               
   
      
    )
    
  } else return(NULL)
  
})
















