


# # # # #
# QC - 19 - ANOVA 1 FACTOR




# Test de Diferencia de Proporciones
Test19_QC_ANOVA  <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    if(!is.null(decimales_goku())) {
      if(!is.null(input$alfa_ho)) {
              
              
              
              DATOS <- BASE_goku()
          
        
              DATOS[,2] <- as.factor(as.character(DATOS[,2]))
              DECIMALES <- decimales_goku()
              ALFA <- input$alfa_ho
              
       #       DATOS <- mtcars[,c(1,2)]
        #      DATOS[,2] <- as.factor(as.character(DATOS[,2]))
              ALFA <- 0.05
              DECIMALES <- 2
              
              ANALISIS <- ANOVA01_MASTER( input_datos = DATOS,
                                          input_decimales = DECIMALES,
                                          input_alfa = ALFA)
              
              
              
              ANALISIS
              
     
      
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
})




# Salida Requisitos de ANOVA
observe( output$tabla01_ho_qc_19 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test19_QC_ANOVA())){
    Test19_QC_ANOVA()$SALIDA_ARMADA$RESUMEN_NH
  } else return(NULL)
}))


# Salida resumen de ANOVA
observe( output$tabla02_ho_qc_19 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test19_QC_ANOVA())){
    Test19_QC_ANOVA()$SALIDA_ARMADA$RESUMEN_ANOVA
  } else return(NULL)
}))



# Salida TABLA ANOVA
observe( output$tabla03_ho_qc_19 <- renderTable(rownames =T, digits=decimales_goku(), {
  
  if (!is.null(Test19_QC_ANOVA())){
    Test19_QC_ANOVA()$SALIDA_ARMADA$TABLA_ANOVA
  } else return(NULL)
}))



# Salida TABLA TUKEY
observe( output$tabla04_ho_qc_19 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  
  if (!is.null(Test19_QC_ANOVA())){
    Test19_QC_ANOVA()$SALIDA_ARMADA$TABLA_TUKEY
  } else return(NULL)
}))



# FRASE1 ANOVA
output$frase01_ho_qc_19 <- renderUI({
  if (!is.null(Test19_QC_ANOVA())){
    HTML( Test19_QC_ANOVA()$SALIDA_ARMADA$FRASE_ANOVA)
  } else return(NULL)
})




# FRASE2 Requisitos
output$frase02_ho_qc_19 <- renderUI({
  if (!is.null(Test19_QC_ANOVA())){
    HTML( Test19_QC_ANOVA()$SALIDA_ARMADA$FRASE_NH)
  } else return(NULL)
})




# FRASE3 tukey
output$frase03_ho_qc_19 <- renderUI({
  if (!is.null(Test19_QC_ANOVA())){
    HTML( Test19_QC_ANOVA()$SALIDA_ARMADA$FRASE_TUKEY)
  } else return(NULL)
})



# Grafico Boxplot para ANOVA
output$plot_ho_qc_19 <- renderPlot({ 
  
  
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





Opc_ho_qc_19 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      h3("ANOVA a 1 Factor"),
               h3("Resumen de Requisitos Estadísticos"),
               tableOutput("tabla01_ho_qc_19"),
               uiOutput("frase02_ho_qc_19"),
               h3("Resumen del Test ANOVA"),
               tableOutput("tabla02_ho_qc_19"),
               uiOutput("frase01_ho_qc_19"),
               h3("Tabla ANOVA"),
               tableOutput("tabla03_ho_qc_19"),
               h3("Test de Tukey"),
               tableOutput("tabla04_ho_qc_19"),
               uiOutput("frase03_ho_qc_19"), br(),
      
      h3("Gráfico Boxplot"),
      plotOutput("plot_ho_qc_19", width = "400px", height = "400px")
        )
  
    
  } else return(NULL)
  
})




