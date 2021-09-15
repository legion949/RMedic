

# # # # #
# 2Q - 05 - Test de Proporciones

output$ho_2q_05 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    div(
      selectInput("exito1_ho_2q_05", paste("Variable1 - Divisor de Grupos (Solo visualización) '", colnames(BASE_goku())[1], "'", sep=""), 
                  choices = levels(as.factor(BASE_goku()[,1]))),
      selectInput("exito2_ho_2q_05", paste("Variable2: Seleccionar categoría de 'Éxito''", colnames(BASE_goku())[2], "'", sep=""), 
                  choices = levels(as.factor(BASE_goku()[,2])))
      
      
    )
    
  } else return(NULL)
  
})

# Test de Proporciones

# Tabla de Frecuencias para las 2 variables
Test05_2Q_Proporciones <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2 && sum(tipo_var_goku() == "Categórica") == 2) {
    if(!is.null(input$exito1_ho_2q_05) && sum(levels(as.factor(BASE_goku()[,1])) == input$exito1_ho_2q_05) == 1) {
      if(!is.null(input$exito2_ho_2q_05) && sum(levels(as.factor(BASE_goku()[,2])) == input$exito2_ho_2q_05) == 1) {
        if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            PROP02_MASTER( input_datos = BASE_goku(), 
                           input_exito1 = input$exito1_ho_2q_05, 
                           input_exito2 = input$exito2_ho_2q_05, 
                           input_decimales = decimales_goku(),
                           input_alfa = input$alfa_ho)
            
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  
  
})
#######################################################

# Salida de valores resumen estadistico Chi Cuadrado
observe( output$tabla_ho_2q_05 <- renderTable(rownames = FALSE, digits=decimales_goku(), {
  if(!is.null(Test05_2Q_Proporciones())) {
    Test05_2Q_Proporciones()$SALIDA_ARMADA$RESUMEN
  } else return(NULL)
}))

observe(output$frase01_ho_2q_05 <- renderUI({
  if(!is.null(Test05_2Q_Proporciones())) {
    HTML(Test05_2Q_Proporciones()$SALIDA_ARMADA$FRASE1)
  } else return(NULL)
}))


observe(output$frase02_ho_2q_05 <- renderUI({
  if(!is.null(Test05_2Q_Proporciones())) {
    HTML(Test05_2Q_Proporciones()$SALIDA_ARMADA$FRASE2)
  } else return(NULL)
}))


Opc_ho_2q_05 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
    
    
    div(
      uiOutput("ho_2q_05"), br(),
      h3("Test de Proporciones"), br(),
      tableOutput("tabla_ho_2q_05"),
      uiOutput("frase01_ho_2q_05"),
      uiOutput("frase02_ho_2q_05"),
      br(), br()
    )
    
    
    
  } else return(NULL)
  
})





