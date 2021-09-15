

# # # # #
# 1Q - 02 - Test de Uniformidad

output$ho_1q_02 <- renderUI({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    div(
      selectInput("exito_ho_1q_02", paste("Categoría de 'Éxito' de la variable '", colnames(BASE_goku())[1], "'", sep=""), 
                  choices = levels(as.factor(BASE_goku()[,1]))),
      numericInput("referencia_ho_1q_02", "Valor bajo Hipótesis de proporción:", min=0,  max=1, step=0.01, value=0.50)
      
      
    )
    
  } else return(NULL)
  
})

# Test de Proporciones

# Tabla de Frecuencias para las 2 variables
Test02_1Q_Uniformidad <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1 && tipo_var_goku() == "Categórica") {
         if(!is.null(decimales_goku())) {
          if(!is.null(input$alfa_ho)) {
            
            uniformidad_master(input_base = BASE_goku(), 
                               input_decimales = decimales_goku(),
                               input_alfa = input$alfa_ho,
                               input_cadena = NULL
                               )
            
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)

  
  
  
})
#######################################################

# Salida de valores resumen estadistico Chi Cuadrado
observe( output$tabla_ho_1q_02 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c", {
  if(!is.null(Test02_1Q_Uniformidad())) {
    Test02_1Q_Uniformidad()$tablas$Test_Uniformidad
  } else return(NULL)
}))

observe(output$frase01_ho_1q_02 <- renderUI({
  if(!is.null(Test02_1Q_Uniformidad())) {
    HTML(Test02_1Q_Uniformidad()$frases$Validacion)
  } else return(NULL)
}))


observe(output$frase02_ho_1q_02 <- renderUI({
  if(!is.null(Test02_1Q_Uniformidad())) {
    HTML(Test02_1Q_Uniformidad()$frases$Explicacion)
  } else return(NULL)
}))


Opc_ho_1q_02 <- reactive({
  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    
    div(
      # h3("Test de Uniformidad"),
#      h3("Detalles"),
#      uiOutput("ho_1q_02"), br(),
      h3("Test de Uniformidad"), br(),
      tableOutput("tabla_ho_1q_02"),
      uiOutput("frase01_ho_1q_02"),
      uiOutput("frase02_ho_1q_02"),
      br(), br()
    )
    
    
    
  } else return(NULL)
  
})





