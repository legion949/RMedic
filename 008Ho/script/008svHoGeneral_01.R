

# # # # #
# 1Q - 01 - Test de Proporciones




# Menu del opciones para el test de proporciones
output$ho_1q_01 <- renderUI({

 
    titulo_armado <- paste("Categoría de 'Éxito' de la variable '", colnames(BASE_goku())[1], "'", sep="")
    opciones_categorias <- levels(as.factor(BASE_goku()[,1]))
    
  div(
    # Seleccion de una categoria
    selectInput(inputId = "exito_ho_1q_01", 
                label = titulo_armado, 
                choices = opciones_categorias),
    br(),
    
    
    # Seleccion del valor bajo H0
    numericInput(inputId = "referencia_ho_1q_01",
                 label = "Valor bajo Hipótesis de proporción: ",
                 min=0,  max=1, step=0.01, value=0.50),
    br(),
    
    # Seleccion del tipo de prueba
    radioButtons("prueba_ho_1q_01", "Tipo de Prueba de Hipótesis:", 
                 choices = c("Bilateral" = "two.sided",
                             "Unilateral Izquierda" = "less",
                             "Unilateral Derecha" = "greater"))
    
    
  )
    

    
})

# Test de Proporciones

# Tabla de Frecuencias para las 2 variables
Test01_1Q_Proporciones <- reactive({

  if (!is.null(BASE_goku()))  if (!is.null(input$exito_ho_1q_01)){
            
             prop01_master( input_base = BASE_goku(),
                           input_exito = input$exito_ho_1q_01,
                           input_ref = input$referencia_ho_1q_01,
                           input_decimales = decimales_goku(),
                           input_formato = input$prueba_ho_1q_01,
                           input_alfa = input$alfa_ho)
            
  }

          
          
    
})
#######################################################

# Salida de tabla resumen del test de Proporciones 1Q
observe( output$tabla_ho_1q_01 <- renderTable(rownames = FALSE, digits=decimales_goku(), align = "c",{
  
 Test01_1Q_Proporciones()$tablas$Test_Prop01

}))

# Frase 1: Validacion
observe(output$frase01_ho_1q_01 <- renderUI({
HTML(Test01_1Q_Proporciones()$frases$Validacion)
}))

# Frase 2: Explicacion
observe(output$frase02_ho_1q_01 <- renderUI({
HTML(Test01_1Q_Proporciones()$frases$Explicacion)
}))


# Armado/Salida del test de Proporciones 1Q
Opc_ho_1q_01 <- reactive({

     div(
          h3("Detalles"),
          uiOutput("ho_1q_01"), br(),
          h3("Test de Proporciones"), br(),
          h3(span(uiOutput("frase01_ho_1q_01"), style="color:red")),
          tableOutput("tabla_ho_1q_01"),
          uiOutput("frase02_ho_1q_01"),
          br(), br()
        )

  })

                           
  
  
  
  