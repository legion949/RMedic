



# Texto inicial de AYUDA
output$TEXTO_00_TABLAS <- renderUI({ 
  
  
  if (!is.null(input$qtty_var_tablas))  if (input$qtty_var_tablas == ""){
    
      div("La pestaña 'Tablas' desarrolla:",
    tags$ol(
      tags$li('Para 1 o 2 variables numéricas: Medidas de Resumen (posición y dispersión) e Intervalos de Confianza para la media.'),
      tags$li("Para 1 variable categórica: Distribución de Frecuencias en tablas de simple entrada."),
      tags$li("Para 2 variables categóricas: Distribución de Frecuencias en tablas de doble y simple entrada."),
      tags$li("Para 1 variable categórica y 1 numérica: Medidas Resumen (particionadas) e Intervalos de Confianza para la media (particionados).")
    )
    )
  } else TEXTO <- NULL
  
})


menuDESCRIPTIVAS <- reactive({
  
  if (paso_BASE(BASE_SALIDA())) {
    
  tabs <- list()
  tabs[[1]] <- tabPanel(title = "Tablas", 
                        icon = icon("user-md"), 
                        value = 3, 
                        #id = "prueba1",
                        h3("Menú para Tablas Descriptivas"),
                        uiOutput("menu1_tablas"),
                        uiOutput("menu2_tablas"),
                        uiOutput("MODorden_var_tablas"),
                        br(), br(), br(), br(),  
                        uiOutput("TEXTO_00_TABLAS")
                        )
  tabs
  
  } else return(NULL)
})


# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Tablas".

observe(output[["SALIDAmenuDESCRIPTIVAS"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  tabs3 <- menuDESCRIPTIVAS()
  do.call(tabsetPanel, c(id = "goku", c(tabs1,tabs2, tabs3)))
  
 
  
}))

