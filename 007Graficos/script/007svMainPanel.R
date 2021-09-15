




# Texto inicial de AYUDA
output$TEXTO_00_GRAFICOS <- renderUI({ 
  
  
  if (!is.null(input$qtty_var_graf))  if (input$qtty_var_graf == ""){
    
    div("La pestaña 'Gráficos' desarrolla una galería con el espectro de gráficos más utilizados según el tipo de variables seleccionado."
        # tags$ol(
        #   tags$li('Para 1 o 2 variables numéricas: Medidas Resumen (posición, disperión, cuartiles) e Intervalos de Confianza para la media.'),
        #   tags$li("Para 1 variable categórica: Distribución de Frecuencias en tablas de simple entrada"),
        #   tags$li("Para 2 variables categóricas: Distribución de Frecuencias en tablas de doble y simple entrada"),
        #   tags$li("Para 1 variable categórica y 1 numérica: Medidas Resumen Particionadas e Intervalos de Confianza para la media Particionados")
        # )
    )
  } else TEXTO <- NULL
  
})


# La pestania "Graficos"...
# Esta contenida en el siguiente objeto reactivo...
menuGRAFICOS <- reactive({
  
  if (paso_BASE(BASE_SALIDA())) {
    
  tabs <- list()
  tabs[[1]] <- tabPanel(title = "Gráficos", 
                        icon = icon("user-md"), 
                        value = 4, 
                        #id = "prueba1",
                        h3("Menú de Gráficos"),
                        uiOutput("menu1_graf"),
                        uiOutput("menu2_graf"),
                        uiOutput("MODorden_var_graf"),
                        br(), br(), br(), br(),  
                        uiOutput("TEXTO_00_GRAFICOS")
                        )
  tabs
  
  } else return(NULL)
  
})



# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
observe(output[["SALIDAmenuGRAFICOS"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  tabs3 <- menuDESCRIPTIVAS()
  tabs4 <- menuGRAFICOS()
  do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4))
  
}))


