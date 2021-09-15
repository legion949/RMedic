

# Texto inicial de AYUDA
output$TEXTO_00_H0 <- renderUI({ 
  

  if(!is.null(input$qtty_var_ho))  if (input$qtty_var_ho == ""){
    div("La pestaña 'Pruebas de Hipótesis', según las variables seleccionadas desarrolla:",
        tags$ol(
          tags$li('Ayuda para elegir un test estadístico correcto.'),
          tags$li("Una galería con los test estadísticos más utilizados."),
          tags$li("Asistente estadístico automatizado: frases que ayudan a interpretar los resultados estadísticos obtenidos.")
        )
    )
  } else TEXTO <- NULL
  
})


# La pestania "hos"...
# Esta contenida en el siguiente objeto reactivo...
menuHO <- reactive({
  if (paso_BASE(BASE_SALIDA())) {
    
  tabs <- list()
  tabs[[1]] <- tabPanel(title = "Pruebas de Hipótesis", 
                        icon = icon("user-md"), 
                        value = 5, 
                        #id = "prueba1",
                        h3("Menú de Pruebas de Hipótesis"),
                        uiOutput("menu1_ho"),
                        uiOutput("menu2_ho"),
                        br(), br(), br(), br(),
                        uiOutput("TEXTO_00_H0")
                        )
  tabs
  
  } else return(NULL)
})


# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
observe(output[["SALIDAmenuHo"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  tabs3 <- menuDESCRIPTIVAS()
  tabs4 <- menuGRAFICOS()
  tabs5 <- menuHO()
  do.call(tabsetPanel,  c(id="goku", tabs1,tabs2, tabs3, tabs4, tabs5))

}))

