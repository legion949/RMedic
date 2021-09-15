
# Base de Datos a Controlar
BASE_CONTROL <- reactive({
  
  # Se agrego la siguiente sentencia como condicional...
  # if ( sum(colnames(BASE_SALIDA()) == input$menu2_control) > 0)
  # Por que... al cambiar de base de datos... por un momento tiene cargada el nombre de 
  # las columnas en el menu2 de la base anterior... entonces intenta crear la BASE_CONTROL 
  # con la Base nueva, pero con el nombre de las columnas de la base anterior...
  
  if (paso_detalle(input$menu2_control)) if ( sum(colnames(BASE_SALIDA()) == input$menu2_control) > 0) {
    seleccion <- BASE_SALIDA()[,input$menu2_control]
    dim(seleccion) <- c(length(seleccion), 1)
    colnames(seleccion) <- input$menu2_control
    seleccion
  } else return(NULL)
})

# Texto inicial de AYUDA
output$TEXTO_00_CONTROL <- renderText({ 
  
  
  if (is.null(BASE_CONTROL())) {

    TEXTO <- "Realice un control sobre cada variable de su Base de Datos.<br/>
              En variables categóricas verifique que todas las categorías presentes en la columna sean correctas.<br/>
              En variables numéricas verifique los valores de mínimo y máximo."
  
  } else TEXTO <- NULL
  
  })

# Control Numerico Frase01
output$TEXTO_01_CONTROL <- renderText({
  
  if (paso_detalle(input$menu2_control)) {
    
    dt_verdad <- is.numeric(BASE_CONTROL())
    
    nombres_columnas <- colnames(BASE_SALIDA())
    orden_col <- c(1:ncol(BASE_SALIDA()))
    esta_columna <- colnames(BASE_CONTROL())
    
    num_esta_columna <- orden_col[nombres_columnas == esta_columna]
    letra_esta_columna <- num2let(c(num_esta_columna))
    
    if(input$menu3_control == "Categórica") {
      
      TEXTO <- paste0("La variable '", colnames(BASE_CONTROL()), "' - (", letra_esta_columna,") es CATEGÓRICA")
      TEXTO
      #BASE_CONTROL()
      
    } else   if(input$menu3_control == "Numérica") {
      
      if (dt_verdad == TRUE) {  
        TEXTO <- paste0("La variable '", colnames(BASE_CONTROL()), "' - (", letra_esta_columna,") es NUMÉRICA")
        
        TEXTO
      } else {
        TEXTO <- paste0("Variable seleccionada: '", colnames(BASE_CONTROL()), "' - (", letra_esta_columna,")")
        TEXTO
        
        
      }
    } else return(NULL)
    
  } else return(NULL)
})

# Control Numerico Frase02
output$TEXTO_02_CONTROL <- renderText({
  
  if (paso_detalle(input$menu2_control)) {
    
    dt_verdad <- is.numeric(BASE_CONTROL())
    
    nombres_columnas <- colnames(BASE_SALIDA())
    orden_col <- c(1:ncol(BASE_SALIDA()))
    esta_columna <- colnames(BASE_CONTROL())
    
    num_esta_columna <- orden_col[nombres_columnas == esta_columna]
    letra_esta_columna <- num2let(c(num_esta_columna))
    
    if(input$menu3_control == "Categórica") {
      
      TEXTO <- NULL
      TEXTO
      #BASE_CONTROL()
      
    } else   if(input$menu3_control == "Numérica") {
      
      if (dt_verdad == TRUE) {  
        TEXTO <- NULL
        TEXTO
      } else {
        TEXTO <- c("Hay inconvenientes en la variable seleccionada.<br/>
                   Usted ha indicado que la variable es Numérica.<br/>
                   Sin embargo, se han encontrado en la columna elementos no numéricos<br/>
                   que deben ser corregidos.")
        TEXTO
        
        
      }
    } else return(NULL)
    
  } else return(NULL)
})

# Tabla Control
output$TABLA_CONTROL <- renderTable({
  
  # Si hay una columna seleccionada... y un tipo de dato...
  if (paso_detalle(input$menu2_control)  && paso_detalle(!is.null(input$menu3_control))) {
    
    # Tabla del Control
    tabla_control(BASE_CONTROL(), input$menu3_control, 10)
    
  } else return(NULL)
})



# La pestania "Control"...
# Esta contenida en el siguiente objeto reactivo...
menuCONTROL <- reactive({
  
  # Si hay una Base Salida ya cargada...
  if (paso_BASE(BASE_SALIDA())) {
    
    # Update Menu1  
    nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
    opciones_carga1 <- namel(nombres1)
    
    # Update Menu2  
    nombres2_original <- colnames(BASE_SALIDA())
    nombres2 <- nombres2_original
    letras_elegidas <- paste0("(", num2let(c(1:length(nombres2))), ")")
    nombres2 <- paste0(letras_elegidas, " - ", nombres2)
    observeEvent(input$menu1_control, {
      
      
      nuevo_orden <- c(1:length(nombres2))
      if (input$menu1_control == "Alfabético Ascendente")  nuevo_orden <- order(nombres2, decreasing = F)
      if (input$menu1_control == "Alfabético Descendente") nuevo_orden <- order(nombres2, decreasing = T)
      
      nombres2 <- nombres2[nuevo_orden]
      nombres2_original <- nombres2_original[nuevo_orden]
      
      opciones_carga2 <- namel(nombres2)
      
      # Agregamos un cambio a lo que hace "namel"
      # Mostrara la letra y el nombre de la columna, pero internamente manejara 
      # solo el nombre de la columna
      for (n in 1:length(opciones_carga2)) opciones_carga2[[n]] <- nombres2_original[n]
      
      updateSelectInput(session, 
                        inputId='menu2_control',
                        label="Variables",
                        choices=  c("Seleccione una" = "", opciones_carga2),
                        selected = NA)  
      
      
    })
    
    # Update Menu3  
    nombres3 <- c("Categórica", "Numérica")
    opciones_carga3 <- namel(nombres3)
    if(1 == 1){ observeEvent(input$menu2_control, {
      
      observe(cat("-", input$menu2_control, "-AVER\n", sep=""))
      
      if (!is.null(input$menu2_control) & input$menu2_control != "") {
        
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3)    
        if (is.numeric(BASE_CONTROL())) dt <- 2 else dt <- 1
        
        
        conditionalPanel("input.menu2_control != ''",
                         updateRadioButtons(session, 
                                            inputId='menu3_control',
                                            label="Tipo de Variable",
                                            choices=  opciones_carga3,
                                            selected = opciones_carga3[dt])  
        )
        
      } else return(NULL)
    })
    }
    
    
    # Update Menu4  
    nombres4_1 <- c("Nominal", "Ordinal")
    nombres4_2 <- c("Discreta", "Continua")
    opciones_carga4 <- namel(nombres4_1)
    observeEvent(input$menu3_control, {
      
      
      nombres4_1 <- c("Nominal", "Ordinal")
      nombres4_2 <- c("Discreta", "Continua")
      if (input$menu3_control == "Categórica") opciones_carga4 <- namel(nombres4_1) else opciones_carga4 <- namel(nombres4_2)
      
      updateRadioButtons(session, 
                         inputId='menu4_control',
                         label="Subtipo de Variable",
                         choices=  opciones_carga4,
                         selected = opciones_carga4[1])  
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    tabs <- list()
    tabs[[1]] <-  tabPanel(title = "Control", 
                           icon = icon("user-md"), 
                           value = 2, 
                           h3("Control de las Variables"),
                           #id = "prueba1",
                           
                           fluidRow(
                             column(3,
                                    # Menu1: Orden de las seleccion de variables         
                                    selectInput(inputId='menu1_control',
                                                label="Orden de las variables",
                                                choices=  opciones_carga1,
                                                selected = opciones_carga1[1]
                                    )
                             ),
                             
                             column(3,
                                    # Menu2: La variable a controlar
                                    selectInput(inputId="menu2_control", 
                                                label="Variable",
                                                choices=  c("Seleccione una" = ""),
                                                selected = NA
                                    )
                             ),
                             
                             column(3,
                                    # Menu3: Tipo de variable
                                    #??????????????????????????????????????? Sale igual...
                                    conditionalPanel("input.menu2_control != ''",      
                                                     radioButtons(inputId='menu3_control',
                                                                  label="Tipo de Variable",
                                                                  choices=  opciones_carga3,
                                                                  selected = opciones_carga3[1]
                                                                  
                                                     )
                                    )
                                    
                             ) # Columna
                           ), # FuidRow
                           
                           h3(textOutput("TEXTO_01_CONTROL")),
                           h3(span(htmlOutput("TEXTO_02_CONTROL"), style="color:red")),
                           tableOutput("TABLA_CONTROL"),
                           br(), br(), br(), br(), 
                           htmlOutput("TEXTO_00_CONTROL")
                           
    ) # Fin TabPanel
    tabs
    
  } else return(NULL)
  
})




#############################################################






# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
observe(output[["SALIDAmenuCONTROL"]] <- renderUI({
  
  tabs1 <- menuBASE()
  tabs2 <- menuCONTROL()
  #  tabs2 <- uiOutput("uiMainPanel_005CONTROL")
  
  do.call(tabsetPanel, c(tabs1, tabs2))
  
}))




# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
observe(output[["SALIDAmenuBASE"]] <- renderUI({
  
  menuBASE()
  
}))
