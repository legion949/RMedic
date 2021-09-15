
# # # # OBJETOS OUTPUT MINIMOS DE CADA PESTANIA

# Objetos de Tablas
if (1 == 1) {
  
  # Menu 1
  ### Orden de las variables, Variable, Decimales
  output$menu1_tablas <- renderUI({
    
    if (paso_BASE(BASE_SALIDA())) {  
      
      conditionalPanel(" 1 == 1",
                       fluidRow(
                         column(4,
                                selectInput(inputId='orden1_tablas',
                                            label="Orden de las variables",
                                            choices=c("Orden Original"=1,
                                                      "Alfabético Ascendente"=2
                                            ),
                                            selected = 1
                                            # "Cuatro"='Cuatro',
                                            # "Cinco"='Cinco'
                                )
                         ),
                         column(4, 
                                selectInput(inputId='qtty_var_tablas',
                                            label="Cantidad de Variables",
                                            choices=c(c("Seleccione..." = "",
                                                        "Una"=1,
                                                        "Dos"=2
                                                        #    "Tres o más"=3
                                                        # "Cuatro"='Cuatro',
                                                        # "Cinco"='Cinco'
                                            )))
                         ),           
                       
                         column(4, 
                                numericInput("decimales_tablas", "Decimales:", min=0,  max=100, step=1, value=2)
                         )
                       )
      )
      
    } else return(NULL)
    
  })  
  
  
  # Var1, TipoVar1 y Armado de eso
  output$MODvar1_tablas <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_tablas == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var1_tablas", 
                  label="Variable 1",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$MODtipo_var1_tablas <- renderUI({
    
    if (paso_detalle(input$var1_tablas)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var1_tablas
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var1_tablas]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var1_tablas',
                     label = "Tipo de Variable1",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var1_tablas_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_tablas)) {
      if (input$qtty_var_tablas >= 1) {
        
        fluidRow(
          column(3, 
                 uiOutput("MODvar1_tablas")        ),
          conditionalPanel('input.var1_tablas != ""',
                           column(3, 
                                  uiOutput("MODtipo_var1_tablas"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  # var2, Tipovar2 y Armado de eso
  output$MODvar2_tablas <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_tablas == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var2_tablas", 
                  label="Variable 2",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$MODtipo_var2_tablas <- renderUI({
    
    if (paso_detalle(input$var2_tablas)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var2_tablas
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var2_tablas]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var2_tablas',
                     label = "Tipo de Variable2",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var2_tablas_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_tablas)) {
      if (input$qtty_var_tablas >= 2) {
        
        fluidRow(
          column(3, 
                 uiOutput("MODvar2_tablas")        ),
          conditionalPanel('input.var2_tablas != ""',
                           column(3, 
                                  uiOutput("MODtipo_var2_tablas"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  
  # Orden de ingreso, si son dos
  output$MODorden_var_tablas <- renderUI({
    
    if(paso_detalle(input$qtty_var_tablas) && as.numeric(input$qtty_var_tablas > 1)) {
    if(paso_detalle(input$tipo_var1_tablas)) {
      if(paso_detalle(input$tipo_var2_tablas)) {
        if (input$tipo_var1_tablas == input$tipo_var2_tablas) {
          
          
          if (input$tipo_var1_tablas == "Categórica") nyc <- "En filas..." else nyc <- "En el eje X..."
          var.opts <- c(input$var1_tablas, input$var2_tablas)
          selectInput(inputId = "orden_var_tablas", 
                      label = nyc,
                      choices = c("Seleccione una" = "", var.opts),
                      selected = var.opts[1])
          
        } else return(NULL)        
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    
  })
  
  
  # Menu 2 
  ### Var y TipoVar a partir de los armados "all"
  output$menu2_tablas <- renderUI({
    
    
    fluidRow(
      column(2),
      column(9,
             fluidRow( uiOutput("var1_tablas_all")),           
             fluidRow( uiOutput("var2_tablas_all"))
      )
    ) # Fin fluidrow
    
    
    
  })
  
  
}


# Objetos de Graficos
if (1 == 1) {
  # Menu 1
  ### Orden de las variables, Variable, Decimales
  output$menu1_graf <- renderUI({
    
    if (paso_BASE(BASE_SALIDA())) {  
      
      conditionalPanel(" 1 == 1",
                       fluidRow(
                         column(4,
                                selectInput(inputId='orden1_graf',
                                            label="Orden de las variables",
                                            choices=c("Orden Original"=1,
                                                      "Alfabético Ascendente"=2
                                            ),
                                            selected = 1
                                            # "Cuatro"='Cuatro',
                                            # "Cinco"='Cinco'
                                )
                         ),
                         column(4, 
                                selectInput(inputId='qtty_var_graf',
                                            label="Cantidad de Variables",
                                            choices=c(c("Seleccione..." = "",
                                                        "Una"=1,
                                                        "Dos"=2
                                                        #    "Tres o más"=3
                                                        # "Cuatro"='Cuatro',
                                                        # "Cinco"='Cinco'
                                            )))
                         ),           
                      
                         column(4, 
                                numericInput("decimales_graf", "Decimales:", min=0,  max=100, step=1, value=2)
                         )
                       )
      )
      
    } else return(NULL)
    
  })
  
  
  
  # Var1, TipoVar1 y Armado de eso
  output$var1_graf <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_graf == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var1_graf", 
                  label="Variable 1",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$tipo_var1_graf <- renderUI({
    
    if (paso_detalle(input$var1_graf)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var1_graf
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var1_graf]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var1_graf',
                     label = "Tipo de Variable1",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var1_graf_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_graf)) {
      if (input$qtty_var_graf >= 1) {
        
        fluidRow(
          column(3, 
                 uiOutput("var1_graf")        ),
          conditionalPanel('input.var1_graf != ""',
                           column(3, 
                                  uiOutput("tipo_var1_graf"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  # var2, Tipovar2 y Armado de eso
  output$var2_graf <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_graf == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var2_graf", 
                  label="Variable 2",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$tipo_var2_graf <- renderUI({
    
    if (paso_detalle(input$var2_graf)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var2_graf
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var2_graf]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var2_graf',
                     label = "Tipo de Variable2",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var2_graf_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_graf)) {
      if (input$qtty_var_graf >= 2) {
        
        fluidRow(
          column(3, 
                 uiOutput("var2_graf")        ),
          conditionalPanel('input.var2_graf != ""',
                           column(3, 
                                  uiOutput("tipo_var2_graf"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  
  output$MODorden_var_graf <- renderUI({
    
    if(paso_detalle(input$tipo_var1_graf)) {
      if(paso_detalle(input$tipo_var2_graf)) {
        if(paso_detalle(input$qtty_var_graf) && input$qtty_var_graf > 1) {
        if (input$tipo_var1_graf == input$tipo_var2_graf) {
          
          var.opts <- c(input$var1_graf, input$var2_graf)
          if (input$tipo_var1_graf == "Categórica") nyc <- "En filas..." else nyc <- "En el eje X..."
          selectInput(inputId = "orden_var_graf", 
                      label = nyc,
                      choices = c("Seleccione una" = "", var.opts),
                      selected = var.opts[1])
          
          } else return(NULL)
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    
  })
  
  
  # Menu 2 
  ### Var y TipoVar a partir de los armados "all"
  output$menu2_graf <- renderUI({
    
    
    
    fluidRow(
      column(2),
      column(9,
             fluidRow( uiOutput("var1_graf_all")),           
             fluidRow( uiOutput("var2_graf_all"))
      )
    ) # Fin fluidrow
    
    
    
  })
  
  
  
  
  
}


# Objetos de Ho

if (1 == 1) {
  
  # Menu 1
  ### Orden de las variables, Variable, Decimales
  output$menu1_ho <- renderUI({
    
    if (paso_BASE(BASE_SALIDA())) {  
      
      conditionalPanel(" 1 == 1",
                       fluidRow(
                         column(3,
                                selectInput(inputId='orden1_ho',
                                            label="Orden de las variables",
                                            choices=c("Orden Original"=1,
                                                      "Alfabético Ascendente"=2
                                            ),
                                            selected = 1
                                            # "Cuatro"='Cuatro',
                                            # "Cinco"='Cinco'
                                )
                         ),
                         column(3, 
                                selectInput(inputId='qtty_var_ho',
                                            label="Cantidad de Variables",
                                            choices=c(c("Seleccione..." = "",
                                                        "Una"=1,
                                                        "Dos"=2
                                                        #    "Tres o más"=3
                                                        # "Cuatro"='Cuatro',
                                                        # "Cinco"='Cinco'
                                            )))
                         ),           
                        
                         column(3, 
                                numericInput("alfa_ho", "Alfa:", min=0,  max=1, step=0.01, value=0.05)
                         ),
                         column(3, 
                                numericInput("decimales_ho", "Decimales:", min=0,  max=100, step=1, value=2)
                         )
                       )
      )
      
    } else return(NULL)
    
  })  
  
  # Var1, TipoVar1 y Armado de eso
  output$MODvar1_ho <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_ho == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var1_ho", 
                  label="Variable 1",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$MODtipo_var1_ho <- renderUI({
    
    if (paso_detalle(input$var1_ho)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var1_ho
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var1_ho]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var1_ho',
                     label = "Tipo de Variable1",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var1_ho_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_ho)) {
      if (input$qtty_var_ho >= 1) {
        
        fluidRow(
          column(3, 
                 uiOutput("MODvar1_ho")        ),
          conditionalPanel('input.var1_ho != ""',
                           column(3, 
                                  uiOutput("MODtipo_var1_ho"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  # var2, Tipovar2 y Armado de eso
  output$MODvar2_ho <- renderUI({
    
    
    
    if (paso_BASE(BASE_SALIDA())) {
      
      
      nombres_originales <- colnames(BASE_SALIDA())
      orden <- c(1:ncol(BASE_SALIDA()))
      if (input$orden1_ho == 2) {
        orden <- order(nombres_originales, decreasing = F)    
      }
      
      nombres_ordenados <- nombres_originales[orden]
      detalle <- engrampar_letras(nombres_ordenados)
      
      var.opts <- namel(nombres_ordenados)
      names(var.opts) <- detalle
      
      
      
      
      selectInput(inputId="var2_ho", 
                  label="Variable 2",
                  choices=c("Seleccione una" = "", var.opts),
                  selected = NA)
      
    } else return(NULL)
    
  })
  output$MODtipo_var2_ho <- renderUI({
    
    if (paso_detalle(input$var2_ho)) {
      #  if (paso_BASE(BASE_SALIDA())) {  
      
      nombres1 <- colnames(BASE_SALIDA())
      nombres2 <- input$var2_ho
      
      if (sum(nombres1 == nombres2) > 0) {
        
        mini <- BASE_SALIDA()[,input$var2_ho]
        dt <- is.numeric(mini)
        
        nombres3 <- c("Categórica", "Numérica")
        opciones_carga3 <- namel(nombres3) 
        
        if (dt == TRUE) salida <- 2 else salida <- 1
        
        radioButtons(inputId = 'tipo_var2_ho',
                     label = "Tipo de Variable2",
                     choices = opciones_carga3,
                     selected = opciones_carga3[salida]
                     #selected ="Categórica"
        )
        
      }
      # }
    } else return(NULL)
  })
  output$var2_ho_all <- renderUI({
    
    if (paso_detalle(input$qtty_var_ho)) {
      if (input$qtty_var_ho >= 2) {
        
        fluidRow(
          column(3, 
                 uiOutput("MODvar2_ho")        ),
          conditionalPanel('input.var2_ho != ""',
                           column(3, 
                                  uiOutput("MODtipo_var2_ho"))
          ))
        
      } else return(NULL)
    } else return(NULL)
    
  })  
  
  # Orden de ingreso, si son dos
  output$MODorden_var_ho <- renderUI({
    
    if(paso_detalle(input$qtty_var_ho) && as.numeric(input$qtty_var_ho > 1)) {
      if(paso_detalle(input$tipo_var1_ho)) {
        if(paso_detalle(input$tipo_var2_ho)) {
          if (input$tipo_var1_ho == input$tipo_var2_ho) {
            
            var.opts <- c(input$var1_ho, input$var2_ho)
            if (input$tipo_var1_ho == "Categórica") nyc <- "En filas..." else nyc <- "En el eje X..."
            selectInput(inputId = "orden_var_ho", 
                        label = nyc,
                        choices = c("Seleccione una" = "", var.opts),
                        selected = var.opts[1])
            
          } else return(NULL)        
        } else return(NULL)
      } else return(NULL)
    } else return(NULL)
    
  })
  
  
  # Menu 2 
  ### Var y TipoVar a partir de los armados "all"
  output$menu2_ho <- renderUI({
    
    
    fluidRow(
      column(2),
      column(9,
             fluidRow( uiOutput("var1_ho_all")),           
             fluidRow( uiOutput("var2_ho_all"))
      )
    ) # Fin fluidrow
    
    
    
  })
  
  
}