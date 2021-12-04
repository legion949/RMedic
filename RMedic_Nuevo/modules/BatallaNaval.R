## Segmento del UI
BatallaNavalUI <- function(id) {
  ns <- NS(id)
  

 uiOutput(ns("ARMADO_BATALLON"))
  
}




## Segmento del server
BatallaNavalSERVER <- function(input, output, session, 
                               base, zocalo_CIE, 
                               verbatim ) {

  ns <- session$ns
  
 
  
  if(is.null(base)) return(NULL)
  
  # return(
  #   list(
  #     frec_input = reactive({ input$frec_input }),
  #     max_input = reactive({ input$max_input })
  #   )
  # )
  
  
  
  
  ##################################
  
  OpcionesColumnas <- reactive({
  if(is.null(base())) return(NULL)
  if(is.null(input$orden)) return(NULL)  
    
    
    nombres2_original <- colnames(base())
    opciones_carga2 <- OpcionesDeColumnas(nombres2_original)
    
    # Orden Original de la base...
    if(input$orden == 1) {
      nuevo_orden <- c(1:length(nombres2_original))
      opciones_carga2 <- opciones_carga2[nuevo_orden]
    } else
    # Orden Creciente...  
    if(input$orden == 2) {
        nuevo_orden <- order(nombres2_original, decreasing = F)
        opciones_carga2 <- opciones_carga2[nuevo_orden]
      } else
      # Orden Decreciente...  
        if(input$orden == 3) {
          nuevo_orden <- order(nombres2_original, decreasing = T)
          opciones_carga2 <- opciones_carga2[nuevo_orden]
        } 
        
          opciones_carga2
   
   
  })

  observe({
    
     freezeReactiveValue(input, "var1")
    updateSelectInput(session, 
                      inputId = "var1",
                      label = "Variable 1: ",
                      choices = c("Seleccione una... " = "", OpcionesColumnas())
    )
  })
  

  observeEvent(input$qtty_var,{
    
    if(input$qtty_var != '') {
      
      if(input$qtty_var >= 1) {
    freezeReactiveValue(input, "var1")
    updateSelectInput(session, 
                      inputId = "var1",
                      label = "Variable 1: ",
                      choices = c("Seleccione una... " = "", OpcionesColumnas())
    )
      }
    
      
      if(input$qtty_var >= 2) {
        freezeReactiveValue(input, "var2")
        updateSelectInput(session, 
                          inputId = "var2",
                          label = "Variable 2: ",
                          choices = c("Seleccione una... " = "", OpcionesColumnas())
        )
      }
      
      
    }
  })
  
  
  observe({
    if(!is.null(base())) {
      if(!is.null(input$var1)) {
        if(input$var1 != "") {
          
          if(is.numeric(base()[,input$var1])) salida <- 2 else salida <- 1
          
          mis_opciones <- c("Categórica" = 1, "Numérica" = 10)
          
           freezeReactiveValue(input, "tipo_var1")
          updateRadioButtons(session, 
                             inputId = "tipo_var1",
                             label = "Tipo Variable 1: ",
                             choices = mis_opciones,
                             selected = mis_opciones[salida])
          
          
        }
      }
    }
    
  })
  
  ##############################
  
  observe({
    
     freezeReactiveValue(input, "tipo_var2")
    updateSelectInput(session, 
                      inputId = "var2",
                      label = "Variable 2: ",
                      choices = c("Seleccione una... " = "", OpcionesColumnas())
    )
  })
  
  observe({
    if(!is.null(base())) {
      if(!is.null(input$var2)) {
        if(input$var2 != "") {
          
          if(is.numeric(base()[,input$var2])) salida <- 2 else salida <- 1
          
          mis_opciones <- c("Categórica" = 1, "Numérica" = 10)
          
          freezeReactiveValue(input, "tipo_var2")
          updateRadioButtons(session, 
                             inputId = "tipo_var2",
                             label = "Tipo Variable 2: ",
                             choices = mis_opciones,
                             selected = mis_opciones[salida])
          
          
        }
      }
    }
    
  })
  
  ##############################
  
  
  observe({
    if(!is.null(base())) {
      if(!is.null(input$qtty_var)) {
      if(input$qtty_var == 2) {
           if(!is.null(input$tipo_var1)){
           if(!is.null(input$tipo_var2)){
                if(input$tipo_var1 != "") {
                if(input$tipo_var2 != "") {
              
                 
            if(input$tipo_var1 == input$tipo_var2){
            
              
             armado <- c(F, T)
             names(armado) <- c(input$var1, input$var2)
   
             if(input$tipo_var1 == 1) rotulo <- "En filas: " else
               if(input$tipo_var1 == 10) rotulo <- "En el eje X: " else
               
             freezeReactiveValue(input, "flip")
            updateSelectInput(session,
                               inputId = "flip",
                               label = rotulo,
                               choices = armado,
                               selected = armado[1])
            
            
             }
            
          
            }
          }
                }
                }
              }
              }
          }
          
      
      
    
    
  })
  ##################################################
  
  
  
  # Batalla Naval
  user_selection <- reactive({
    
    if(is.null(input$qtty_var)) return(NULL)
    if(input$qtty_var == '') return(NULL)
    
    variables <- c()
    tipo_variables <- c()
    numero_tipo <- c()
    caso_tipo_variables <- c() # Es un valor unico
    verificacion_interna <- c()
    verificacion_general <- c() # Es un valor unico
    lenguaje_tipo <- c()
    
    # Todo para 1 variable
    if(input$qtty_var == 1) {
      
      if(is.null(input$var1)) return(NULL)
      if(is.na(input$var1)) return(NULL)
      if(input$var1 == "") return(NULL)
      if(is.null(input$tipo_var1)) return(NULL)
      if(is.na(input$tipo_var1)) return(NULL)
      if(input$tipo_var1 == "") return(NULL)
      
      # Variable
      variables[1] <- input$var1
      
      # Numero de tipo de variable
      numero_tipo[1] <- as.numeric(input$tipo_var1)
      
      # Tipo de variable
      tipo_variables[numero_tipo ==  1] <- "Character"
      tipo_variables[numero_tipo == 10] <- "Numeric"
      
      # Lenguage en tipo de variable
      lenguaje_tipo[numero_tipo ==  1] <- "Categórica"
      lenguaje_tipo[numero_tipo == 10] <- "Numérica"
      
      # Verificacion interna
      dt_numerica1_interna <- is.numeric(base()[,input$var1])
      dt_numerica1_externa <- tipo_variables[1] == "Numeric"
      
      
      # Por defecto esta todo bien.
      # Hay un solo caso en el que el sistema esta mal, que es
      # cuando la variable internamente no es numerica y el usuario
      # insiste en que si lo es.
      # En ese caso no pasamos la verificacion interna...
      verificacion_interna[1] <- TRUE
      if(!dt_numerica1_interna){
        if(dt_numerica1_externa) verificacion_interna[1] <- FALSE
      }
      
      
      
    }
    
    
    # Todo para 2 variables
    if(input$qtty_var == 2) {
      
      if(is.null(input$var1)) return(NULL)
      if(is.na(input$var1)) return(NULL)
      if(input$var1 == "") return(NULL)
      if(is.null(input$tipo_var1)) return(NULL)
      if(is.na(input$tipo_var1)) return(NULL)
      if(input$tipo_var1 == "") return(NULL)
      
      if(is.null(input$var2)) return(NULL)
      if(is.na(input$var2)) return(NULL)
      if(input$var2 == "") return(NULL)
      if(is.null(input$tipo_var2)) return(NULL)
      if(is.na(input$tipo_var2)) return(NULL)
      if(input$tipo_var2 == "") return(NULL)
      
      # Variables
      variables[1] <- c(input$var1)
      variables[2] <- c(input$var2)
      
      # Numero de tipo de variable
      numero_tipo[1] <- as.numeric(input$tipo_var1)
      numero_tipo[2] <- as.numeric(input$tipo_var2)
      numero_tipo <- as.numeric(numero_tipo)
      
      # Tipo de variable
      tipo_variables[numero_tipo ==  1] <- "Character"
      tipo_variables[numero_tipo == 10] <- "Numeric"
      
      # Lenguage en tipo de variable
      lenguaje_tipo[numero_tipo ==  1] <- "Categórica"
      lenguaje_tipo[numero_tipo == 10] <- "Numérica"
      
      # Verificacion interna
      dt_numerica1_interna <- is.numeric(base()[,input$var1])
      dt_numerica1_externa <- tipo_variables[1] == "Numeric"
      dt_numerica2_interna <- is.numeric(base()[,input$var2])
      dt_numerica2_externa <- tipo_variables[2] == "Numeric"
      
      verificacion_interna[1] <- TRUE
      verificacion_interna[2] <- TRUE
      if(!dt_numerica1_interna) if(dt_numerica1_externa) verificacion_interna[1] <- FALSE
      if(!dt_numerica2_interna) if(dt_numerica2_externa) verificacion_interna[2] <- FALSE
      
    
    } # Fin para 2 variables
    
    
    # Para todas las salidas posibles...
    # Determinamos pos_RMedic
    {
      
      
      
      suma_caso <- sum(as.numeric(numero_tipo))
      casos_posibles <- c(1, 10, 2, 20, 11)
      
      # Determinamos los 5 casos para RMedic
      # 1) 1Q =  1 puntos
      # 2) 1C = 10 puntos
      # 3) 2Q =  2 puntos
      # 4) 2C = 20 puntos
      # 5) QC o CQ = 11 puntos
      orden_casos <- c(1:length(casos_posibles))
      dt_caso <- suma_caso == casos_posibles
      caso_tipo_variables[1] <- orden_casos[dt_caso]
      
      # Verificacion General
      if(sum(verificacion_interna) == length(verificacion_interna)){
        verificacion_general <- TRUE
      }  else  verificacion_general <- FALSE
      
      ###
    }
    ###########################################################
    
    # Return Exitoso
    my_exit <- list(variables = variables, 
                    numero_tipo = numero_tipo,
                    tipo_variables = tipo_variables,
                    caso_tipo_variables = caso_tipo_variables,
                    verificacion_interna = verificacion_interna,
                    verificacion_general = verificacion_general,
                    lenguaje_tipo = lenguaje_tipo)
    
    return(my_exit)
    
    
  })
  
  
  # Batalla Naval
  batalla_naval <- reactive({
    
    if(is.null(input$qtty_var)) return(NULL)
    if(input$qtty_var == '') return(NULL)
    if(is.null(user_selection())) return(NULL)
    if(input$qtty_var != 2) return(user_selection())
    
    # Solo sigue si hay 2 variables en juego!
    
    variables <- user_selection()$variables 
    numero_tipo <- user_selection()$numero_tipo
    tipo_variables <- user_selection()$tipo_variables
    caso_tipo_variables <- user_selection()$caso_tipo_variables
    verificacion_interna <- user_selection()$verificacion_interna
    verificacion_general <- user_selection()$verificacion_general
    lenguaje_tipo <- user_selection()$lenguaje_tipo

      # Rotacion!      # Numerica, #categorica
      # Cuando hay una variable de cada tipo, se ordenada todo
      # para que la primera variable sea la categorica y la 2da la numerica
      # Es una forma de trabajo estandard.
    
      # Si esta modo el modelo de cambio... Rotamos!
      #                 Numerica  y    categorica
      modelo_cambio <- c(10,             1)

      if (identical(numero_tipo, modelo_cambio)){
        variables <- variables[c(2,1)]
        numero_tipo <- numero_tipo[c(2,1)]
        tipo_variables <- tipo_variables[c(2,1)]
        verificacion_interna <- verificacion_interna[c(2,1)]
        lenguaje_tipo <- lenguaje_tipo[c(2,1)]
      }


      
      # Flip!
      # Esto es para cuando las dos variables son cuantitativas
      # o las dos son cualitativas
      
      if(tipo_variables[1] == tipo_variables[2]) {
        if(is.null(input$flip)) return(NULL)
        if(input$flip == '') return(NULL)
        
      if(input$flip) {
        variables <- variables[c(2,1)]
        numero_tipo <- numero_tipo[c(2,1)]
        tipo_variables <- tipo_variables[c(2,1)]
        verificacion_interna <- verificacion_interna[c(2,1)]
        lenguaje_tipo <- lenguaje_tipo[c(2,1)]
      }
      }
      
    
   

      # Return Exitoso
      my_exit <- list(variables = variables, 
                      numero_tipo = numero_tipo,
                      tipo_variables = tipo_variables,
                      caso_tipo_variables = caso_tipo_variables,
                      verificacion_interna = verificacion_interna,
                      verificacion_general = verificacion_general,
                      lenguaje_tipo = lenguaje_tipo)
      
      return(my_exit)
    
   
  })

 
  
  
  
  # Zocalo
  zocalo <- reactive({
    
    if(is.null(user_selection())) return(NULL)

    # Armado vacio
    # La 1ra opcion para html
    # La 2da opcion es para el download
    armado <- list(c(), c())
    names(armado)[1] <- "Batalla Naval"
    names(armado)[2] <- "Batalla Naval"
    
    

    # Todo para variable 1
    if(input$qtty_var >= 1) {
    
    if(sum(colnames(base()) == input$var1) == 0) return(NULL)
      
      armado[[1]] <- paste0("<b>Variable 1: </b>", user_selection()[[1]][1], 
                  " - Columna ", MyLetter(Base = base(), the_col = user_selection()[[1]][1]),
                  " - ",  user_selection()$lenguaje_tipo[1])
      

    }
    
    # Todo para la variable 2    
    if(input$qtty_var >= 2) {
      
      if(sum(colnames(base()) == input$var2) == 0) return(NULL)
      
      armado[[1]] <- paste0(armado[[1]], "<br/>",
                            "<b>Variable 2: </b>", user_selection()[[1]][2], 
                " - Columna ", MyLetter(Base = base(), the_col = user_selection()[[1]][2]),
                " - ",  user_selection()$lenguaje_tipo[2])
      
      
    }
    
    
 
    if(!is.null(zocalo_CIE()[[1]])){

      armado[[1]] <- paste0(armado[[1]], "<br/>",  zocalo_CIE()[[1]])
      armado[[2]] <-  c(armado[[2]], zocalo_CIE()[[2]])
    }
    
    
    armado[[1]] <- HTML(armado[[1]])
   
    names(armado)[1] <- "Batalla Naval"
    names(armado)[2] <- "Batalla Naval"
    return(armado)
    
  })
  
  decimales <- reactive({
    if(is.null(input$decimales)) return(NULL)
    
    return(input$decimales)
  })
  
  
  qtty_var <- reactive({
    if(is.null(input$qtty_var)) return(NULL)
    if(input$qtty_var == '') return(NULL)
    return(input$qtty_var)
    
    
  })
  
  
  output$message01 <- renderText({
    
    if(is.null(batalla_naval())) return(NULL)
    
    if(!batalla_naval()[[5]][1]) {
      armado <- paste0("<b><u>Advertencia:</u> ", 
                       "La variable 1 '", input$var1, "' no puede
                        ser considerada numérica ya que posee letras y/o símbolos.
                        Esto imposibilita la generación de cualquier tipo de tablas, gráficos y test
                        estadísticos. Realice un CONTROL sobre esta variable.</b>", 
                       "<br/><br/><br/><br/>")
      
      armado
      } else return(NULL)
  })

  output$message02 <- renderText({ 
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[5]])) return(NULL)
    if(is.null(batalla_naval()[[5]][2])) return(NULL)
    if(is.na(batalla_naval()[[5]][2])) return(NULL)
                            
    if(!batalla_naval()[[5]][2]) {
      armado <- paste0("<b><u>Advertencia:</u> ", 
                       "La variable 2 '", input$var2, "' no puede
                        ser considerada numérica ya que posee letras y/o símbolos.
                        Esto imposibilita la generación de cualquier tipo de tablas, gráficos y test
                        estadísticos.
                        Realice un CONTROL sobre esta variable.</b>", 
                       "<br/><br/><br/><br/>")
      
      armado
    } else return(NULL)
    })
  
  output$message03 <- renderText({
    if(is.null(batalla_naval())) return(NULL)
    if(length(batalla_naval()[[1]]) != 2) return(NULL)
    if(is.null(batalla_naval()[[1]][1])) return(NULL)
    if(is.na(batalla_naval()[[1]][1])) return(NULL)
    
 
      if (batalla_naval()[[1]][1] == batalla_naval()[[1]][2]){
      
        armado <- paste0("<b><u>Advertencia:</u> Has seleccionado dos
                         veces la variable '", input$var1, "'.</b>", 
                         "<br/><br/><br/><br/>")
        
        armado
        
      } else return(NULL)
   
  })
  
  
  output$Zocalo <- renderText({
    
    # Usamos solo la salida HTML
    paste0(
    div(
      h3(names(zocalo())[1]),
      zocalo()[[1]]
    )
    )

  }) 
  
  output$MiTexto_BatallaNaval <- renderText({
    
  
    
    if(is.null(verbatim)) return(NULL)
    if(!verbatim) return(NULL)
    
    unlist(batalla_naval()) 
  }) 
  
  
  output$ARMADO_BATALLON <- renderUI({
    
    if(is.null(base())) return(NULL)
    div(
      fluidRow(
        column(4, 
               selectInput(inputId = ns("orden"), 
                           label = "Orden: ",
                           choices = c("Orden Original" = 1, 
                                       "Albético Creciente" = 2,
                                       "Albatético Decreciente" = 3)
               )
        ),
        column(4,
               selectInput(inputId = ns("qtty_var"),
                           label = "Cantidad de variables",
                           choices = c("Seleccione... " = "", 1, 2)
               )
        ),
        column(4,
               numericInput(inputId = ns("decimales"),
                            label = "Decimales",
                            value = 2,
                            min = 0,
                            max = 10,
                            step = 1)
        )
      ),
      conditionalPanel(condition = "input.qtty_var != ''", ns = ns,
                       fluidRow(
                         conditionalPanel(condition = "input.qtty_var >= 1", ns = ns,
                                          column(4,
                                                 selectInput(inputId = ns("var1"),
                                                             label = "Variable 1: ",
                                                             choices = "")
                                          ),
                                          column(4,
                                                 conditionalPanel(condition = "input.var1 != ''", ns = ns,
                                                                  radioButtons(inputId = ns("tipo_var1"),
                                                                               label = "Tipo Variable 1:",
                                                                               choices = "")
                                                 )
                                          )
                         )
                       ),
                       fluidRow(
                         conditionalPanel(condition = "input.qtty_var>= 2", ns = ns,
                                          column(4,
                                                 selectInput(inputId = ns("var2"),
                                                             label = "Variable 2: ",
                                                             choices = "")
                                          ),
                                          column(4,
                                                 conditionalPanel(condition = "input.var2 != ''", ns = ns,
                                                                  radioButtons(inputId = ns("tipo_var2"),
                                                                               label = "Tipo Variable 2:",
                                                                               choices = "")
                                                 )
                                          )
                         )
                       ),
                       fluidRow(span(htmlOutput(ns("message01")), style="color:red")),
                       fluidRow(span(htmlOutput(ns("message02")), style="color:red")),
                       fluidRow(span(htmlOutput(ns("message03")), style="color:red")),
                       fluidRow(
                         conditionalPanel(condition = "input.qtty_var>= 2", ns = ns,
                                          conditionalPanel(condition = "input.var1 != ''", ns = ns,
                                          conditionalPanel(condition = "input.var2 != ''", ns = ns,                 
                                          conditionalPanel(condition = "input.tipo_var1 != ''", ns = ns,
                                                           conditionalPanel(condition = "input.tipo_var1 == input.tipo_var2", ns = ns,
                                                                  fluidRow(
                                                                    column(4,
                                                                           selectInput(inputId = ns("flip"),
                                                                                        label = "my Flip",
                                                                                        choices = "",
                                                                                        selected = "")
                                                                    )
                                                                  )
                                                           )
                                          )
                                          )
                                          )
                         )
                       ),
                       htmlOutput(ns("Zocalo")), br(), br(),
                       verbatimTextOutput(ns("MiTexto_BatallaNaval")),
      )
    )
    
  })
  
  
  # Final Return of the Modul!
  return(
    list(
      batalla_naval = batalla_naval,
      decimales = decimales,
      qtty_var = qtty_var
      )
  )
    
  # return(
  #   list(
  #     batalla_naval =  batalla_naval
  # 
  #   )
  # )
}


