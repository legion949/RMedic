## Segmento del UI
selectorUI <- function(id) {
  ns <- NS(id)
  
  div(
  fluidRow(
    column(4, 
            selectInput(inputId = ns("orden"), 
                        label = "Orden: ",
                        choices = c("Orden Original", "Albético Creciente", "Albatético Decreciente")
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
                        min = 1,
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
    fluidRow(span(textOutput(ns("message01")), style="color:red")),
    fluidRow(span(textOutput(ns("message02")), style="color:red")),
    fluidRow(
      conditionalPanel(condition = "input.qtty_var>= 2", ns = ns,
      conditionalPanel(condition = "input.tipo_var1 != ''", ns = ns,
      conditionalPanel(condition = "input.tipo_var1 == input.tipo_var2", ns = ns,
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
}




## Segmento del server
selectorSERVER <- function(input, output, session, base) {
  # return(
  #   list(
  #     frec_input = reactive({ input$frec_input }),
  #     max_input = reactive({ input$max_input })
  #   )
  # )
  
  
  
  
  ##################################
  observe({
    
     freezeReactiveValue(input, "var1")
    updateSelectInput(session, 
                      inputId = "var1",
                      label = "Variable 1: ",
                      choices = c("Seleccione una... " = "", colnames(base()))
    )
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
                      choices = c("Seleccione una... " = "", colnames(base()))
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
  
  
  
  batalla_naval <- reactive({
    
    if(is.null(input$qtty_var)) return(NULL)
    if(input$qtty_var == '') return(NULL)
    
    variables <- c()
    tipo_variables <- c()
    numero_tipo <- c()
    caso_tipo_variables <- c() # Es un valor unico
    verificacion_interna <- c()
    verificacion_general <- c() # Es un valor unico
    
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
      
      # Verificacion interna
      dt_numerica1_interna <- is.numeric(base()[,input$var1])
      dt_numerica1_externa <- tipo_variables[1] == "Numeric"
      dt_numerica2_interna <- is.numeric(base()[,input$var2])
      dt_numerica2_externa <- tipo_variables[2] == "Numeric"
      
      verificacion_interna[1] <- TRUE
      verificacion_interna[2] <- TRUE
      if(!dt_numerica1_interna) if(dt_numerica1_externa) verificacion_interna[1] <- FALSE
      if(!dt_numerica2_interna) if(dt_numerica2_externa) verificacion_interna[2] <- FALSE
     
     
      
      
      # Rotacion!      # Numerica, #categorica
      # Cuando hay una variable de cada tipo, se ordenada todo
      # para que la primera variable sea la categorica y la 2da la numerica
      # Es una forma de trabajo estandard.
      modelo_cambio <- c(10,             1)

      if (identical(numero_tipo, modelo_cambio)){
        variables <- variables[c(2,1)]
        numero_tipo <- numero_tipo[c(2,1)]
        tipo_variables <- tipo_variables[c(2,1)]
        verificacion_interna <- verificacion_interna[c(2,1)]
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
        
      }
      }
      
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
    my_exit <- list(variables, numero_tipo, tipo_variables, caso_tipo_variables,
                    verificacion_interna, verificacion_general)
    
    return(my_exit)
    
   
  })

  # MensajesParaBase <- reantive({
  #   if(is.null(input$qtty_var)) return(NULL)
  #   if(input$qtty_var == '') return(NULL)
  #   
  #   mensajes <- rep(NA, 2)
  #   
  #   if(batalla_naval()$verificacion_general) mensajes[1] <- 
  # })
  
  output$message01 <- renderText({"This is some red text"})

  output$message02 <- renderText({"This is some red text"})
  
  return(
    list(
      batalla_naval =  batalla_naval
      
    )
  )
}


