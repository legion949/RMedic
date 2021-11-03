


Graficos1Q_03_Tortas_UI <- function(id) {
  
  ns <- NS(id)
  
  div(
    bsButton(ns("goButtonMaster2"), "Mostrar/Ocultar opciones gráficas", type = "toggle", value = TRUE,
             icon("bars"), style = "primary", size = "large"
    ),br(),br(),
    h2("Gráfico de Tortas"),
    
    fluidRow(
      column(4 ,   div(id = "James10", uiOutput(ns("controlador_general_1q_torta")))
    ),
    column(8,
    plotOutput(ns("grafico_tortas_1q"))
  )
    )
  )
  

  
  
  
}






## Segmento del server
Graficos1Q_03_Tortas_SERVER <- function(input, output, session, 
                                        minibase, 
                                        batalla_naval,
                                        decimales) {
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Caso 1: 1Q
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  # Tabla de Frecuencias RMedic 1Q
  DF_interna <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  # Cantidad de categorias
  cantidad_categorias <- reactive({
    if(is.null(DF_interna())) return(NULL)
    
    return(nrow(DF_interna()))
    
  })
  
  # Corona para la torta
  corona <- reactive({
    
    la_corona <- list()
    
    la_suma <- sum(as.numeric(as.character(input$porciones_coronadas)))
    
    cat("length(input$porciones_coronadas): ", length(input$porciones_coronadas), "\n")
    cat("la_suma: ", la_suma, "\n")
    
    la_nada <- rep("", cantidad_categorias())
    la_categoria <- as.character(as.vector(DF_interna()[,1]))
    la_fa <- as.character(as.vector(DF_interna()[,2]))
    la_porcentaje <- as.character(as.vector(DF_interna()[,6]))
    la_fusion <- as.character(as.vector(DF_interna()[,7]))
      
    if(is.null(input$porciones_coronadas)) {
      la_corona[[1]] <- la_categoria
    
    } else
      if(length(input$porciones_coronadas) == 0) {
        
        la_corona[[1]] <- la_nada
      
      } else 
        if(la_suma == 0) {
          
          la_corona[[1]] <- la_nada
          
        } else
        if(la_suma == 1) {
            
            la_corona[[1]] <-  paste0(la_categoria, " (", la_fa, ")")
          } else
            if(la_suma == 10) {
              
              la_corona[[1]] <- paste0(la_categoria, " (", la_porcentaje, ")")
            } else
              if(la_suma == 11) {
                
                la_corona[[1]] <- paste0(la_categoria, " (", la_fa, " ; ",la_porcentaje ,")")
              } 
    
    return(la_corona)
  }) 
  
  
  observeEvent(input$goButtonMaster2, {
    
    shinyjs::toggle("James10", asis = T, anim = TRUE, animType = "fade")
    
  })
  
  # Colores seleccionados
  colores_seleccionados <-  eventReactive(input$goButton4,({
  #  if (is.null(DF_interna())) return(NULL)
    
   
    cantidad <- cantidad_categorias()
    mis_colores <- rep(NA, cantidad)
    
    for(i in 1:cantidad){ 
      nombre_input <- paste("col", i, sep="_")
      #   cat("nombre_input: ", nombre_input, "\n" )
      #   cat("input[[nombre_input]]: ", input[[nombre_input]], "\n" )
      if(is.null(input[[nombre_input]])) return(NULL)
      
      mis_colores[i] <- input[[nombre_input]]
      
    }
    mis_colores
    
    #   cat("mis_colores:", mis_colores, "\n")
   # return(mis_colores)
  }),ignoreNULL  = FALSE)
  

  
  # Salida de colores
  output$MODcolor <- renderUI({
    
    if (is.null(DF_interna())) return(NULL)

    
    
    
    cantidad <- c()
    cantidad <- nrow(DF_interna())
    label_armado <- paste0("Categoría '", as.vector(DF_interna()[,1]), "'")
        
 
    
     colores_internos <- rainbow(cantidad)
    # if(cantidad == 1) colores_internos <- "#FF0000"
    # colores_internos <- rep("#FF0000", cantidad)
    
    lapply(1:cantidad, function(i) {
      
      nombre_input <- paste("col", i, sep="_")
      div(
        colourpicker::colourInput(inputId = ns(nombre_input),
                                  label = label_armado[i], 
                                  value = colores_internos[i]), br()
      )
      
    })
    
  })
  
  
  
  output$controlador_general_1q_torta <- renderUI({
    
    div(
      checkboxGroupInput(inputId = ns("porciones_coronadas"),
                   label = h3("Agregar al gráfico de tortas..."),
                   choices = c("Frecuencias Absolutas" = 1,
                               "Porcentaje " = 10
                               ),
                   width = "100%"
      ),
      br(),

      uiOutput(ns("MODcolor")),
      br(),
      bsButton(ns("goButton4"), "Aplicar cambio de color", type = "toggle", value = TRUE,
               icon("bars"), style = "primary", size = "large"
      )
    )
    
    
    
    
    
  })
  
  

  output$grafico_tortas_1q <- renderPlot({
    
    # pie(table(mtcars[,2]))
    valores <- as.numeric(as.character(DF_interna()[,2]))
    
    armado1 <- DF_interna()[,1]
    armado2 <- corona()[[1]]
    armado3 <- paste0(armado1, armado2)
    
    if(is.null(colores_seleccionados())) mis_colores <- rainbow(cantidad_categorias()) else
      mis_colores <- colores_seleccionados()
    
   # names(valores) <- armado3
    
    pie(valores, label = armado2, col = mis_colores, radius = 1)
  })
  
}