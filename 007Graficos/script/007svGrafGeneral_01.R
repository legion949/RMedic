

# Generacion Dinamica de GRAFICOS



# Graficos para 1 Variable Categorica
output$menu_1q_graf <- renderUI ({
  
        if(paso_BASE(BASE_goku())) {
          
          
          tabsetPanel(id="menu_graf",
                      tabPanel("Barras", value=1),
                      tabPanel("Tortas", value=2)
          )
          
          
        } else return(NULL)
})



# Graficos para 1 Variable Cuantitativa
output$menu_1c_graf <- renderUI ({
  
    if(paso_BASE(BASE_goku())) {
          
          tabsetPanel(id="menu_graf",
                      tabPanel("Histograma", value=3),
                      tabPanel("Boxplot", value=4),
                      tabPanel("Dispersión", value=5),
                      tabPanel("Puntos", value=6),
                      tabPanel("Media y Desvío", value=7),
                      tabPanel("Media y Error Estándard", value=8)
          )
          
          
          
        } else return(NULL)
})



# Grafico para 2 Variables Cualitativas
output$menu_2q_graf <- renderUI ({
  
    if(paso_BASE(BASE_goku())) {
              
              tabsetPanel(id="menu_graf",
                          tabPanel("Barras", value=9)
              )
              
    } else return(NULL)
})



# Graficos para 2 Variables Cuantitativas
output$menu_2c_graf <- renderUI ({
  
    if(paso_BASE(BASE_goku())) {
          
          tabsetPanel(id="menu_graf",
                      tabPanel("XY", value=10)
          )
          
    } else return(NULL)
})




# Graficos para 2 Variables... una Q y una C
output$menu_qc_graf <- renderUI ({
  
    if(paso_BASE(BASE_goku())) {
          
          tabsetPanel(id="menu_graf",
                      tabPanel("Boxplot", value=11),
                      tabPanel("Media y Desvío", value=12),
                      tabPanel("Media y Error Estándard", value=13)
          )
      
    } else return(NULL)
})



# Cantidad de colores
output$MODcantidad_color <- renderUI({
  
  radioButtons("cantidad_color", "Coloración...",
               choices=c("Color Único" = "1",
                         "Un color por categoría" = "2")
               )
  
})


# Tipo coloracion
output$MODtipo_coloracion <- renderUI({
  
radioButtons("tipo_coloracion", "Tipo coloración...",
             choices=c("Estándard" = "Estandard",
                       "Paleta Completa" = "Paleta"),
             selected = "Estandard")
  
})


# Input de Colores
cols <- reactive({
  
  
  if (paso_BASE(BASE_goku())) {
    if(!is.null(input$menu_graf) && !is.null(input$tipo_coloracion)) {
      
      # Barras 1q
      if (input$menu_graf == 1) {
      
        if (!is.null(input$cantidad_color)) {
          
        niveles <- levels(as.factor(BASE_goku()[,1]))
        cantidad <- length(niveles)
        if (input$cantidad_color == "1") cantidad <- 1
        
        
        if (input$tipo_coloracion == "Estandard"){
          
        
        lapply(1:cantidad, function(i) {
          armado <- paste0("Color categoría '", niveles[i], "':")
          armado_colores <- c("Rojo" = "red",
                              "Azul" = "blue",
                              "Naranja" = "orange",
                              "Negro" = "black",
                              "Blanco"= "white")
          
          seleccionado <- i
          secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
          
          if (i > length(armado_colores)) seleccionado <- i - secuestrador 
          if (seleccionado == 0) seleccionado <- length(armado_colores)
          
          if (input$cantidad_color == "1") armado <- "Color..."
          div(
            selectInput(paste("col", i, sep="_"), armado,
                        armado_colores,
                        selected = armado_colores[seleccionado])
          )
        })
        
    
        }  else if (input$tipo_coloracion == "Paleta"){
          
          
          lapply(1:cantidad, function(i) {
            armado <- paste0("Color categoría '", niveles[i], "':")
            #   if (input$menu_graf != 2 && input$cantidad_color == "1") armado <- "Color mod..."
            
            armado_colores <- c("red", "blue", "orange", "black", "white")
            seleccionado <- i
            secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
            if (i > length(armado_colores)) seleccionado <- i - secuestrador 
            if (seleccionado == 0) seleccionado <- length(armado_colores)
            
            if (input$cantidad_color == "1") armado <- "Color..."
            div(
              colourInput(paste("col", i, sep="_"), armado, armado_colores[seleccionado]), br()
            )
          })
          
        }
         

      }
      
      } else  if (input$menu_graf == 2) {
        
      
          
          niveles <- levels(as.factor(BASE_goku()[,1]))
          cantidad <- length(niveles)
         
          if (input$tipo_coloracion == "Estandard"){
          lapply(1:cantidad, function(i) {
            armado <- paste0("Color categoría '", niveles[i], "':")
            armado_colores <- c("Rojo" = "red",
                                "Azul" = "blue",
                                "Naranja" = "orange",
                                "Negro" = "black",
                                "Blanco"= "white")
            
            seleccionado <- i
            secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
            
            if (i > length(armado_colores)) seleccionado <- i - secuestrador 
            if (seleccionado == 0) seleccionado <- length(armado_colores)
            
         #   if (input$cantidad_color == "1") armado <- "Color..."
            div(
              selectInput(paste("col", i, sep="_"), armado,
                          armado_colores,
                          selected = armado_colores[seleccionado])
            )
          })
          
          
            
          }  else if (input$tipo_coloracion == "Paleta"){
            
            
            lapply(1:cantidad, function(i) {
              armado <- paste0("Color categoría '", niveles[i], "':")
              #   if (input$menu_graf != 2 && input$cantidad_color == "1") armado <- "Color mod..."
              
              armado_colores <- c("red", "blue", "orange", "black", "white")
              seleccionado <- i
              secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
              if (i > length(armado_colores)) seleccionado <- i - secuestrador 
              if (seleccionado == 0) seleccionado <- length(armado_colores)
              
              div(
                colourInput(paste("col", i, sep="_"), armado, armado_colores[seleccionado]), br()
              )
            })
            
          }
          
          
        
        
      }  else  if ((input$menu_graf >= 3 && input$menu_graf <= 8) | input$menu_graf == 10)  {
        
        
        
        niveles <- levels(as.factor(BASE_goku()[,1]))
        cantidad <- 1
        
        if (input$tipo_coloracion == "Estandard"){
        lapply(1:cantidad, function(i) {
          armado <- "Color..."
          armado_colores <- c("Rojo" = "red",
                              "Azul" = "blue",
                              "Naranja" = "orange",
                              "Negro" = "black",
                              "Blanco"= "white")
          
          seleccionado <- i
          secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
          
          if (i > length(armado_colores)) seleccionado <- i - secuestrador 
          if (seleccionado == 0) seleccionado <- length(armado_colores)
          
          #   if (input$cantidad_color == "1") armado <- "Color..."
          div(
            selectInput(paste("col", i, sep="_"), armado,
                        armado_colores,
                        selected = armado_colores[seleccionado])
          )
        })
        }  else if (input$tipo_coloracion == "Paleta"){
          
          
          lapply(1:cantidad, function(i) {
            armado <- "Color..."
            #   if (input$menu_graf != 2 && input$cantidad_color == "1") armado <- "Color mod..."
            
            armado_colores <- c("red", "blue", "orange", "black", "white")
            seleccionado <- i
            secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
            if (i > length(armado_colores)) seleccionado <- i - secuestrador 
            if (seleccionado == 0) seleccionado <- length(armado_colores)
            
            div(
              colourpicker::colourInput(paste("col", i, sep="_"), armado, armado_colores[seleccionado]), br()
            )
          })
          
        }
        
        
        
        
        
        
      }  else  if (input$menu_graf == 9)  {
        
        
        
        niveles <- levels(as.factor(BASE_goku()[,1]))
        cantidad <- length(niveles)
        
        
        if (input$tipo_coloracion == "Estandard"){
        lapply(1:cantidad, function(i) {
          armado <- paste0("Color categoría '", niveles[i], "':")
          armado_colores <- c("Rojo" = "red",
                              "Azul" = "blue",
                              "Naranja" = "orange",
                              "Negro" = "black",
                              "Blanco"= "white")
          
          seleccionado <- i
          secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
          
          if (i > length(armado_colores)) seleccionado <- i - secuestrador 
          if (seleccionado == 0) seleccionado <- length(armado_colores)
          
          #   if (input$cantidad_color == "1") armado <- "Color..."
          div(
            selectInput(paste("col", i, sep="_"), armado,
                        armado_colores,
                        selected = armado_colores[seleccionado])
          )
        })
        
        }  else if (input$tipo_coloracion == "Paleta"){
          
          
          lapply(1:cantidad, function(i) {
            armado <- paste0("Color categoría '", niveles[i], "':")
            #   if (input$menu_graf != 2 && input$cantidad_color == "1") armado <- "Color mod..."
            
            armado_colores <- c("red", "blue", "orange", "black", "white")
            seleccionado <- i
            secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
            if (i > length(armado_colores)) seleccionado <- i - secuestrador 
            if (seleccionado == 0) seleccionado <- length(armado_colores)
            
            div(
              colourInput(paste("col", i, sep="_"), armado, armado_colores[seleccionado]), br()
            )
          })
          
        }
        
        
        
        
        
        
      } else  if (input$menu_graf >= 11 && input$menu_graf <= 13)  {
        
        
        
        niveles <- levels(as.factor(BASE_goku()[,2]))
        cantidad <- length(niveles)
        
        if (input$tipo_coloracion == "Estandard"){
        lapply(1:cantidad, function(i) {
          armado <- paste0("Color categoría '", niveles[i], "':")
          armado_colores <- c("Rojo" = "red",
                              "Azul" = "blue",
                              "Naranja" = "orange",
                              "Negro" = "black",
                              "Blanco"= "white")
          
          seleccionado <- i
          secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
          
          if (i > length(armado_colores)) seleccionado <- i - secuestrador 
          if (seleccionado == 0) seleccionado <- length(armado_colores)
          
          #   if (input$cantidad_color == "1") armado <- "Color..."
          div(
            selectInput(paste("col", i, sep="_"), armado,
                        armado_colores,
                        selected = armado_colores[seleccionado])
          )
        })
        
        
        }  else if (input$tipo_coloracion == "Paleta"){
          
          
          lapply(1:cantidad, function(i) {
            armado <- paste0("Color categoría '", niveles[i], "':")
            #   if (input$menu_graf != 2 && input$cantidad_color == "1") armado <- "Color mod..."
            
            armado_colores <- c("red", "blue", "orange", "black", "white")
            seleccionado <- i
            secuestrador <- (i%/%length(armado_colores))*length(armado_colores)
            if (i > length(armado_colores)) seleccionado <- i - secuestrador 
            if (seleccionado == 0) seleccionado <- length(armado_colores)
            
            div(
              colourInput(paste("col", i, sep="_"), armado, armado_colores[seleccionado]), br()
            )
          })
          
        }
        
        
        
        
      } 
   
      
   
      
    
    } else return(NULL)
  } else return(NULL)
})


# Salida de colores
output$MODcolor <- renderUI({cols()})


# Rejunte de colores seleccionados
mis_colores <- reactive({
  
  if (!is.null(input$menu_graf)) {
    
    if (input$menu_graf == 1) {
      
      if (!is.null(input$cantidad_color)) {
        niveles <- levels(as.factor(BASE_goku()[,1]))
        cantidad <- length(niveles)
        
        
        
        if (input$cantidad_color == "1") cantidad <- 1
      } else return(NULL)
    } 
    
    if (input$menu_graf == 2 | input$menu_graf == 9 | input$menu_graf >= 11) {
      
      
      niveles <- levels(as.factor(BASE_goku()[,1]))
      if(ncol(BASE_goku()) == 2 && input$menu_graf >= 11) niveles <- levels(as.factor(BASE_goku()[,2]))
      cantidad <- length(niveles)
      
    } 
    
    if ((input$menu_graf >= 3 && input$menu_graf <= 8) | input$menu_graf == 10 ) {
      
      cantidad <- 1
      
    } 
    
    
    
  
  rejunte <- rep(NA, cantidad)
  
#  lapply(1:cantidad, function(i) {
 for (i in 1:cantidad) {
   if (!is.null(input[[paste("col", i, sep="_")]]))  rejunte[i] <-   input[[paste("col", i, sep="_")]]
 # })
 }
  
  rejunte
  

  } else return(NULL)
})





# Detalle para grafico de Barras
output$MODy_1q_barras <- renderUI({

  selectInput(inputId="y_1q_barras",
              label="Graficar...",
              choices=c("Frecuencia Absoluta (FA)" = "FA",
                        "Frecuencia Relativa (FR)" = "FR", 
                        "Porcentajes (%)" = "%"),
              multiple=FALSE
  )
  
  
})

# Detalle para histograma (1c)
output$pack_1c_hist_basico <- renderUI({
  
  div(
                   selectInput(inputId="tipografico_1c_hist",
                               label="Graficar...",
                               choices=c("Frecuencia Absoluta (FA)" = "FA",
                                         "Frecuencia Relativa (FR)" = "FR"), 
                               multiple=FALSE),
                   selectInput(inputId="cerrado_1c_hist",
                               label="Intervalo Cerrado a la...",
                               choices=c("Derecha" = T,
                                         "Izquierda" = F), 
                               multiple=FALSE)
  ) # Fin div
  
})


# Detalle para histograma (1c)
output$tabla_hist <- renderTable({ 
  # Si ya hay base de datos...  
  if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    # y ya esta el menu cargado...
    if (!is.null(input$menu_graf)) {
      
      # Si es un Histograma
      if (input$menu_graf == 3) {
        
        categorias <-     graficar_1c(BASE_goku(), input_graf = "hist", input_color = mis_colores(),
                                      input_tipograf = input$tipografico_1c_hist, 
                                      input_cerrado = input$cerrado_1c_hist, input_save = T)
        

        vector_categorias <- as.character(as.vector(as.matrix(categorias)))
        
        nombre_original <- colnames(categorias)
        nombres <- levels(as.factor(as.vector(as.matrix(categorias)))) #c("[10,20]" , "(20,30]", "(30,40]", "(40,50]")
        
        minimos1 <- as.vector(unlist(strsplit(nombres, ","))[c(T,F)])
        minimos2 <- strsplit(minimos1, "")
        for (n in 1:length(minimos2)) minimos2[[n]] <- as.numeric(paste0(minimos2[[n]][2:length(minimos2[[n]])], collapse=""))
        minimos3 <- unlist(minimos2)
        orden_minimos3 <- order(minimos3, decreasing = T)
        orden_minimos4 <- orden_minimos3[length(orden_minimos3):1]
        
        factor_categorias <- factor(vector_categorias,levels(as.factor(vector_categorias))[orden_minimos4])
        factor_categorias <- data.frame(factor_categorias)
        colnames(factor_categorias) <- nombre_original
        

        df01(factor_categorias, decimales_goku())$df01$DF

      } else return(NULL)
    } else return(NULL)
  } else return(NULL)
  
  
  })


# Detalle para barras (2q)
output$pack_2q_barras_basico <- renderUI({
  
  div(
    radioButtons(inputId="legenda_2q_barras",
                 label="Leyenda...",
                 choices=c("SI" = T,
                           "NO" = F)
                 ),
    radioButtons(inputId="tipo_2q_barras",
                 label="Graficar...",
                 choices=c("Apilado" = F,
                           "Particionado" = T)
    ),
    selectInput(inputId="y_2q_barras",
                label="Graficar...",
                choices=c("Frecuencia Absoluta (FA)" = "FA",
                          "Frecuencia Relativa (FR)" = "FR", 
                          "Porcentajes (%)" = "%"),
                multiple=FALSE
    )
  ) # Fin div
  
})



# Menu Grafico
output$menu_graf <- renderUI({
  
  if (paso_BASE(BASE_goku())) {
    if(!is.null(tipo_var_goku())) {
      
      # Si es 1 sola variable  
      if(ncol(BASE_goku()) == 1) {
        
        # Si es categorica... o si no lo es...
        if(tipo_var_goku() == "Categórica")   uiOutput("menu_1q_graf")  else   uiOutput("menu_1c_graf")
        
      } else  if(ncol(BASE_goku()) == 2) {
        
        # Si las dos son numericas
        if(sum(tipo_var_goku() == "Categórica") == 0)   uiOutput("menu_2c_graf")  else   if(sum(tipo_var_goku() == "Categórica") == 1)   uiOutput("menu_qc_graf") else if(sum(tipo_var_goku() == "Categórica") == 2)  uiOutput("menu_2q_graf")  else return(NULL)
        
      }
    } else return(NULL)
  } else return(NULL)
})



# Grafico en si mismo
observe(
output$literal_graf <- renderPlot({

  # Si ya hay base de datos...  
  if(paso_BASE(BASE_goku())) {
    
    # y ya esta el menu cargado...
    if (!is.null(input$menu_graf)) {

  
      # 1) Barras
      if (input$menu_graf == 1) {
        if(!is.null(tabla_1q_df_goku()) && !is.null(input$y_1q_barras)) {
          
  graf_barras(input_tabla = tabla_1q_df_goku(), 
              input_y = input$y_1q_barras,
              input_color = mis_colores())
        } else return(NULL)
      
      # 2) Tortas    
      } else if (input$menu_graf == 2) {
        if(!is.null(tabla_1q_df_goku())) {  
        graf_tortas(input_tabla = tabla_1q_df_goku(), 
                    input_y = input$y_1q_barras,
                    input_color = mis_colores())
      
        }    else return(NULL)
      
      # Histograma  
      }    else if (input$menu_graf == 3) {
        
        graficar_1c(BASE_goku(), input_graf = "hist", input_color = mis_colores(),
                    input_tipograf = input$tipografico_1c_hist,
                    input_cerrado = input$cerrado_1c_hist)
       # hist(BASE_goku())
     #  hist(mtcars[,1])
      }   else if (input$menu_graf >= 4 && input$menu_graf <= 8) {
        
        detector <-  c(NA, NA, NA, "boxplot", "dispersion", "puntos", "mdes", "mye")
        este_detector <- detector[as.numeric(as.character(input$menu_graf))]
        
        graficar_1c(BASE_goku(), input_graf = este_detector, input_color = mis_colores())
        # hist(BASE_goku())
        #  hist(mtcars[,1])
      } else if (input$menu_graf == 9) {
        
        if(!is.null(pack_tabla_2q_df_goku()) && !is.null(input$y_2q_barras)) {
        
        # TABLA <- tabla2Q_graf_df()$FA
        # #TABLA <- DF02(mtcars[,c(2,8)], 2, T)$FA
        # TABLA <- TABLA[-nrow(TABLA), -ncol(TABLA)]
        # #barplot(TABLA)
        # # mi_y <- "FA"
        # # mi_color <- "red"
#        colores <- c("red", "blue", "green", "orange", "grey", "white", "black")
          
          opciones <- c("FA", "FR", "%")
          orden <- c(1, 3, 5) 
          dt <- opciones == input$y_2q_barras
          este_orden <- orden[dt]
         
        TABLA <- pack_tabla_2q_df_goku()[[1]][[este_orden]]
        if (este_orden == 4) {
          este_orden <- 3
          TABLA <- pack_tabla_2q_df_goku()[[1]][[este_orden]]
          TABLA <- TABLA*100
        }
        
        TABLA <- as.matrix(TABLA)
        barplot(TABLA, col=mis_colores(), ylab=input$y_2q_barras, beside=as.logical(input$tipo_2q_barras),
                legend=as.logical(input$legenda_2q_barras))
        # hist(BASE_goku())
        #  hist(mtcars[,1])
      } else return(NULL)
      } else if (input$menu_graf == 10) {

        graficar_2c(input_base=BASE_goku(),
                    input_graf="xy",
                    input_color=mis_colores())
      #  plot(BASE_goku(), col= mis_colores())
        
      }else if (input$menu_graf >= 11 && input$menu_graf <= 13) {
        
        detector <-  c(rep(NA, 10), "boxplot", "mdes", "mye")
        este_detector <- detector[as.numeric(as.character(input$menu_graf))]
        
        graficar_qc(input_base=BASE_goku(),
                    input_graf=este_detector,
                    input_color=mis_colores())

      }else return(NULL)
    } else return(NULL)
  } else return(NULL)
})
)




output$armado_opc_graf <- renderUI({
  
  if(!is.null(input$menu_graf)) {
    
    # Grafico de Barras
    if(input$menu_graf == 1) {
  div(
    fluidRow(
      column(6, 
  uiOutput("MODy_1q_barras"),
  uiOutput("MODcantidad_color"),
  uiOutput("MODtipo_coloracion"),
  uiOutput("MODcolor")
  ),
      column(6,plotOutput("literal_graf"))
  ),
 br(), br()
  )

    # Grafico de Tortas
    } else  if(input$menu_graf == 2) {
      div(
        fluidRow(
          column(6, 
        uiOutput("MODtipo_coloracion"),
        uiOutput("MODcolor")
        ),
        column(6, plotOutput("literal_graf"))
        )
      )
      
      # Histograma
    } else  if(input$menu_graf == 3) {
      div(
        fluidRow(
          column(6, 
                 uiOutput("pack_1c_hist_basico"),
                 uiOutput("MODtipo_coloracion"),
                 uiOutput("MODcolor")
          ),
          column(6, plotOutput("literal_graf"))
        ),
        h3("Distribución de Frecuencias del Histograma"),
        fluidRow(tableOutput("tabla_hist"))
      )
      
    # El resto de los graficos "1c" que no sean histogramas
    } else if(input$menu_graf >= 4 && input$menu_graf <= 8) {
      div(
        fluidRow(
          column(6, 
                 uiOutput("MODtipo_coloracion"),
                 uiOutput("MODcolor")
          ),
          column(6, plotOutput("literal_graf"))
        )
      )
      
    # Si es un grafico de barras "2q"
    } else if(input$menu_graf == 9) {
      div(
        fluidRow(
          column(6, 
                 uiOutput("pack_2q_barras_basico"),
                 uiOutput("MODtipo_coloracion"),
                 uiOutput("MODcolor")
          ),
          column(6, plotOutput("literal_graf"))
        )
      )
      
    }  else if(input$menu_graf == 10) {
      div(
        fluidRow(
          column(6, 
                 uiOutput("MODtipo_coloracion"),
                 uiOutput("MODcolor")
          ),
          column(6, plotOutput("literal_graf"))
        )
      )
      
    } else if(input$menu_graf >= 11 && input$menu_graf <= 13) {
      div(
        fluidRow(
          column(6, 
                 uiOutput("MODtipo_coloracion"),
                 uiOutput("MODcolor")
          ),
          column(6, plotOutput("literal_graf"))
        )
      )
      
    }else return(NULL) 
      
  } else return(NULL)
})


# Planeta Grafico
output$planeta_graf <- renderUI({
  
  if (paso_BASE(BASE_goku())) {

   
      #barplot(table(BASE_goku()[,1]), col = mis_colores()) 
      
   
        
    div(
        uiOutput("menu_graf"),
        uiOutput("armado_opc_graf"),
        uiOutput("planeta_tablas")
    )
  
  } else return(NULL)
})













