# Seccion 03 - Base de Datos
{
  ###
  
  MyFileName <- reactive({
    
    
    
    if(!is.null(input$FileTypePicker)){
      
      if(!is.null(Control01())) {
        if(Control01()[[1]]) {
          
          if(input$FileTypePicker == "Excel") { 
            
            if (!is.null(input$xls_file)) my_file <- input$xls_file[[1]] else return(NULL)
            
            
          } else 
            if(input$FileTypePicker == "CSV") { 
              
              if (!is.null(input$csv_file)) my_file <- input$csv_file[[1]] else return(NULL)
              
              
            } else 
              if(input$FileTypePicker == "Ejemplos") { 
                
                
                if (!is.null(input$ejemplo_file)) my_file <- input$ejemplo_file else return(NULL)
                
                
              } else return(NULL)
          
          
          # Return of the king...
          return(my_file)
          
        } else return(NULL)  
      } else return(NULL)
    } else return(NULL)
    
    
    
  })
  
  
  # Carga de la Toda la base de datos
  Base01 <- reactive({
    
    if(!is.null(input$FileTypePicker)){
      
      if(!is.null(Control01())) {
        if(Control01()[[1]]) {
          
          if(input$FileTypePicker == "Excel") { 
            
            if (!is.null(input$xls_file)) {
              
              # Detalles varios...
              inFile <- input$xls_file
              
              # La direccion del archivo...
              temporal_file <- inFile$datapath
              
              # cat("inFile: ", inFile, "\n" )
              # cat("archivo: ", archivo, "\n" )
              # cat("archivo2: ", input$xls_file[[1]], "\n" )
              # cat("temporal_file: ", temporal_file, "\n" )
              
              # 1) DataSet
              library(readxl)
              DataSet <- as.data.frame(read_excel(temporal_file, col_names= TRUE, sheet = 1, trim_ws = FALSE))
              
            } else return(NULL)
            
          } else
            if(input$FileTypePicker == "CSV") { 
              
              if (!is.null(input$csv_file)) {
                
                # Detalles varios de direccion
                inFile <- input$csv_file
                
                # La carga de datos formato CSV
                DataSet <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, quote=input$quote)
                
              } else return(NULL)
              
            } else 
              if(input$FileTypePicker == "Ejemplos") { 
                
                
                if (!is.null(input$ejemplo_file)) {
                  
                  
                  # cat("ejemplo_file: ", input$ejemplo_file, "\n")
                  # La carga de datos formato CSV
                  DataSet <- eval(parse(text = input$ejemplo_file))
                  
                }  else return(NULL)
              }  else return(NULL)
          
          # Salida
          my_exit <- list(DataSet)
          return(my_exit)
          
        } else return(NULL)
        
      } else return(NULL)
      
    } else return(NULL)
    
    
  })
  
  Base02 <- reactive({
    
    if(!is.null(Base01())){
      
      if(!is.null(Control02())){
        
        if(!is.null(Control03())){
          
          mi_filtro <- Base01()[[1]][,input$cie_columna]
          dt_filtro <- mi_filtro == input$cie_categoria
          base2 <- Base01()[[1]][dt_filtro, ]
          
          my_exit <- list(base2)
          return(my_exit)
          
        } else return(NULL)
        
      } else return(NULL)
    } else return(NULL)   
    
    
    
  })
  
  
  BaseSalida <- reactive({
    
    if (!is.null(Base01())) {
      
      if(input$cie_especificaciones == 1) Base01() else
        if((input$cie_especificaciones == 2) && (!is.null(Base02()))) Base02()
      
    } else return(NULL)    
  })
  
  # Base de datos seleccionada
  Base_Planeta <- reactive({
    
    if (status_BaseSalida()) {
      if (!is.null(var_planeta())) {
        
        BaseSalida()[[1]][var_planeta()]
        
      } else return(NULL)
    } else return(NULL)
  })   
  
  
  output$TextBase_Alert01 <- renderText({
    if (!is.null(Control01())) {
      # Tab01_Base()[[3]]
      Control01()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  
  output$TextBase_Alert02 <- renderText({
    if (!is.null(Control02())) {
      # Tab01_Base()[[3]]
      Control02()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  output$TextBase_Alert03 <- renderText({
    if (!is.null(Control03())) {
      # Tab01_Base()[[3]]
      Control03()[[2]]
      #  "AVER"
    } else return(NULL)
  })
  
  output$TextBase_Alert04 <- renderText({
    if (!is.null(Control04())) {
      
      Control04()[[2]]
      
    } else return(NULL)
  })
  
  output$TextBase_Alert05 <- renderText({
    if (!is.null(Control05())) {
      
      Control05()[[2]]
      
    } else return(NULL)
  })
  
  
  output$TextBase_InfoDataSet <- renderUI({
    
    texto_salida <- c()
    
    if (!is.null(Base01())) {
      if(RMedic_general()){
        
        
        texto_completo01 <- c("<b>Base:</b> _mi_archivo_ <br/>
                           <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                           <b>Unidades (Filas o repeticiones):</b> _nrowBase01_ unidades.<br/>")
        
        texto_salida <- texto_completo01
        
        
        
        if (!is.null(Base02()) && !is.null(input$cie_categoria)) {
          
          texto_completo02 <- c("
                            <b>Base:</b> _mi_archivo_ <br/>
                            <b>Variables (Columnas):</b> _ncolBase01_ variables.<br/>
                            <b>Unidades totales (Filas totales o repeticiones totales):</b> _nrowBase01_ unidades.<br/>
                            <br/>
                            <b>Criterio de Inclusión Estadístico (CIE):</b> _mi_CIE_.<br/>
                            <b>Categoría de Inclusión:</b> la categoría seleccionada es '_mi_categoria_'.<br/>
                            <b>Unidades seleccionadas (Filas seleccionadas o repeticiones seleccionadas):</b> _nrowBase02_ de _nrowBase01_ unidades.<br/>
                              ")
          
          agregado01 <- c("
                        <b>Nota Importante:</b> aplicando el criterio de inclusión estadístico sobre las
                        _nrowBase01_ unidades totales son seleccionadas e ingresan efectivamente a
                        tablas, gráficos y test estadístisticos solo _nrowBase02_ unidades. Estas _nrowBase02_ unidades
                        presentan la categoría '_mi_categoria_' en el CIE _mi_CIE_.
                        ")
          
          agregado02 <- c("
                        <b>Nota Importante:</b> Todas las unidades responden a la misma
                        categoría de inclusión. Trabajar con este criterio de inclusión
                        o directamente con el total de la base de datos, es lo mismo.
                        ")
          
          texto_salida <- texto_completo02
          
          if(nrow(Base01()[[1]]) != nrow(Base02()[[1]])) texto_salida <- paste0(texto_salida, "<br/>", agregado01) else texto_salida <- paste0(texto_salida, "<br/>", agregado02)
          
          
          
          dt_col <- colnames(Base02()[[1]]) == input$cie_columna
          #   cat(input$cie_categoria, "\n")
          #   cat(dt_col, "\n")
          orden_col <- c(1:length(dt_col))[dt_col]
          armado <- paste0("Variable '", input$cie_columna, "' - Letra Columna '",
                           num2let(orden_col), "'")
          
          #  armado <- "AVERRERRRRR"
          texto_salida <- gsub("_mi_CIE_", armado, texto_salida)
          
          texto_salida <- gsub("_mi_categoria_", input$cie_categoria, texto_salida)
          texto_salida <- gsub("_nrowBase02_", nrow(Base02()[[1]]), texto_salida)
          
        }
        
        #  HTML(paste(t1, t2, sep = '<br/>'))
        
        
        texto_salida <- gsub("_mi_archivo_", MyFileName(),texto_salida)
        texto_salida <- gsub("_ncolBase01_", ncol(Base01()[[1]]),texto_salida)
        texto_salida <- gsub("_nrowBase01_", nrow(Base01()[[1]]),texto_salida)
        
        mi_salida <- HTML(texto_salida)
        mi_salida
      } else return(NULL)
    } else return(NULL)
  })
  
  
  output$TextBase_Intro <- renderText({
    if (!is.null(Base01())) {
      if(RMedic_general()){
        "Visualización de la Base de Datos"
      } else return(NULL)
    } else return(NULL)
  })
  
  
  
  output$BASE_SALIDA <- renderDataTable({
    
    if (status_BaseSalida()) {
      
      #cat("RMedic_general(): ", RMedic_general(), "\n")
      if(RMedic_general()){
        
        
        
        #   rownames(mi_base) <- rep("", nrow(mi_base))
        # mi_base
        #  cantidad_columnas <-  ncol(Tab01_Base()[[2]][3])
        # cantidad_columnas <-  ncol(mi_base)
        # 
        # sketch = htmltools::withTags(table(
        #   class = 'display',
        #   thead(
        #     tr(
        #        lapply(num2let(c(1:(cantidad_columnas)), th, colspan = 1)
        #      ),
        #      tr(
        #        lapply(c(colnames(mi_base)), th)
        #      )
        #    )
        #  )
        # ))
        
        
        
        #container = sketch,
        datatable(BaseSalida()[[1]], rownames = F,  options = list(
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#000'});",
            "}"), language = list(
              search = "Búsqueda:",
              lengthMenu = "Mostrar _MENU_ registros",
              info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
              infoFiltered = "(filtrados de un total de _MAX_ registros)",
              paginate = list(previous =    "Anterior", `next` = "Siguiente")
            )
        ))
        
        
        # sketch = htmltools::withTags(table(
        #   tableHeader(iris),
        #   tableFooter(iris)
        # ))
        # datatable(
        #   head(iris, 10),
        #   container = sketch, options = list(pageLength = 5, dom = 'tip'), rownames = FALSE
        # )
      } else return(NULL)
    } else return(NULL)
  })
  
  
  menuBASE <- reactive({
    
    
    tabs <- list()
    
    tabs[[1]] <-    tabPanel(title = "Base de Datos", 
                             icon = icon("user-md"), 
                             value = 1,
                             br(),
                             fluidRow(
                               #   column(4, OpcionesDeCarga),
                               column(8, 
                                      h3(textOutput("TextBase_Alert01")), 
                                      h3(textOutput("TextBase_Alert02")), 
                                      h3(textOutput("TextBase_Alert03")),
                                      h3(textOutput("TextBase_Alert04")),
                                      h3(htmlOutput("TextBase_Alert05")),
                                      htmlOutput("TextBase_InfoDataSet"), br(),
                                      h3(textOutput("TextBase_Intro")),
                                      dataTableOutput('BASE_SALIDA')
                               ),
                               
                             ),
                             br(), br()
    )
    
    tabs
    
  })
  
  ###
} # End Seccion 03
#############################################
