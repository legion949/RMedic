source("lib.R")

function(input, output, session) {
  

  RMedic_general <- reactiveVal(T)
  reseteo_conteo <- reactiveVal(0)
  
  # Update - Todo OK!
  {
    
  # Reseteo por tipo de archivo...
  observeEvent(input$FileTypePicker,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
    
  })
  
  
  # Reseteo por archivo excel seleccionado...
  observeEvent(input$xls_file,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
  })
  
  # Reseteo por archivo csv seleccionado...
  observeEvent(input$csv_file,{
    
    # Reset selected step
  #  # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
  })
  
  # Reseteo por archivo csv seleccionado...
  observeEvent(input$ejemplo_file,{
    
    # Reset selected step
   # # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
  })
  
  # Reseteo por especificaciones del criterio de inclusion estadistico
  observeEvent(input$cie_especificaciones,{
    
    # Reset selected step
    if(input$cie_especificaciones == 2) RMedic_general(F) else 
      if(input$cie_especificaciones == 1) RMedic_general(T)
  })
  
  # Reseteo por cie_columna
  observeEvent(input$cie_columna,{
    
    # Reset selected step
    # RMedic_general(F)
    
  })
  
  # Reseteo por cie_categoria
  observeEvent(input$cie_categoria,{
    
    # Reset selected step
    # RMedic_general(F)
    
  })
  
  ####################################################################
  
  observeEvent(input$reset,{
    
    # Reset selected step
    # RMedic_general(F)
    
    reseteo_conteo(reseteo_conteo()+1)
    
    })
  
  observeEvent(input$cargar,{
    
    # Reset selected step
    RMedic_general(T)
    
  })
  
 #####################################################################
  
  }
  ###################################
  
  
  # Control (Que sea un archivo valido)
  Control01 <- reactive({ 
    
    if(!is.null(input$FileTypePicker)){  
      
      if(input$FileTypePicker == "Excel") { 
        
        if (!is.null(input$xls_file)) {
          
          the_file <-  input$xls_file[[1]]
          
          
          correct_format <- c(".xls$", ".xlsx$")
          dt_format <- rep(NA, length(correct_format))
          dt_format[1] <- grepl(correct_format[1], the_file)
          dt_format[2] <- grepl(correct_format[2], the_file)
          ok_format <- sum(dt_format) > 0
          
          frase_yes <- ""
          frase_no  <- "Indicaste que subirías un archivo tipo Excel pero 
                        seleccionaste un archivo de otro tipo."
          
          if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
          
          my_exit <- list(ok_format, frase_alert)
          
          
        } else return(NULL)
        
        
      } else 
        if(input$FileTypePicker == "CSV") { 
          
          if (!is.null(input$csv_file)) {
            
            the_file <-  input$csv_file[[1]]
            
            correct_format <- c(".csv$")
            dt_format <- rep(NA, length(correct_format))
            dt_format[1] <- grepl(correct_format[1], the_file)
            ok_format <- sum(dt_format) > 0
            
            frase_yes <- ""
            frase_no  <- "Indicaste que subirías un archivo CSV pero 
                        seleccionaste un archivo de otro tipo."
            
            if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
            
            my_exit <- list(ok_format, frase_alert)
            
            
          } else return(NULL)
          
          
          
          
          
        } else 
          if(input$FileTypePicker == "Ejemplos") { 
            
            if (!is.null(input$ejemplo_file)) {
              
              ok_format <- TRUE
              frase_yes <- ""
              frase_no  <- ""
              
              if(ok_format) frase_alert <- frase_yes else frase_alert <- frase_no
              
              my_exit <- list(ok_format, frase_alert)
              
              
            } else return(NULL)
            
            
            
            
            
          } else return(NULL)
      
      return(my_exit)
      
    } 
  })
  
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

  # Variable criterio de inclusion
  observeEvent(reseteo_conteo(),{
    
    # Reset selected step
    # # RMedic_general(F)
    
    updateRadioButtons(session, inputId = "cie_especificaciones", 
                 label = "Especificaciones", 
                 choices = c("Todas las filas" = 1, 
                             "Criterio de Inclusión Estadístico" = 2))
    
    if(!is.null(Base01())) {
    updateSelectInput(session, inputId = "cie_columna",
                      label = "Criterio de Inclusión Estadístico (Solo una variable):",
                      choices = colnames(Base01()[[1]]))
    }
    
   
  })
  
  # Categoria de inclusion
  observeEvent(input$cie_columna,{
  if(!is.null(Base01())) if(!is.null(input$cie_columna)) {
  updateSelectInput(session, inputId = "cie_categoria",
                    label = "Categoria de Inclusión (Solo una categoría):",
                    choices = levels(as.factor(Base01()[[1]][,input$cie_columna]))
                    )
  }
  })
  

  
  
  ###################################################
  
  # Control02 - La base tiene al menos una columna...
  Control02 <- reactive({ 
    
    if(!is.null(Base01())){
      
         
            dt_ok_01 <- ncol(Base01()[[1]]) > 0
            dt_ok_02 <- nrow(Base01()[[1]]) > 0
            dt_ok_03 <- sum(dt_ok_01, dt_ok_02) == 0
            dt_ok_04 <- sum(dt_ok_01, dt_ok_02) == 2
            
            frase_FALSE_01 <- "La base de datos que has seleccionado no posee
                               columnas con datos."
            
            frase_FALSE_02 <- "La base de datos que has seleccionado no posee
                               filas con datos."
            
            frase_FALSE_03 <- "La base de datos que has seleccionado es un archivo
                                completamente vacío. Sin datos."
            
            frase_TRUE <- ""
        
            # Frase
            if(dt_ok_04) frase_salida <- frase_TRUE else
              if(dt_ok_03) frase_salida <- frase_FALSE_03 else
                if(dt_ok_02) frase_salida <- frase_FALSE_02 else
                  if(dt_ok_01) frase_salida <- frase_FALSE_01 
                    
            # dt_ok
            if(dt_ok_04) dt_ok <- dt_ok_04 else
              if(dt_ok_03) dt_ok <- dt_ok_03 else
                if(dt_ok_02) dt_ok <- dt_ok_02 else
                  if(dt_ok_01) dt_ok <- dt_ok_01 
                            
            my_exit <- list(dt_ok, frase_salida)
            return(my_exit)
            
    
  } else return(NULL)
})
  
  # Control 03 (Que sea la variable CIE tiene al menos una categoria)
  Control03 <- reactive({ 
    
    if(!is.null(Base01())){
      
      if(!is.null(input$cie_especificaciones)){
        
        if(input$cie_especificaciones == 2){
          
          if( Control02()[[1]]) {
          
          if(!is.null(input$cie_columna)){
            
            mis_categorias <- levels(as.factor(Base01()[[1]][,input$cie_columna]))
            
            dt_ok <- length(mis_categorias) > 0
            
            frase_TRUE <- ""
            frase_FALSE <- "La variable que cumplirá el rol de criterio de inclusión
                            estadístico debe tener al menos una categoría."
            
            if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
            
            my_exit <- list(dt_ok, frase_salida)
            return(my_exit)
            
          } else return(NULL)
          } else return(NULL)
          
        } else return(NULL)
        
      } else return(NULL)
      
    } else return(NULL)
    
  })
  

  
  # Control 04 (Que haya subido la base antes de elegir un CIE)
  Control04 <- reactive({ 
    
    dt_ok <- TRUE

    frase_TRUE <- ""
    frase_FALSE <- "Para poder seleccionar un Criterio de Inclusión Estadística y una Categoría de Inclusión primero debe subir una base de datos."
    

    if (is.null(Base01()) && input$cie_especificaciones == 2) dt_ok <- FALSE          
   

    if (dt_ok) frase_salida <- frase_TRUE else frase_salida <- frase_FALSE
    
   # cat("frase04: ", dt_ok, "\n")
    
    my_exit <- list(dt_ok, frase_salida)
    return(my_exit)
    
        
    
    
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
  
  
  ##########################################################
  
  output$TextBase_InfoDataSet <- renderUI({
    
    if (!is.null(Base01())) {
      t1 <- paste0("<b>Base:</b> ", MyFileName()) 
      t2 <- paste0("<b>Variables (Columnas):</b> ", ncol(Base01()[[1]]) , " variables",
                   " - Desde la columna '", num2let(1), "' hasta la columna '",
                   num2let(as.numeric(as.character(ncol(Base01()[[1]])))), "'.")
      t3 <- paste0("<b>Unidades (Filas o repeticiones):</b> ", nrow(Base01()[[1]]),
                   " unidades.")

      mi_salida <- HTML(paste(t1, t2, t3, sep = '<br/>'))
      
      if (!is.null(Base02())) {
        
        t4 <- paste0("<b>Unidades (Filas o repeticiones) totales:</b> ", 
                     nrow(Base01()[[1]]), " unidades.")
        t5 <- paste0("<b>Especificaciones de inclusión:</b> ",
                     "solo las unidades con el detalle de '_detalle_' en la
                     variable '_nombre_variable_' serán considerados.")
        t6 <- paste0("<b>Unidades (Filas o repeticiones) seleccionadas:</b> ", 
                     nrow(Base02()[[1]]), " unidades.")
        
        mi_salida <- HTML(paste(t1, t2, t4, '<br/>', t5, t6, sep = '<br/>'))
        
      }
      
    #  HTML(paste(t1, t2, sep = '<br/>'))
      mi_salida
    } else return(NULL)
  })
 

  output$TextBase_Intro <- renderText({
    if (!is.null(Base01())) {
      "Visualización de la Base de Datos"
    } else return(NULL)
  })
  
  Base02 <- reactive({
    
    if(!is.null(Base01())){
      
      if(!is.null(Control02())){
        
        if(!is.null(Control03())){
            
            mi_filtro <- Base01()[[1]][,input$cie_columna]
            dt_filtro <- mi_filtro == input$cie_categoria
            base2 <- Base01()[[1]][dt_filtro, ]
            base2
            
            } else return(NULL)
            
          } else return(NULL)
    } else return(NULL)   
     
    
    
  })
  
  
  
  
  
  output$BASE_SALIDA <- renderDataTable({
    
    if (!is.null(Base01())) {
      
      cat("RMedic_general(): ", RMedic_general(), "\n")
      if(RMedic_general()){
        
        mi_base <- Base01()[[1]]
        if(input$cie_especificaciones == 2) mi_base <- Base02()
        
        
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
        datatable(mi_base, rownames = F,  options = list(
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
  
  
  
  
  
}