source("lib.R")

function(input, output, session) {
  

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
                        seleccionaste un archivo de otro tipo. Cambia de archivo!"
          
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
                        seleccionaste un archivo de otro tipo. Cambia de archivo!"
              
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
  
  
  Tab01_Base <- reactive({
    
    # 1) DataSet
    # 2) InfoDataSet
    
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
          
          # 3) InfoDataSetFilas y columnas
          my_columns <- c("FileName", "Rows", "Cols")
          InfoDataSet <- rep(NA, length(my_columns))
          names(InfoDataSet) <- my_columns
          InfoDataSet[1] <- input$xls_file[[1]]
          InfoDataSet[2] <- nrow(DataSet)
          InfoDataSet[3] <- ncol(DataSet)
          
          
             
          # My Exit
          my_exit <- list(InfoDataSet, DataSet)
          

          
        } else return(NULL)
        
        
        } else 
          if(input$FileTypePicker == "CSV") { 
            
            if (!is.null(input$csv_file)) {
              
              # Detalles varios de direccion
              inFile <- input$csv_file
              
              # Si no hay archivo
              if (is.null(inFile))
                return(NULL)
              
              # La carga de datos formato CSV
              DataSet <- read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, quote=input$quote)
              
              # 3) InfoDataSetFilas y columnas
              my_columns <- c("FileName", "Rows", "Cols")
              InfoDataSet <- rep(NA, length(my_columns))
              names(InfoDataSet) <- my_columns
              InfoDataSet[1] <- input$csv_file[[1]]
              InfoDataSet[2] <- nrow(DataSet)
              InfoDataSet[3] <- ncol(DataSet)
              
              
                
              my_exit <- list(InfoDataSet, DataSet)
              

              
            } else return(NULL)
            
            
          } else 
            if(input$FileTypePicker == "Ejemplos") { 
              
              
              if (!is.null(input$ejemplo_file)) {
                
              
               # cat("ejemplo_file: ", input$ejemplo_file, "\n")
                # La carga de datos formato CSV
                DataSet <- eval(parse(text = input$ejemplo_file))
               # DataSet <- mtcars
                # 3) InfoDataSetFilas y columnas
                my_columns <- c("FileName", "Rows", "Cols")
                InfoDataSet <- rep(NA, length(my_columns))
                names(InfoDataSet) <- my_columns
                InfoDataSet[1] <- input$ejemplo_file
                InfoDataSet[2] <- nrow(DataSet)
                InfoDataSet[3] <- ncol(DataSet)
                
             
                
                
                my_exit <- list(InfoDataSet, DataSet)
                
                
                
              } else return(NULL)
              
              
            } else return(NULL)
        
      
      # Return of the king...
      return(my_exit)
      
        } else return(NULL)  
    } else return(NULL)
    } else return(NULL)
    
    # Si ha puesto una direccion de archivo...
 
    
    
  })
  
  
  output$BASE_SALIDA <- renderDataTable({
    
    if (!is.null(Tab01_Base())) {
      
    mi_base <- Tab01_Base()[[2]]
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
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
  })
  
  
  if (1 == 2) {
  # Output de la Base Salida
  output$BASE_SALIDA <- renderDataTable({
    
    if (!is.null(Tab01_Base())) {
      
    #  Tab01_Base()[[2]]
      
   #  cantidad_filas <- as.numeric(as.character(Tab01_Base()[[1]][2]))
   #  cantidad_columnas <-  as.numeric(as.character(Tab01_Base()[[1]][3]))
   #    
  #   sketch <-  htmltools::withTags(table(style = 'font-size: 13px; line-height: 10px'))
     
     
     
     
     datatable(Tab01_Base()[[2]], rownames = F) 
               
     
   # #   class = 'compact nowrap',
  #     style = 'font-size: 13px; line-height: 10px'))
   #    thead(
   #      #  tr(
   #      # # th(rowspan = 2, "Dataset")
   #      # #   th(rowspan = 2, "Data type"),
   #      #    ),
   #      #   tr(
   #     #      lapply(num2let(1:cantidad_columnas),th, colspan = 1)
   #          # th(rowspan = 2, 'Species'),
   #          # th(colspan = 2, 'Sepal'),
   #          # th(colspan = 2, 'Petal')
   #    #    ),
   #      tr(
   #         lapply(colnames(Tab01_Base()[[2]]),th)
   #         #lapply(rep(colnames(Tab01_Base()[[2]]), 2),th)
   #      )
   #    )
   #  )
   #  )
   #  # 
   #  # # Referencias de salidas para datatable()
   #  # # https://datatables.net/reference/option/language
   #  # # https://rstudio.github.io/DT/004-i18n.html
   #  # # "emptyTable":     "No data available in table",
   #  # # "info":           "Showing _START_ to _END_ of _TOTAL_ entries",
   #  # # "infoEmpty":      "Showing 0 to 0 of 0 entries",
   #  # # "infoFiltered":   "(filtered from _MAX_ total entries)",
   #  # # "infoPostFix":    "",
   #  # # "thousands":      ",",
   #  # # "lengthMenu":     "Show _MENU_ entries",
   #  # # "loadingRecords": "Loading...",
   #  # # "processing":     "Processing...",
   #  # # "search":         "Search:",
   #  # # "zeroRecords":    "No matching records found",
   #  
    datatable(Tab01_Base()[[2]], rownames = F,
              container = sketch, list(pageLength = 5,
                                     #  escape = FALSE,
                                    #  autoWidth = TRUE,
                                     #  columnDefs = list(list( targets = 2, width = '600px')),
                                     #  scrollX = TRUE,
                                       #language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                                       language = list(
                                            search = "Búsqueda:",
                                            lengthMenu = "Mostrar _MENU_ registros",
                                            info = "Mostrando registros del _START_ al _END_ de un total de _TOTAL_ registros",
                                            infoFiltered = "(filtrados de un total de _MAX_ registros)",
                                            paginate = list(previous =    "Anterior", `next` = "Siguiente")
                                                     )
    ))
    # datatable(BASE_SALIDA(),escape = FALSE,container = sketch, rownames = F,
    #           selection = "none", options = list(pageLength = cantidad_filas, dom = 't'))
    
    # BASE_DIPLO()
    
    } else return(NULL)
  })

  }
  
  
  
  output$TextBase_InfoDataSet <- renderUI({
    
    if (!is.null(Tab01_Base())) {
    t1 <- paste0("<b>Base:</b> ", Tab01_Base()[[1]][1]) 
    t2 <- paste0("<b>Variables (Columnas):</b> ", Tab01_Base()[[1]][3],
                 " - Desde la columna '", num2let(1), "' hasta la columna '", 
                 num2let(as.numeric(as.character(Tab01_Base()[[1]][3]))), "'.")
    t3 <- paste0("<b>Unidades (Filas):</b> ", Tab01_Base()[[1]][2])
  
    HTML(paste(t1, t2, t3, sep = '<br/>'))
    
    } else return(NULL)
  })
  
  output$TextBase_Intro <- renderText({
    if (!is.null(Tab01_Base())) {
    "Visualización de la Base de Datos"
      } else return(NULL)
  })

  output$TextBase_Alert <- renderText({
    if (!is.null(Control01())) {
     # Tab01_Base()[[3]]
      Control01()[[2]]
    #  "AVER"
    } else return(NULL)
  })
  
  

  
  
}