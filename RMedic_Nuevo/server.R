library(DT)

function(input, output, session) {
  

  
  BASE_SALIDA <- reactive({
    
    # 1) DataSet
    # 2) InfoDataSet
    
    if(!is.null(input$FileTypePicker)){
    
      if(input$FileTypePicker == "Excel") { 
        
        if (!is.null(input$xls_file)) {
          
          # Detalles varios...
          inFile <- input$xls_file
          
          # La direccion del archivo...
          archivo <- inFile$datapath
          
          
          
          # 1) DataSet
          library(readxl)
          DataSet <- as.data.frame(read_excel(archivo, col_names= TRUE, sheet = 1, trim_ws = FALSE))
          
          # 3) InfoDataSetFilas y columnas
          my_columns <- c("FileName", "Rows", "Cols")
          InfoDataSet <- rep(NA, length(my_columns))
          names(InfoDataSet) <- my_columns
          InfoDataSet[1] <- input$xls_file[[1]]
          InfoDataSet[2] <- nrow(DataSet)
          InfoDataSet[3] <- ncol(DataSet)
          
          my_exit <- list(InfoDataSet, DataSet)
          
          # Return of the king...
          return(my_exit)
          
        } else return(NULL)
        
        
        } else return(NULL)
        
    } else return(NULL)
    
    
    # Si ha puesto una direccion de archivo...
 
    
    
  })
  

  # Output de la Base Salida
  output$BASE_SALIDA <- renderDataTable({
    
    
    
    
    
    # if (input$cantidad_filas == 1) cantidad_filas <- 30 else cantidad_filas <- nrow(BASE_SALIDA())
    # cantidad_filas <- 10
    cantidad_filas <- nrow(BASE_SALIDA()[[1]][2])
    
    sketch <-  htmltools::withTags(table(
      class = 'compact nowrap',
      style = 'font-size: 13px; line-height: 10px;',
      thead(
        #  tr(
        # # th(rowspan = 2, "Dataset")
        # #   th(rowspan = 2, "Data type"),
        # # th(colspan = 1, style = 'font-style:italic;','Samples'),
        # # th(colspan = 2, style = 'font-style:italic;','Plots'),
        # # th(colspan = 3, style = 'font-style:italic;','Plots2'),
        # # th(colspan = 4, style = 'font-style:italic;','Plots3')
        #    ),
        tr(
          lapply(colnames(BASE_SALIDA()[[2]]),th)
        )
      )
    )
    )
    
    # Referencias de salidas para datatable()
    # https://datatables.net/reference/option/language
    # https://rstudio.github.io/DT/004-i18n.html
    # "emptyTable":     "No data available in table",
    # "info":           "Showing _START_ to _END_ of _TOTAL_ entries",
    # "infoEmpty":      "Showing 0 to 0 of 0 entries",
    # "infoFiltered":   "(filtered from _MAX_ total entries)",
    # "infoPostFix":    "",
    # "thousands":      ",",
    # "lengthMenu":     "Show _MENU_ entries",
    # "loadingRecords": "Loading...",
    # "processing":     "Processing...",
    # "search":         "Search:",
    # "zeroRecords":    "No matching records found",
    
    datatable(BASE_SALIDA()[[2]], rownames = F, container = sketch, list(pageLength = 5,
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
  })

  output$TextInfoDataSet <- renderUI({
    
    if (!is.null(BASE_SALIDA())) {
    t1 <- paste0("Base: ", BASE_SALIDA()[[1]][1]) 
    t2 <- paste0("Filas (Unidades): ", BASE_SALIDA()[[1]][2])
    t3 <- paste0("Columnas (Variables): ", BASE_SALIDA()[[1]][3])
  
    HTML(paste(t1, t2, t3, sep = '<br/>'))
    
    } else return(NULL)
  })
}