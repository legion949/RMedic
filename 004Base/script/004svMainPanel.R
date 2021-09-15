

# La Base Excel...
BASE_XLS <- reactive({
  
  # Si ha puesto una direccion de archivo...
  if (!is.null(input$xls_file)) {
    
    # Detalles varios...
    inFile <- input$xls_file
    
    # La direccion del archivo...
    archivo <- inFile$datapath
    
    # Cargamos la Base Excel
    carga_xls(archivo)[[2]]
    
  } else return(NULL)
  
  
  
})

# La Base CSV...
BASE_CSV <- reactive({
  
  # Detalles varios de direccion
  inFile <- input$csv_file
  
  # Si no hay archivo
  if (is.null(inFile))
    return(NULL)
  
  # La carga de datos formato CSV
  read.csv(inFile$datapath, header=input$header, sep=input$sep, dec=input$dec, quote=input$quote)
})


# La Base Ejemplo
BASE_EJEMPLO <- reactive({
  
  # Si no hay todavia nadie selecionado...
  if (!is.null(input$ejemplo_file)) {
    
    # Seleccionamos uno de estos...
    switch(input$ejemplo_file,
           "Base Ejemplo" = brm,
           "rock" = rock,
           "pressure" = pressure,
           "cars" = cars,
           "mtcars" = mtcars,
           "AVER" = AVER,
           "iris" = iris)
  }
})

# La Base UCC
BASE_UCC <-  reactive({
  
  if (!is.null(input$bases_ucc)) {
    
    camino <- GoUp("data")
    
    
    media_direccion <- paste0(camino, "/UCC")
    
    archivo <- input$bases_ucc
    
    direccion_completa <- paste(media_direccion, "/", archivo, sep="")
    
    
    read.csv(direccion_completa, sep=";", dec=",", header=T)
    
   
    
    
  }
  
})

# La Base de Cursos 2018
BASE_CURSOS2018 <-  reactive({
  
  if (!is.null(input$selector_base_guardada)) {
    
    camino <- GoUp("data")
    
    curso_elegido <- input$selector_cursos2018
    media_direccion <- paste0(camino, "/Cursos 2018/", curso_elegido)
    

    
    archivo <- input$selector_base_guardada
    
    direccion_completa <- paste(media_direccion, "/", archivo, sep="")
    
    
    read.csv(direccion_completa, sep=";", dec=",", header=T)
    
    #mtcars
    
    
  }
  
})
######################################################


# Base Cruda (es lo que sea que haya seleccionado... y si hay varias cosas seleccionadas,
#  aqui esta la que esta en vista)
BASE_CRUDA <- reactive({
  
  # BASE_DIPLO()
  
  
  if(!is.null(input$selector_tipo_archivo)) {
    
    if (input$selector_tipo_archivo == "Excel") BASE_XLS() else{
      if (input$selector_tipo_archivo == "CSV") BASE_CSV() else{
        if (input$selector_tipo_archivo == "Cursos 2018")  BASE_CURSOS2018() else {
        if (input$selector_tipo_archivo == "Ejemplo")  BASE_EJEMPLO() else {
              if (input$selector_tipo_archivo == "UCC")  BASE_UCC()  
              }
          }
          
        }}} else return(NULL)
  
  
})

# Nombre de la Base de Datos en vista
NOMBRE_BASE <- reactive({
  
  # BASE_DIPLO()
  
  
  if(!is.null(input$selector_tipo_archivo)) {
    
    if (input$selector_tipo_archivo == "Excel") input$xls_file[[1]] else{
      if (input$selector_tipo_archivo == "CSV") input$csv_file[[1]] else{
        if (input$selector_tipo_archivo == "Cursos 2018")  paste0(input$selector_base_guardada, " (", input$selector_cursos2018, ")") else {
          if (input$selector_tipo_archivo == "Ejemplo")  paste0(input$ejemplo_file, " (BASE EJEMPLO)") else {
            if (input$selector_tipo_archivo == "UCC")  paste0(input$bases_ucc, "BASE UCC") 
          }
        }
        
      }}} else return(NULL)
  
  
})


# La Base Salida (Es la Base Cruda, limpiada de algunos errores posibles)...
BASE_SALIDA <- reactive({
  
  # Si ya ha cargado una base...
  if (!is.null(BASE_CRUDA())) if (ncol(BASE_CRUDA()) > 0) if (nrow(BASE_CRUDA()) > 0){
    
    # La limpiamos...
    # Lo que hacemos es... prep_base() hace todo lo que sea factor lo vuelve categorico.
    # finite_base() corrige espacios a la izquierda o derecha en celdas de variables categoricas
    # La base ya tiene muchos menos errores ahora.
   finite_base(prep_base(BASE_CRUDA()))
   
  } else return(NULL)
  
  
  
})



# Cartel de Ayuda Inicial
output$TEXTO_00_BASE <- renderText({ 
  
  # Si aun no se cargo una base de datos
  if (is.null(BASE_SALIDA())) {
    
    TEXTO <- "Seleccione su archivo tipo Excel haciendo clic en 'Browser'.<br/>
              Podrá seleccionar una base de datos precargada cambiando 'Excel' por 'Ejemplo'"
    
  } else TEXTO <- NULL
  
})  

# Output de la Base Salida
output$BASE_SALIDA <- renderDataTable({
  


  
  
 # if (input$cantidad_filas == 1) cantidad_filas <- 30 else cantidad_filas <- nrow(BASE_SALIDA())
  # cantidad_filas <- 10
  cantidad_filas <- nrow(BASE_SALIDA())
  
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
        lapply(colnames(BASE_SALIDA()),th)
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
  
  datatable(BASE_SALIDA(), rownames = F, container = sketch, list(pageLength = 5,
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



# La pestania "Base de Datos"...
# Esta contenida en el siguiente objeto reactivo...
menuBASE <- reactive({

  tabs <- list()
  tabs[[1]] <- tabPanel(title = "Base de Datos", 
                        icon = icon("user-md"), 
                        value = 1, 

                        # Detalles del archivo seleccionado
                        fluidRow(
                          column(4),
                          column(8,
                            paste0("Base: ", NOMBRE_BASE()), br(),
                            paste0("Columnas: ", ncol(BASE_SALIDA())), br(),
                            paste0("Filas: ", nrow(BASE_SALIDA()))
                          )
                        ),
                        br(), br(),
                        
                        # La Base de Datos
                        dataTableOutput('BASE_SALIDA'),
                        br(), br(), br(), br(),
                        
                        # Frase de Ayuda para Base de Datos
                        htmlOutput("TEXTO_00_BASE")
                        )
  tabs
})


# El siguiente objeto...
# Es la salida para observar solo como queda la pestaña "Bade de Datos".
output[["SALIDAmenuBASE"]] <- renderUI({
  
    tabs <- menuBASE()
    do.call(tabsetPanel, tabs)
    
})


