## Segmento del UI
Graficos1C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos1C"))
  
}




## Segmento del server
Graficos1C_SERVER <- function(input, output, session, 
                              minibase, 
                              casoRMedic,
                              caso,
                              decimales,
                              batalla_naval) {
  

  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    ejecucion <- FALSE
    if(is.null(casoRMedic())) return(ejecucion)
    if(is.null(caso)) return(ejecucion)

    if(casoRMedic() == caso) {
      
      if(batalla_naval()[[6]]) ejecucion <- TRUE else ejecucion <- FALSE 
        
      } else ejecucion <- FALSE
    
    
    return(ejecucion)
    
  })
  

  
  tablas_1c <- reactive({
    
    if(!control_ejecucion()) return(NULL)

    
    
    
    # Nota: al valor input$x_breaks lo tuve que poner
    #      como na.omit(input$x_breaks)[1] por que algunas veces
    #      otorga un vector con dos valores, pero uno de ellos es NA.
    
    
    
    salida <-  RMedic_1c_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = NULL,
                                input_max = NULL,
                                input_breaks = NULL,
                                input_side = NULL
    )
    
    
    
    # Return Exitoso
    return(salida)
    
    
  })  
  
 # DF_interna <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  callModule(module = Graficos1C_01_RMedicHelp_SERVER,
             id =  "graficos04A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)
  
  callModule(module = Graficos1C_02_MediaDesvioEstandard_SERVER,
             id =  "graficos04B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)

  callModule(module = Graficos1C_03_MediaErrorEstandard_SERVER,
             id =  "graficos04C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)


  callModule(module = Graficos1C_04_Boxplot_SERVER, id =  "graficos04D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)

  callModule(module = Graficos1C_05_Violinplot_SERVER, id =  "graficos04E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)

  callModule(module = Graficos1C_06_Histograma_SERVER, id =  "graficos04F",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)
   
  callModule(module = Graficos1C_07_Dispersion_SERVER, id =  "graficos04G",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)
  

  callModule(module = Graficos1C_08_Puntos_SERVER, id =  "graficos04H",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c)

 
  
  
  
  output$SeccionGraficos1C <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)



    # Si es el caso 1, seguimos!

      div(
        h2("RMedic - Gráficos para 1 Variable Cuantitativa"),
        tabsetPanel(id = ns("Graficos_1c"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             Graficos1C_01_RMedicHelp_UI(ns("graficos04A"))),
                    tabPanel(title = "Media y Desvío Estándard", value = 2,
                             Graficos1C_02_MediaDesvioEstandard_UI(ns("graficos04B"))),
                    tabPanel(title = "Media y Error Estándard", value = 3,
                             Graficos1C_03_MediaErrorEstandard_UI(ns("graficos04C"))),
                    tabPanel(title = "Boxplot", value = 4,
                             Graficos1C_04_Boxplot_UI(ns("graficos04D"))),
                    tabPanel(title = "Violín Plot", value = 5,
                             Graficos1C_05_Violinplot_UI(ns("graficos04E"))),
                    tabPanel(title = "Histograma", value = 6,
                             Graficos1C_06_Histograma_UI(ns("graficos04F"))),
                    tabPanel(title = "Dispersión", value = 7,
                             Graficos1C_07_Dispersion_UI(ns("graficos04G"))),
                    tabPanel(title = "Puntos", value = 8,
                             Graficos1C_08_Puntos_UI(ns("graficos04H")))
        )
      )

  })


  
  
   #     }
 # })
  
}


