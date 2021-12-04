## Segmento del UI
GraficosQC_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficosQC"))
  
}




## Segmento del server
GraficosQC_SERVER <- function(input, output, session, 
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
  
  
  
  tablas_qc <- reactive({
    
    # Control interno 01
    if(!control_ejecucion()) return(NULL)
    
    salida <-  RMedic_qc_tablas(input_base =  minibase(),
                                input_decimales = decimales(),
                                input_min = NULL,
                                input_max = NULL,
                                input_breaks = NULL,
                                input_side = NULL
    )
    
    # salida[[11]] <- as.matrix(table(minibase()))
    # salida[[11]] <- as.matrix(salida[[11]])
    salida[[11]][1,1] <- as.character(salida[[11]][1,1])
    
    # Return Exitoso
    return(salida)
    
  })
  

  callModule(module = GraficosQC_01_RMedicHelp_SERVER,
             id =  "graficos07A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tabla_qc = tablas_qc)



  callModule(module = GraficosQC_02_MediaDesvioEstandard_SERVER,
             id =  "graficos07B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_qc = tablas_qc)



  callModule(module = GraficosQC_03_MediaErrorEstandard_SERVER,
             id =  "graficos07C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_qc = tablas_qc)



  callModule(module = GraficosQC_04_Boxplot_SERVER,
             id =  "graficos07D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_qc = tablas_qc)




  callModule(module = GraficosQC_05_Violinplot_SERVER,
             id =  "graficos07E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_qc = tablas_qc)


  # 
  # 
  callModule(module = GraficosQC_06_Dispersion_SERVER,
             id =  "graficos07F",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_qc = tablas_qc)



  
  
  output$SeccionGraficosQC <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)




      div(
        h2("RMedic - Gráficos para 1 Variable Cualitativa y 1 Variable Cuantitativa"),
        tabsetPanel(id = ns("Graficos_qc"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             GraficosQC_01_RMedicHelp_UI(ns("graficos07A"))) ,


                    tabPanel(title = "Media y Desvío Estándard", value = 2,
                             GraficosQC_02_MediaDesvioEstandard_UI(ns("graficos07B"))),

                    tabPanel(title = "Media y Error Estándard", value = 3,
                             GraficosQC_03_MediaErrorEstandard_UI(ns("graficos07C"))),

                    tabPanel(title = "Boxplot", value = 4,
                             GraficosQC_04_Boxplot_UI(ns("graficos07D"))),

                    tabPanel(title = "Violon Plot", value = 5,
                             GraficosQC_05_Violinplot_UI(ns("graficos07E"))),
                    # 
                    tabPanel(title = "Dispersión", value = 6,
                             GraficosQC_06_Dispersion_UI(ns("graficos07F")))
        )
      )

  })


  
  
  
  
}


