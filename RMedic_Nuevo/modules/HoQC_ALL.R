## Segmento del UI
HoQC_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionHoQC"))
  
}




## Segmento del server
HoQC_SERVER <- function(input, output, session, 
                              minibase,
                              casoRMedic,
                              caso,
                              decimales,
                              alfa,
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
  

  callModule(module = HoQC_01_RMedicHelp_SERVER,
             id =  "ho07A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tabla_qc = tablas_qc,
             alfa = alfa)

  
  callModule(module = HoQC_02_TestTDosMuestrasIndependientes_SERVER,
             id =  "ho07B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  callModule(module = HoQC_03_TestWilcoxonDosMuestrasIndependientes_SERVER,
             id =  "ho07C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  callModule(module = HoQC_07_TestHomogeneidadDeVarianzasFisher_SERVER,
             id =  "ho07G",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  callModule(module = HoQC_08_TestHomogeneidadDeVarianzasBartlett_SERVER,
             id =  "ho07H",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  callModule(module = HoQC_09_TestNormalidadShapiroWilkParticionado_SERVER,
             id =  "ho07I",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  
  # callModule(module = GraficosQC_02_MediaDesvioEstandard_SERVER,
  #            id =  "graficos07B",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_qc = tablas_qc)
  # 
  # 
  # 
  # callModule(module = GraficosQC_03_MediaErrorEstandard_SERVER,
  #            id =  "graficos07C",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_qc = tablas_qc)
  # 
  # 
  # 
  # callModule(module = GraficosQC_04_Boxplot_SERVER,
  #            id =  "graficos07D",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_qc = tablas_qc)
  # 
  # 
  # 
  # 
  # callModule(module = GraficosQC_05_Violinplot_SERVER,
  #            id =  "graficos07E",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_qc = tablas_qc)
  # 
  # 
  # # 
  # # 
  # callModule(module = GraficosQC_06_Dispersion_SERVER,
  #            id =  "graficos07F",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_qc = tablas_qc)
  # 


  
  
  output$SeccionHoQC <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)




      div(
        h2("RMedic - Gráficos para 1 Variable Categórica y 1 Variable Numérica"),
        tabsetPanel(id = ns("Ho_qc"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             HoQC_01_RMedicHelp_UI(ns("ho07A"))),
                    
                    tabPanel(title = "Test t (Dos muestras independientes)", value = 2,
                             HoQC_02_TestTDosMuestrasIndependientes_UI(ns("ho07B"))),
                    
                    tabPanel(title = "Test Wilcoxon (Dos muestras independientes)", value = 3,
                             HoQC_03_TestWilcoxonDosMuestrasIndependientes_UI(ns("ho07C"))),
                    
                    tabPanel(title = "Test de Homogeneidad de Varianzas de Fisher", value = 7,
                             HoQC_07_TestHomogeneidadDeVarianzasFisher_UI(ns("ho07G"))),
                    
                    tabPanel(title = "Test de Homogeneidad de Varianzas de Bartlett", value = 8,
                             HoQC_08_TestHomogeneidadDeVarianzasBartlett_UI(ns("ho07H"))),
                    
                    tabPanel(title = "Test de Normalidad Shapiro-Wilk (Particionado)", value = 9,
                             HoQC_09_TestNormalidadShapiroWilkParticionado_UI(ns("ho07I"))),

                    
                    # 
                    # tabPanel(title = "Boxplot", value = 4,
                    #          GraficosQC_04_Boxplot_UI(ns("graficos07D"))),
                    # 
                    # tabPanel(title = "Violon Plot", value = 5,
                    #          GraficosQC_05_Violinplot_UI(ns("graficos07E"))),
                    # # 
                    # tabPanel(title = "Dispersión", value = 6,
                    #          GraficosQC_06_Dispersion_UI(ns("graficos07F")))
        )
      )

  })


  
  
  
  
}


