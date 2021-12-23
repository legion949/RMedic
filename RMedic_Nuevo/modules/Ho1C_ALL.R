## Segmento del UI
Ho1C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionHo1C"))
  
}




## Segmento del server
Ho1C_SERVER <- function(input, output, session, 
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
  
  callModule(module = Ho1C_01_RMedicHelp_SERVER,
             id =  "ho04A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c,
             alfa = alfa)
  
  callModule(module = Ho1C_02_TestTUnaMuestra_SERVER,
             id =  "ho04B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c,
             alfa = alfa)
   
  callModule(module = Ho1C_03_TestWilcoxonUnaMuestra_SERVER,
             id =  "ho04C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c,
             alfa = alfa)


  callModule(module = Ho1C_04_TestNormalidadShapiroWilk_SERVER,
             id =  "ho04D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c,
             alfa = alfa)
  
  callModule(module = Ho1C_05_TestChiCuadradoUnaMuestra_SERVER,
             id =  "ho04E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1c = tablas_1c,
             alfa = alfa)
  
  # 
  # callModule(module = Graficos1C_05_Violinplot_SERVER, id =  "ho04E",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_1c = tablas_1c)
  # 
  # callModule(module = Graficos1C_06_Histograma_SERVER, id =  "ho04F",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_1c = tablas_1c)
  #  
  # callModule(module = Graficos1C_07_Dispersion_SERVER, id =  "ho04G",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_1c = tablas_1c)
  # 
  # 
  # callModule(module = Graficos1C_08_Puntos_SERVER, id =  "ho04H",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_1c = tablas_1c)

 
  
  
  
  output$SeccionHo1C <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)



    # Si es el caso 1, seguimos!

      div(
        h2("RMedic - Pruebas de Hipótesis para 1 Variable Numérica"),
        tabsetPanel(id = ns("Ho_1c"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             Ho1C_01_RMedicHelp_UI(ns("ho04A"))),
                    tabPanel(title = "Test t (Una muestra)", value = 2,
                             Ho1C_02_TestTUnaMuestra_UI(ns("ho04B"))),
                    tabPanel(title = "Test de Wilcoxon (Una muestra)", value = 3,
                             Ho1C_03_TestWilcoxonUnaMuestra_UI(ns("ho04C"))),
                    tabPanel(title = "Test Normalidad (Shapiro-Wilk)", value = 4,
                             Ho1C_04_TestNormalidadShapiroWilk_UI(ns("ho04D"))),
                    tabPanel(title = "Test Chi Cuadrado (Una muestra)", value = 5,
                             Ho1C_05_TestChiCuadradoUnaMuestra_UI(ns("ho04E")))
        )
      )

  })


  
  
   #     }
 # })
  
}


