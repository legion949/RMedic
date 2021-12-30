## Segmento del UI
Ho2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionHo2C"))
  
}




## Segmento del server
Ho2C_SERVER <- function(input, output, session, 
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
  
  
  
  callModule(module = Ho2C_01_RMedicHelp_SERVER,
             id =  "ho06A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)


  # Ho 1 : Test Correlacion de Pearson
  callModule(module = Ho2C_02_TestCorrelacionPearson_SERVER,
             id =  "ho06B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  # Ho 2 : Test Correlacion de Spearman
  callModule(module = Ho2C_03_TestCorrelacionSpearman_SERVER,
             id =  "ho06C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  
  # Ho 3 : Test Regresion Lineal Simple
  callModule(module = Ho2C_04_TestRegresionLinealSimple_SERVER,
             id =  "ho06D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  # Ho 5 : Test t (dos muestras apareadas)
  callModule(module = Ho2C_05_TestTApareado_SERVER,
             id =  "ho06E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)

  
  # Ho 6 : Test Wilcoxon (dos muestras apareadas)
  callModule(module = Ho2C_06_TestWilcoxonApareado_SERVER,
             id =  "ho06F",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  # Ho 7 : Test de Homogeneidad de Varianzas de Fisher
  callModule(module = Ho2C_07_TestHomogenedadDeVarianzasFisher_SERVER,
             id =  "ho06H",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)

  
 
  # Ho 8 : Test de Homogeneidad de Varianzas de Bartlett
  callModule(module = Ho2C_08_TestHomogenedadDeVarianzasBartlett_SERVER,
             id =  "ho06I",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  
  # Ho 9 : Test de Homogeneidad de Varianzas de Levene
  callModule(module = Ho2C_09_TestHomogenedadDeVarianzasLevene_SERVER,
             id =  "ho06J",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             alfa = alfa)
  
  # callModule(module = Graficos2C_03_MediaDesvioEstandard_SERVER,
  #            id =  "graficos06C",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)
  # 
  # 
  # 
  # callModule(module = Graficos2C_04_MediaErrorEstandard_SERVER,
  #            id =  "graficos06D",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)
  # 
  # 
  # callModule(module = Graficos2C_05_Boxplot_SERVER,
  #            id =  "graficos06E",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)
  # 
  # 
  # 
  # 
  # callModule(module = Graficos2C_06_Violinplot_SERVER,
  #            id =  "graficos06F",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)
  # 
  # 
  # 
  # 
  # callModule(module = Graficos2C_07_Dispersion_SERVER,
  #            id =  "graficos06G",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)
  # 
  # 
  # callModule(module = Graficos2C_08_Conectores_SERVER,
  #            id =  "graficos06H",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion)

 
  
  
  output$SeccionHo2C <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)




      div(
        h2("RMedic - Pruebas de hipótesis para 2 Variables Numéricas"),
        tabsetPanel(id = ns("Ho_2c"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             Ho2C_01_RMedicHelp_UI(ns("ho06A"))),
                    
                    tabPanel(title = "Test de Correlación de Pearson", value = 2,
                             Ho2C_02_TestCorrelacionPearson_UI(ns("ho06B"))),
                    
                    tabPanel(title = "Test de Correlación de Spearman", value = 3,
                             Ho2C_03_TestCorrelacionSpearman_UI(ns("ho06C"))),
                    
                    tabPanel(title = "Test de Regresión Lineal Simple", value = 4,
                             Ho2C_04_TestRegresionLinealSimple_UI(ns("ho06D"))),
                    
                    
                    
                    
                    tabPanel(title = "Test t (Dos muestras apareadas)", value = 5,
                             Ho2C_05_TestTApareado_UI(ns("ho06E"))),
                    
                    tabPanel(title = "Test Wilcoxon (Dos muestras apareadas)", value = 6,
                             Ho2C_06_TestWilcoxonApareado_UI(ns("ho06F"))),
                    
                    tabPanel(title = "Test de Homogeneidad de Varianzas de Fisher",
                             value = 7,
                             Ho2C_07_TestHomogenedadDeVarianzasFisher_UI(ns("ho06H"))),
                    
                    tabPanel(title = "Test de Homogeneidad de Varianzas de Bartlett",
                             value = 8,
                             Ho2C_08_TestHomogenedadDeVarianzasBartlett_UI(ns("ho06I"))),
                    
                    tabPanel(title = "Test de Homogeneidad de Varianzas de Levene",
                             value = 9,
                             Ho2C_09_TestHomogenedadDeVarianzasLevene_UI(ns("ho06J")))
                    # 
                    # tabPanel(title = "Media y Error Estándard", value = 4,
                    #          Graficos2C_04_MediaErrorEstandard_UI(ns("graficos06D"))),
                    # 
                    # tabPanel(title = "Boxplot", value = 5,
                    #          Graficos2C_05_Boxplot_UI(ns("graficos06E"))),
                    # 
                    # tabPanel(title = "Violon Plot", value = 6,
                    #          Graficos2C_06_Violinplot_UI(ns("graficos06F"))),
                    # 
                    # tabPanel(title = "Dispersión", value = 7,
                    #          Graficos2C_07_Dispersion_UI(ns("graficos06G"))),
                    # 
                    # tabPanel(title = "Conectores", value = 8,
                    #          Graficos2C_08_Conectores_UI(ns("graficos06H")))
        )
      )

  })


  
  
  
  
}


