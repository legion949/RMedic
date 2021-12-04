## Segmento del UI
Graficos2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos2C"))
  
}




## Segmento del server
Graficos2C_SERVER <- function(input, output, session, 
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
  
  
  
  callModule(module = Graficos2C_01_RMedicHelp_SERVER,
             id =  "graficos06A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)


  
  # Grafico 1 : XY
  callModule(module = Graficos2C_02_XY_SERVER,
             id =  "graficos06B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)


  callModule(module = Graficos2C_03_MediaDesvioEstandard_SERVER,
             id =  "graficos06C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)
  


  callModule(module = Graficos2C_04_MediaErrorEstandard_SERVER,
             id =  "graficos06D",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)


  callModule(module = Graficos2C_05_Boxplot_SERVER,
             id =  "graficos06E",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)
  
  


  callModule(module = Graficos2C_06_Violinplot_SERVER,
             id =  "graficos06F",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)
  



  callModule(module = Graficos2C_07_Dispersion_SERVER,
             id =  "graficos06G",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)


  callModule(module = Graficos2C_08_Conectores_SERVER,
             id =  "graficos06H",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion)

 
  
  
  output$SeccionGraficos2C <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)




      div(
        h2("RMedic - Gráficos para 2 Variables Cuantitativas"),
        tabsetPanel(id = ns("Graficos_2c"),
                    tabPanel(title = "RMedic Help!", value = 1,
                             Graficos2C_01_RMedicHelp_UI(ns("graficos06A"))),
                    
                    tabPanel(title = "XY", value = 2,
                             Graficos2C_02_XY_UI(ns("graficos06B"))),
                    
                    tabPanel(title = "Media y Desvío Estándard", value = 3,
                             Graficos2C_03_MediaDesvioEstandard_UI(ns("graficos06C"))),
                    
                    tabPanel(title = "Media y Error Estándard", value = 4,
                             Graficos2C_04_MediaErrorEstandard_UI(ns("graficos06D"))),
                    
                    tabPanel(title = "Boxplot", value = 5,
                             Graficos2C_05_Boxplot_UI(ns("graficos06E"))),
                    
                    tabPanel(title = "Violon Plot", value = 6,
                             Graficos2C_06_Violinplot_UI(ns("graficos06F"))),
                    
                    tabPanel(title = "Dispersión", value = 7,
                             Graficos2C_07_Dispersion_UI(ns("graficos06G"))),
                    
                    tabPanel(title = "Conectores", value = 8,
                             Graficos2C_08_Conectores_UI(ns("graficos06H")))
        )
      )

  })


  
  
  
  
}


