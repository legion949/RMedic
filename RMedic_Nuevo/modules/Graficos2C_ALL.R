## Segmento del UI
Graficos2C_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos2C"))
  
}




## Segmento del server
Graficos2C_SERVER <- function(input, output, session, 
                              minibase, 
                              batalla_naval,
                              casoRMedic,
                              decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  # Caso 4: 2C
  casoRMedic <- reactive({
    
    if(is.null(batalla_naval())) return(NULL)
    if(is.null(batalla_naval()[[4]])) return(NULL)
    if(length(batalla_naval()[[4]]) == 0) return(NULL)
    if(batalla_naval()[[4]] == '') return(NULL)
    casoRMedic <- batalla_naval()[[4]]
    #casoRMedic <- as.numeric(as.character(as.vector(batalla_naval()[[4]])))
    casoRMedic
    
  })
  
  
  callModule(module = Graficos2C_01_RMedicHelp_SERVER, 
             id =  "graficos06A",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)
  
  
  callModule(module = Graficos2C_02_XY_SERVER, 
             id =  "graficos06B",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)

  
  callModule(module = Graficos2C_03_MediaDesvioEstandard_SERVER, 
             id =  "graficos06C",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)  
  
  
  callModule(module = Graficos2C_04_MediaErrorEstandard_SERVER, 
             id =  "graficos06D",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)  
  
  
  callModule(module = Graficos2C_05_Boxplot_SERVER, 
             id =  "graficos06E",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)
  
  
  callModule(module = Graficos2C_06_Violinplot_SERVER, 
             id =  "graficos06F",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)
  
  

  callModule(module = Graficos2C_07_Dispersion_SERVER, 
             id =  "graficos06G",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)
  
  
  callModule(module = Graficos2C_08_Conectores_SERVER, 
             id =  "graficos06H",
             minibase = minibase,
             batalla_naval = batalla_naval,
             decimales = decimales,
             casoRMedic = casoRMedic)
  
  # 
  # 
  # callModule(module = Graficos1Q_03_Tortas_SERVER, id =  "graficos03C",
  #            minibase = minibase,
  #            batalla_naval = batalla_naval,
  #            decimales = decimales,
  #            casoRMedic = casoRMedic,
  #            DF_interna = DF_interna)
  
  
  
  output$SeccionGraficos2C <- renderUI({

    # Especificaciones de cumplimiento
    if(is.null(casoRMedic())) return(NULL)
    if(casoRMedic() != 4) return(NULL)



    # Si es el caso 1, seguimos!

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


