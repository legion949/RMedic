## Segmento del UI
Graficos2Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos2Q"))
  
  
}




## Segmento del server
Graficos2Q_SERVER <- function(input, output, session, 
                              minibase,
                              casoRMedic,
                              caso,
                              decimales,
                              batalla_naval) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    
    if(is.null(casoRMedic())) return(FALSE)
    if(is.null(caso)) return(FALSE)
    
    if(casoRMedic() == caso) return(TRUE) else return(FALSE)
    
  })
  
  
  
  tablas_2q <-  reactive({
    # Control interno 01
    if(!control_ejecucion()) return(NULL)
    
    RMedic_2q_tablas(minibase(), decimales())
    })
  
  callModule(module = Graficos2Q_01_RMedicHelp_SERVER,
             id =  "graficos05A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_2q = tablas_2q)

  
  callModule(module = Graficos2Q_02_Barras_SERVER,
             id =  "graficos05B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_2q = tablas_2q)

  


  
 
  output$SeccionGraficos2Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)
    
    
    
    # Si es el caso 4, seguimos!
    div(
      h2("RMedic - Gráficos para 2 Variables Categóricas"),
      tabsetPanel(id = ns("Graficos_2q"),
                  tabPanel(title = "RMedic Help!", value = 1,
                           Graficos2Q_01_RMedicHelp_UI(ns("graficos05A"))
                          ) ,
                   tabPanel(title = "Barras", value = 2,
                            Graficos2Q_02_Barras_UI(ns("graficos05B"))
                            )
      )
    )
    
    
    
  })
  
  
  
  
  
  
}


