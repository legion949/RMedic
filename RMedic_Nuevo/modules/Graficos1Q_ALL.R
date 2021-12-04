## Segmento del UI
Graficos1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionGraficos1Q"))
  
  
}




## Segmento del server
Graficos1Q_SERVER <- function(input, output, session, 
                              minibase, 
                              casoRMedic,
                              caso,
                              decimales) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    
    if(is.null(casoRMedic())) return(FALSE)
    if(is.null(caso)) return(FALSE)
    
    if(casoRMedic() == caso) return(TRUE) else return(FALSE)
    
  })
  
  
  
  
  tablas_1q <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  callModule(module = Graficos1Q_01_RMedicHelp_SERVER, 
             id =  "graficos03A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1q = tablas_1q)
  
  
  callModule(module = Graficos1Q_02_Barras_SERVER, 
             id =  "graficos03B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1q = tablas_1q)
   
  
  callModule(module = Graficos1Q_03_Tortas_SERVER, 
             id =  "graficos03C",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1q = tablas_1q)

  
 
  output$SeccionGraficos1Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)
    
    
    
    # Si es el caso 1, seguimos!
    div(
      h2("RMedic - Gráficos para 1 Variable Categórica"),
      tabsetPanel(id = ns("Graficos_1q"),
                  tabPanel(title = "RMedic Help!", value = 1,
                           Graficos1Q_01_RMedicHelp_UI(ns("graficos04A")),
                          ),
                  tabPanel(title = "Barras", value = 2,
                           Graficos1Q_02_Barras_UI(ns("graficos03B"))
                           ),
                  tabPanel(title = "Tortas", value = 3,
                           Graficos1Q_03_Tortas_UI(ns("graficos03C"))
                           )
      )
    )
  })
  
  
  
  
  
  
}


