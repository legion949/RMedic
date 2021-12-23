## Segmento del UI
Ho1Q_UI <- function(id) {
  ns <- NS(id)
  
  uiOutput(ns("SeccionHo1Q"))
  
  
}




## Segmento del server
Ho1Q_SERVER <- function(input, output, session, 
                              minibase, 
                              casoRMedic,
                              caso,
                              decimales,
                              alfa) {
  
  
  
  
  # NameSpaceasing for the session
  ns <- session$ns
  
  
  # Control ejecucion 01
  control_ejecucion <- reactive({
    
    
    if(is.null(casoRMedic())) return(FALSE)
    if(is.null(caso)) return(FALSE)
    
    if(casoRMedic() == caso) return(TRUE) else return(FALSE)
    
  })
  
  
  
  
  tablas_1q <-  reactive({RMedic_1q_tablas(minibase(), decimales())[[1]] })
  
  callModule(module = Ho1Q_01_RMedicHelp_SERVER, 
             id =  "ho03A",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1q = tablas_1q,
             alfa = alfa)
  
  
  callModule(module = Ho1Q_02_TestDeUnaProporcion_SERVER,
             id =  "ho03B",
             minibase = minibase,
             decimales = decimales,
             control_ejecucion = control_ejecucion,
             tablas_1q = tablas_1q,
             alfa = alfa)
  #  
  # 
  # callModule(module = Ho1Q_02_TestDeUniformidad_SERVER,
  #            id =  "ho03C",
  #            minibase = minibase,
  #            decimales = decimales,
  #            control_ejecucion = control_ejecucion,
  #            tablas_1q = tablas_1q,
  #            alfa = alfa)

  
 
  output$SeccionHo1Q <- renderUI({
    
    # Especificaciones de cumplimiento
    if(is.null(control_ejecucion())) return(NULL)
    if(!control_ejecucion()) return(NULL)
    
    
    
    # Si es el caso 1, seguimos!
    div(
      h2("RMedic - Pruebas de hipótesis para 1 Variable Categórica"),
      tabsetPanel(id = ns("Ho_1q"),
                  tabPanel(title = "RMedic Help!", value = 1,
                           Ho1Q_01_RMedicHelp_UI(ns("ho03A")),
                          ),
                  tabPanel(title = "Test de Proporciones", value = 2,
                           Ho1Q_02_TestDeUnaProporcion_UI(ns("ho03B"))
                           ),
                  tabPanel(title = "Test de Uniformidad", value = 3,
                           Ho1Q_03_TestDeUniformidad_UI(ns("ho03C"))
                           )
      )
    )
  })
  
  
  
  
  
  
}


