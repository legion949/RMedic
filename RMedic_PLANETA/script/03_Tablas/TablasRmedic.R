

# Tablas para 1 variable cualitativa - Reactiave()!
{
  ### 
  
  
  Reactive_tabla_1q_RMedic <- reactive({
    if (Variables_Tablas()[[1]]) {
      
      aver <- list()
      
      # Distribucion de Frecuencias
      aver[[1]] <-  list("Distribución de Frecuencias", 
                         df01(Base_Planeta(), decimales_planeta())$df01$DF2)
      
      aver[[1]][[2]][,2] <- as.character(aver[[1]][[2]][,2])
      aver[[1]][[2]][,3] <- as.character(aver[[1]][[2]][,2])
      
      
      # Intervalos de Confianza 90%
      aver[[2]] <- list("Intervalos de Confianza del 90%",
                        df01(Base_Planeta(), 
                             decimales_planeta())$df01$IC[[1]])
      
      # Intervalos de Confianza 95%      
      aver[[3]] <- list("Intervalos de Confianza del 95%",
                        df01(Base_Planeta(), 
                             decimales_planeta())$df01$IC[[2]])
      
      # Intervalos de Confianza 99%      
      aver[[4]] <- list("Intervalos de Confianza del 99%",
                        df01(Base_Planeta(), 
                             decimales_planeta())$df01$IC[[3]])
      
      return(aver)
      
      
      
    } else return(NULL)
  })
  
  
  observe(
    output$Salida_tabla_1q_RMedic_01 <- renderTable(digits = decimales_planeta(),
                                                    align= "c",{
                                                      
                                                      if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                        tabla <- Reactive_tabla_1q_RMedic()[[1]][[2]]
                                                        
                                                        tabla
                                                      } else return(NULL)
                                                    })
  )
  
  observe(
    output$Salida_tabla_1q_RMedic_02 <- renderTable(digits=decimales_planeta(), 
                                                    align= "c",{
                                                      
                                                      if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                        Reactive_tabla_1q_RMedic()[[2]][[2]]
                                                      } else return(NULL)
                                                    })
  )
  # 
  # 
  observe(
    output$Salida_tabla_1q_RMedic_03 <- renderTable(digits=decimales_planeta(),
                                                    align= "c",{
                                                      
                                                      if(!is.null(Reactive_tabla_1q_RMedic())) {
                                                        Reactive_tabla_1q_RMedic()[[3]][[2]]
                                                      } else return(NULL)
                                                    })
  )
  # 
  # 
  # 
  observe(
    output$Salida_tabla_1q_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
      
      if(!is.null(Reactive_tabla_1q_RMedic())) {
        Reactive_tabla_1q_RMedic()[[4]][[2]]
      } else return(NULL)
    })
  )
  
  
  ###
}
###########################################################


# 1 Variable Cuantitativa - Objetos Reactivos
{
  ###
  
  Reactive_tabla_1c_RMedic <- reactive({
    if (Variables_Tablas()[[1]]) {
      
      aver <- list()
      
      # Medidas Resumen
      aver[[1]] <-  list("Medidas Resumen", 
                         mp(Base_Planeta(), decimales_planeta())$mp$tabla1_mp
                         )
      
      # Medidas de Posicion
      aver[[2]] <-  list("Medidas de Posición", 
                         mp(Base_Planeta(), decimales_planeta())$mp$tabla1_mp
                        )
      
      # Medidas de Dispersion      
      aver[[3]] <- list("Medidas de Dispersión",
                        md(Base_Planeta(), decimales_planeta())$md$tabla1_md
                        )
      
      # Percentiles
      aver[[4]] <- list("Percentiles", 
                        percentiles(Base_Planeta(), 
                                    input_busqueda = c(1, 5, 10, 25, 50, 75, 90, 95, 99), 
                                    input$decimales_tablas)$percentiles$tabla_percentiles
                        )
      
      # Intervalos de Confianza para la media
      aver[[5]] <- list("Intervalos de Confianza para la media",
                        mp(Base_Planeta(), decimales_planeta())$mp$tabla3_mp
                        )
      
      return(aver)
      
    } else return(NULL)
  })
  
  
  
  ##########
  # 1 Variable Numerica (C)
  {
    
    observe( 
      # Medidas de Posicion
      output$Salida_tabla_1c_RMedic_01 <- renderTable(digits=decimales_planeta(), align= "c",{
        
        if(!is.null(Reactive_tabla_1c_RMedic())) {
          Reactive_tabla_1c_RMedic()[[1]][[2]]
        } else return(NULL)
      })
    )
    
    
    
    observe( 
      # Medidas de Posicion
      output$Salida_tabla_1c_RMedic_02 <- renderTable(digits=decimales_planeta(), align= "c",{
        
        if(!is.null(Reactive_tabla_1c_RMedic())) {
          Reactive_tabla_1c_RMedic()[[2]][[2]]
        } else return(NULL)
      })
    )
    
    observe(
      # Medidas de Dispersion
      output$Salida_tabla_1c_RMedic_03 <- renderTable(digits=decimales_planeta(), align= "c",{
        
        if(!is.null(Reactive_tabla_1c_RMedic())) {
          Reactive_tabla_1c_RMedic()[[3]][[2]]
        } else return(NULL)
      })
    )
    
    
    # Percentiles
    observe(
      output$Salida_tabla_1c_RMedic_04 <- renderTable(digits=decimales_planeta(), align= "c",{
        
        if(!is.null(Reactive_tabla_1c_RMedic())) {
          Reactive_tabla_1c_RMedic()[[4]][[2]]
        } else return(NULL)
      })
    )
    
    
    # IC
    observe( 
      output$Salida_tabla_1c_RMedic_05 <- renderTable(digits= decimales_planeta(), align= "c",{
        
        if(!is.null(Reactive_tabla_1c_RMedic())) {
          Reactive_tabla_1c_RMedic()[[5]]
        } else return(NULL)
      })
    )
    
  }
  ################################################################################
  
  
  ###
} # Fin Variable Categorica
################################################################################

