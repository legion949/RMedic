

# NOX_03.R :::::: # Todos los objetos reactivos basicos ya fueron creados...
# Son todas las tablas que se pueden obtener en estadistica descriptiva...
# Ahora... esos elementos deben ingresar en un output, para que puedan ser empotrados
# como salidas de la pagina...
# El nombre de las salidas, es el mismo que el reactivo que lo genero...
# Para una o dos variables...
# pero de Goku...



# 1 Variable Categorica (Q)
{
observe(  
output$tabla_1q_df_goku <- renderTable(digits=decimales_goku(), align= "c",{
  
  if(!is.null(tabla_1q_df_goku())) {
    tabla <- tabla_1q_df_goku()
    tabla[,2] <- as.character(tabla[,2])
    tabla[,3] <- as.character(tabla[,3])
    tabla
  } else return(NULL)
})
)
  
  
  observe(  
    output$tabla_1q_df2_ic90_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(tabla_1q_df2_goku())) {
        tabla_1q_df2_goku()[[1]]
      } else return(NULL)
    })
  )  
  
  
  observe(  
    output$tabla_1q_df2_ic95_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(tabla_1q_df2_goku())) {
        tabla_1q_df2_goku()[[2]]
      } else return(NULL)
    })
  )  
  
  
  
  observe(  
    output$tabla_1q_df2_ic99_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(tabla_1q_df2_goku())) {
        tabla_1q_df2_goku()[[3]]
      } else return(NULL)
    })
  )  
  
  
}
################################################################################



# 1 Variable Numerica (C)
{
  observe( 
  # Medidas de Posicion
  output$tabla_1c_mp_goku <- renderTable(digits=decimales_goku(), align= "c",{
    
    if(!is.null(pack_tabla_1c_goku())) {
      pack_tabla_1c_goku()[[1]]
    } else return(NULL)
  })
  )
  
  observe(
  # Medidas de Dispersion
  output$tabla_1c_md_goku <- renderTable(digits=decimales_goku(), align= "c",{
    
    if(!is.null(pack_tabla_1c_goku())) {
      pack_tabla_1c_goku()[[2]]
    } else return(NULL)
  })
  )
  
  
  # Percentiles
  observe(
  output$tabla_1c_cuant_goku <- renderTable(digits=decimales_goku(), align= "c",{
    
    if(!is.null(pack_tabla_1c_goku())) {
      pack_tabla_1c_goku()[[3]]
    } else return(NULL)
  })
  )
  
  
  # IC
  observe( 
    output$tabla_1c_ic_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_1c_goku())) {
        pack_tabla_1c_goku()[[4]]
      } else return(NULL)
    })
  )
  
}
################################################################################


# 2 Variales Numericas (CC)
{
  observe( 
    # Medidas de Posicion
    output$tabla_2c_mp_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[1]]
      } else return(NULL)
    })
  )
  
  observe(
    # Medidas de Dispersion
    output$tabla_2c_md_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[2]]
      } else return(NULL)
    })
  )
  
  
  # Percentiles
  observe(
    output$tabla_2c_cuant_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[3]]
      } else return(NULL)
    })
  )
  
  
  observe( 
    # IC de la Media 90%
    output$tabla_2c_ic90_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[4]][[1]]
      } else return(NULL)
    })
  )
  
  
  
  observe( 
    # IC de la Media 95%
    output$tabla_2c_ic95_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[4]][[2]]
      } else return(NULL)
    })
  )
  
  
  
  observe( 
    # IC de la Media 99%
    output$tabla_2c_ic99_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_2c_goku())) {
        pack_tabla_2c_goku()[[4]][[3]]
      } else return(NULL)
    })
  )
  
}
################################################################################



# 2 Variales... una Numerica y otra Categorica (QC)
{
  observe( 
    # Medidas de Posicion Particionada
    output$tabla_qc_mpp_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[1]]
      } else return(NULL)
    })
  )
  
  observe(
    # Medidas de Dispersion
    output$tabla_qc_mdp_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[2]]
      } else return(NULL)
    })
  )
  
  
  # Percentiles Particionados
  observe(
    output$tabla_qc_cuantp_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[3]]
      } else return(NULL)
    })
  )
  
  
  observe( 
    # Intervalos de Confianza 90% para la media particionados
    output$tabla_qc_icp90_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[4]][[1]]
      } else return(NULL)
    })
  )
  
  observe( 
    # Intervalos de Confianza 95% para la media particionados
    output$tabla_qc_icp95_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[4]][[2]]
      } else return(NULL)
    })
  )
  
  observe( 
    # Intervalos de Confianza 99% para la media particionados
    output$tabla_qc_icp99_goku <- renderTable(digits=decimales_goku(), align= "c",{
      
      if(!is.null(pack_tabla_qc_goku())) {
        pack_tabla_qc_goku()[[4]][[3]]
      } else return(NULL)
    })
  )
  
}
################################################################################



# 2 Variable Categoricas (QQ)
{
  
  
    output$tabla_2q_df_00_goku <- renderUI(
  tabsetPanel(id= "kayak",
    tabPanel("Clásico", value = 1),
    tabPanel("Por filas", value = 3),
    tabPanel("Por columnas", value = 4),
    tabPanel("Al Total", value = 2),
    tabPanel("Simple entrada", value = 5)
  )
  )
  
    # Clasico...
    observe(  
      output$tabla_2q_df_01_goku <- renderTable(digits=0, rownames = TRUE, align= "c",{
        
        if(!is.null(pack_tabla_2q_df_goku())) {
          pack_tabla_2q_df_goku()[[1]][[1]]
        } else return(NULL)
      })
    )
    
    observe(  
      output$tabla_2q_df_02_goku <- renderTable(digits=decimales_goku(),rownames = TRUE, align= "c", {
        
        if(!is.null(pack_tabla_2q_df_goku())) {
          pack_tabla_2q_df_goku()[[1]][[2]]
        } else return(NULL)
      })
    )
    
    observe(  
      output$tabla_2q_df_03_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
        
        if(!is.null(pack_tabla_2q_df_goku())) {
          pack_tabla_2q_df_goku()[[1]][[3]]
        } else return(NULL)
      })
    )
    
    observe(  
      output$tabla_2q_df_04_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
        
        if(!is.null(pack_tabla_2q_df_goku())) {
          pack_tabla_2q_df_goku()[[1]][[5]]
        } else return(NULL)
      })
    )
    
    
  # Por Al total...
  observe(  
    output$tabla_2q_df_05_goku <- renderTable(digits=0, rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[2]][[1]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_06_goku <- renderTable(digits=decimales_goku(),rownames = TRUE, align= "c", {
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[2]][[2]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_07_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[2]][[3]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_08_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[2]][[5]]
      } else return(NULL)
    })
  )
  
  # Por filas...
  observe(  
    output$tabla_2q_df_09_goku <- renderTable(digits=0, rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[3]][[1]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_10_goku <- renderTable(digits=decimales_goku(),rownames = TRUE, align= "c", {
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[3]][[2]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_11_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[3]][[3]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_12_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[3]][[5]]
      } else return(NULL)
    })
  )
  
  
  # Por columnas...
  observe(  
    output$tabla_2q_df_13_goku <- renderTable(digits=0, rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[4]][[1]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_14_goku <- renderTable(digits=decimales_goku(),rownames = TRUE, align= "c", {
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[4]][[2]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_15_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[4]][[3]]
      } else return(NULL)
    })
  )
  
  observe(  
    output$tabla_2q_df_16_goku <- renderTable(digits=decimales_goku(), rownames = TRUE, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[4]][[5]]
      } else return(NULL)
    })
  )
  
  # Simple Entrada
  observe(  
    output$tabla_2q_df_17_goku <- renderTable(digits=decimales_goku(), rownames = T, align= "c",{
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        pack_tabla_2q_df_goku()[[6]]
      } else return(NULL)
    })
  )
  
  

  # DF QQ PACK
  observe(  
    output$tabla_2q_df_pack_goku <- renderUI({
      
      if(!is.null(pack_tabla_2q_df_goku())) {
        if(!is.null(input$kayak)) {
          
        
          # Al total
          if(input$kayak == 1) {
            div(
              h3("Frecuencias Absolutas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_01_goku"), br(),
              
              h3("Cociente al Total"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_02_goku"), br(),
              
              h3("Frecuencias Relativas al Total"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_03_goku"),  br(),
              
              h3("Porcentajes al Total"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_04_goku")
            )
            
          # Al total  
          } else  if(input$kayak == 2) {
            div(
            h3("Frecuencias Absolutas"),
            ref2q_goku()[1], br(),
            ref2q_goku()[2], br(),
            uiOutput("tabla_2q_df_05_goku"), br(),
            
            h3("Cociente al Total"),
            ref2q_goku()[1], br(),
            ref2q_goku()[2], br(),
            uiOutput("tabla_2q_df_06_goku"), br(),
            
            h3("Frecuencias Relativas al Total"),
            ref2q_goku()[1], br(),
            ref2q_goku()[2], br(),
            uiOutput("tabla_2q_df_07_goku"), br(),
            
            h3("Porcentajes al Total"),
            ref2q_goku()[1], br(),
            ref2q_goku()[2], br(),
            uiOutput("tabla_2q_df_08_goku")
            )
            
          # por filas  
          } else    if(input$kayak == 3) {
            div(
              h3("Frecuencias Absolutas por Filas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_09_goku"), br(),
              
              h3("Cociente por Filas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_10_goku"), br(),
              
              h3("Frecuencias Relativas por Filas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_11_goku"), br(),
              
              h3("Porcentajes por Filas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_12_goku")
            )
            
          # Por columnas
          } else    if(input$kayak == 4) {
            div(
              h3("Frecuencias Absolutas por Columnas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_13_goku"), br(),
              
              h3("Cociente por Columnas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_14_goku"), br(),
              
              h3("Frecuencias Relativas por Columnas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_15_goku"), br(),
              
              h3("Porcentajes por Columnas"),
              ref2q_goku()[1], br(),
              ref2q_goku()[2], br(),
              uiOutput("tabla_2q_df_16_goku")
            )
            
          # Simple entrada  
          } else  if(input$kayak == 5) {
            div(
              h3("Simple Entrada"),
              uiOutput("tabla_2q_df_17_goku")
            )
            
            
          } else return(NULL)
            
      } else return(NULL)
      } else return(NULL)
    })
  )
  
    
  
  
}
################################################################################



