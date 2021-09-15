

# NOX_04.R :::::: # Ya está todo armado de lo basico
# Para una o dos variables...
# Ahora... armamos la presentacion como la veo el usuario
# pero de Goku...

# 1 Var Categorica
{
  output$salida_TABLAS_1q_goku <- renderUI ({
    
    div(
    h3("Distribución de Frecuencias"),
    uiOutput("tabla_1q_df_goku"), br(),
    h3("Intervalos de Confianza del 90%"),
    uiOutput("tabla_1q_df2_ic90_goku"), br(),
    h3("Intervalos de Confianza del 95%"),
    uiOutput("tabla_1q_df2_ic95_goku"), br(),
    h3("Intervalos de Confianza del 99%"),
    uiOutput("tabla_1q_df2_ic99_goku")
    )
  })
} # Fin 1 Var Categorica
################################################################################
  

# 1 Var Numerica (Cuantitativa)
{
  output$salida_TABLAS_1c_goku <- renderUI ({
    
    div(
      h3("Medidas de Posición"),
      uiOutput("tabla_1c_mp_goku"), br(),
      h3("Medidas de Dispersión"),
      uiOutput("tabla_1c_md_goku"), br(),
      h3("Percentiles"),
      uiOutput("tabla_1c_cuant_goku"), br(),
      h3("Intervalos de Confianza"),
      uiOutput("tabla_1c_ic_goku")
      
    )
  })
} # Fin 1 Var Numerica (Cuantitativa)
################################################################################




# 2 Var Numericas (Cuantitativa)
{
  output$salida_TABLAS_2c_goku <- renderUI ({
    
    div(
      h3("Medidas de Posición"),
      uiOutput("tabla_2c_mp_goku"), br(),
      h3("Medidas de Dispersión"),
      uiOutput("tabla_2c_md_goku"), br(),
      h3("Percentiles"),
      uiOutput("tabla_2c_cuant_goku"), br(),
      h3("Intervalo de Confianza del 90%"),
      uiOutput("tabla_2c_ic90_goku"), br(),
      h3("Intervalo de Confianza del 95%"),
      uiOutput("tabla_2c_ic95_goku"), br(),
      h3("Intervalo de Confianza del 99%"),
      uiOutput("tabla_2c_ic99_goku")
    )
  })
} # Fin 2 Var Numerica (Cuantitativa)
################################################################################



# 2 Var... una numérica y otra categórica (QC)
{
  output$salida_TABLAS_qc_goku <- renderUI ({
    
    div(
      h3("Medidas de Posición Particionadas"),
      uiOutput("tabla_qc_mpp_goku"), br(),
      h3("Medidas de Dispersión Particionadas"),
      uiOutput("tabla_qc_mdp_goku"), br(),
      h3("Percentiles Particionados"),
      uiOutput("tabla_qc_cuantp_goku"), br(),
      h3("Intervalo de Confianza del 90%"),
      uiOutput("tabla_qc_icp90_goku"), br(),
      h3("Intervalo de Confianza del 95%"),
      uiOutput("tabla_qc_icp95_goku"), br(),
      h3("Intervalo de Confianza del 99%"),
      uiOutput("tabla_qc_icp99_goku")
      
    )
  })
} # Fin 2 Var Numerica (Cuantitativa)
################################################################################

  

# 2 Var Categoricas (QQ)
{
  output$salida_TABLAS_2q_goku <- renderUI ({
    
    
    
    div(
      h3("Tablas de Contingencia"),
      uiOutput("tabla_2q_df_00_goku"),
      uiOutput("tabla_2q_df_pack_goku")
    )
  })
} # Fin 1 Var Numerica (Cuantitativa)
################################################################################




  output$planeta_tablas <- renderUI({
    
    if (paso_BASE(BASE_goku())) {
      if(!is.null(tipo_var_goku())) {
      
      # Si es 1 sola variable  
      if(ncol(BASE_goku()) == 1) {
      
      # Si es categorica... o si no lo es...
        if(tipo_var_goku() == "Categórica")   uiOutput("salida_TABLAS_1q_goku")  else   uiOutput("salida_TABLAS_1c_goku")
    
      } else  if(ncol(BASE_goku()) == 2) {
        
        # Si las dos son numericas
        if(sum(tipo_var_goku() == "Categórica") == 0)   uiOutput("salida_TABLAS_2c_goku")  else   if(sum(tipo_var_goku() == "Categórica") == 1)   uiOutput("salida_TABLAS_qc_goku") else if(sum(tipo_var_goku() == "Categórica") == 2)  uiOutput("salida_TABLAS_2q_goku")  else return(NULL)
        
      }
    } else return(NULL)
    } else return(NULL)
   })
  
  