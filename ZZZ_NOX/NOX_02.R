
# NOX_01.R ::::::  Aqui ya se crean todos los objetos reactivos basicos... 
# o sea todas las tablas...
# Para una o dos variables...
# pero de Goku...


# 1 Variable Categorica
{
###
  
tabla_1q_df_goku <- reactive({
  if (paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
    
    
    df01(BASE_goku(), decimales_goku())$df01$DF2
    
  } else return(NULL)
})

tabla_1q_df2_goku <- reactive({
  if (paso_BASE(BASE_goku())) {
    
    
    df01(BASE_goku(), decimales_goku())$df01$IC
    
  } else return(NULL)
})

###
} # Fin Variable Categorica
################################################################################





# 1 Variable Cuantitativa
{
  ###
  
  pack_tabla_1c_goku <- reactive({
    if (paso_BASE(BASE_goku())) {
      
      aver <- list()
      
      # Medidas de Posicion
      aver[[1]] <-  mp(BASE_goku(), decimales_goku())$mp$tabla1_mp
      
      # Medidas de Dispersion      
      aver[[2]] <- md(BASE_goku(), decimales_goku())$md$tabla1_md
      
      # Percentiles
      aver[[3]] <- percentiles(BASE_goku(), input_busqueda = c(1, 5, 10, 25, 50, 75, 90, 95, 99), decimales_goku())$percentiles$tabla_percentiles
      
      # Intervalos de Confianza para la media
      aver[[4]] <- mp(BASE_goku(), decimales_goku())$mp$tabla3_mp
      
      return(aver)
      
      } else return(NULL)
  })
  ###
} # Fin Variable Categorica
################################################################################





# 2 Variable Cuantitativas
{
  ###
  
  pack_tabla_2c_goku <- reactive({
    if (paso_BASE(BASE_goku())) {
      
      
      MINIBASE <- na.omit(BASE_goku())
      estos_decimales <- decimales_goku()
      

      salida <- list()
      
      # Medidas de Posicion simultaneas
      salida[[1]] <-   mps(MINIBASE, estos_decimales)$mps$tabla1_mps
      
      # Medidas de Dispersion simultaneas
      salida[[2]] <-   mds(MINIBASE, estos_decimales)$mds$tabla1_mds
      
      # Percentiles Simultaneos
      salida[[3]] <-   percentiles2(MINIBASE, estos_decimales, input_busqueda = c(5, 10, 90, 95))$perc2$tabla_per2
      
      # Intervalos de confianza simultaneos
      salida[[4]] <-   mps(MINIBASE, estos_decimales)$mps$tabla3_mps
      
      return(salida)
      
    } else return(NULL)
  })
  ###
} # Fin Variable Categorica
################################################################################


# 2 Variables... Q y C
{
  ###
  
  pack_tabla_qc_goku <- reactive({
    if (paso_BASE(BASE_goku())) {
      
      
      MINIBASE <- na.omit(BASE_goku())
      estos_decimales <- decimales_goku()
        
      
      
      salida <- list()
      
      salida[[1]] <-  mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla1_mpp
      
      salida[[2]] <-  mdp(input_base = MINIBASE, input_decimales = estos_decimales)$mdp$tabla1_mdp
      
    #  salida[[3]] <-  mdp(input_base = MINIBASE, input_decimales = estos_decimales)$mdp$tabla1_mdp
      salida[[3]] <-  percentiles3(input_base = MINIBASE, input_decimales = estos_decimales, input_busqueda = c(5, 10, 90, 95))$perc3$tabla1_percp  
   #   salida[[3]] <- mdp(input_base = MINIBASE, input_decimales = estos_decimales)$mdp$tabla1_mdp
      salida[[4]] <-  mpp(input_base = MINIBASE, input_decimales = estos_decimales)$mpp$tabla3_icp2
      
      
      return(salida)
      
    } else return(NULL)
  })
  ###
} # Fin  2 Variables... Q y C
################################################################################




# 2 Variable Categoricas (QQ)
{
  ###
  
  pack_tabla_2q_df_goku <- reactive({
    if (paso_BASE(BASE_goku())) {
      
      
      df02(BASE_goku(), decimales_goku())$df02
      
    } else return(NULL)
  })
  ###
} # Fin Variable Categorica
################################################################################

