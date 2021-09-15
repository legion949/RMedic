


# 1) Ayuda 1Q
{
  ###
  
  # Textos 1Q
  {
    textos_1q <- list()
    
    
    textos_1q[[1]] <- div(
      "Los test más utilizados aplicados a una variable categórica son dos:", br(),
      "'Test de Proporciones' y 'Test de Uniformidad'.", br(),
      br(),
      "En cada test estadístico encontrarás un miniresumen con ", 
      "detalles teóricos, requisitos, estructura de la base de datos y el juego de hipótesis.", br(),
      "Estos te ayudarán a determinar si estas herramientas pueden ser aplicadas en tu trabajo.", br()
      
      
    )
    
    textos_1q[[2]] <- div(
      h3("Test de Proporciones"),
      "Se aplica sobre una columna de la base de datos.", br(),
      "La variable debe tener solo dos categorías.", br(), 
      "De las dos categorías presentes en la variable selecciona una de interés.", br(),
      "De esta categoría se calcula el valor de proporción respecto al total de datos.", br(),
      "Debe elegirse un valor de proporción poblacional bajo hipótesis.", br(),
      "Plantea si la proporción de la categoría seleccionada es igual al valor elegido (bajo hipótesis).",br(), br(),
      
      
      h3("Juego de Hipótesis"),
      "Hay tres formas de generar hipótesis en el test de proporciones para una muestra:", br(), br(),
        h4("Bilateral"),
      "Hipótesis Nula (Ho): La proporción de la categoría seleccionada es igual al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La proporción de la categoría seleccionada es diferente al valor elegido", 
      br(), br(), 
        h4("Unilateral Izquierda"), 
      "Hipótesis Nula (Ho):La proporción de la categoría seleccionada es igual o mayor al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La proporción de la categoría seleccionada es menor al valor elegido", 
      br(),br(), 
        h4("Unilateral Derecha"),
      "Hipótesis Nula (Ho): La proporción de la categoría seleccionada es igual o menor al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La proporción de la categoría seleccionada es mayor al valor elegido", br() 
      
      
    )
    
    
    
    textos_1q[[3]] <- div(
      h3("Test de Uniformidad"), 
      "Se aplica sobre una columna de la base de datos.", br(),
      "La variable debe tener al menos dos categorías.", br(),
      "Plantea si todas las categoría de la variable poseen la misma frecuencia.",br(), br(),
      
      h3("Juego de Hipótesis"), 
      "Hipótesis Nula (Ho): Las frecuencias de todas las categorías son iguales", br(),
      "Hipótesis Alternativa (Hi): Al menos una categoría presenta una frecuencia diferente", br()
      
      
      
    )
    
    
    
    
  } # Fin Textos 1Q
  #######################
  
  output$Help_ho_1q <- renderUI({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
      
      nombre_test <- c("RMedic here!", "Test de Proporciones", "Test de Uniformidad")
      
      div(
        h3("Selección de Ayuda Automática"),
        #radioButtons
        radioButtons(inputId = "radio_help", 
                     label = h3(" "),
                     choices = namel2(nombre_test), 
                     selected = 1)  
        
      )
      
    } else return(NULL)
    
  })
  
  salida_texto_1q <- reactive({ 
    
    if (!is.null(input$radio_help)) {
      
      textos_1q[[as.numeric(input$radio_help)]]
      
    } else return(NULL)
    
  })
  
  # Salida de la ayuda
  observe( output$salida_texto_1q <- renderUI({
    if(!is.null(salida_texto_1q())) {
      salida_texto_1q()
    } else return(NULL)
  }))
  
  
  Help_ho_1q <- reactive({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
      
      
      div(
        fluidRow(
          column(4, uiOutput("Help_ho_1q")),
          column(8, br(), uiOutput("salida_texto_1q"))
        )
      )
      
      
      
    } else return(NULL)
    
  })
  
  ###
} # Fin Ayuda 1Q
###################################################################


# 2) Ayuda 1c
{
  ###
  
  # Textos 1c
  {
  ###
    
    textos_1c <- list()
    
    
    textos_1c[[1]] <- div(
      "Los test más utilizados aplicados a una variable numérica son tres:", br(),
      "'Test t (Una muestra)', 'Test de Wilcoxon (Una muestra)' y 'Test de Normalidad de Shapiro-Wilk'.", br(),
      br(),
      "En cada test estadístico encontrarás un miniresumen con ",  
      "detalles teóricos, requisitos, estructura de la base de datos y el juego de hipótesis.", br(),
      "Estos te ayudarán a determinar si estas herramientas pueden ser aplicadas en tu trabajo.", br()
      
      
    )
    
    textos_1c[[2]] <- div(
      h3("Test t (Una muestra)"),
      "El 'Test t' posee varias formas de ser utilizado.", br(),
      "En este caso, se aplica el test sobre una única columna de la base de datos.", br(),
      "La columna debe ser numérica.", br(), 
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "Tiene como requisito para su utilización verificar previamente que los datos poseen distribución normal.", br(),
      "Utiliza la media de los datos ingresados y compara esta media con un valor de referencia a elección.", br(),
      "Plantea si la media de la muestra es igual al valor seleccionado.",br(), br(),
      
      h3("Juego de Hipótesis"),
      "Hay tres formas de generar hipótesis en el test t para una muestra:", br(),br(),
      h4("Bilateral"),
      "Hipótesis Nula (Ho): La media de la muestra es igual al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La media de la muestra es diferente al valor elegido", br(), br(),
      h4("Unilateral Izquierda"), 
      "Hipótesis Nula (Ho): La media de la muestra es igual o mayor al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La media de la muestra es menor al valor elegido", br(),br(),
      h4("Unilateral Derecha"),
      "Hipótesis Nula (Ho): La media de la muestra es igual o menor al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La media de la muestra es mayor al valor elegido", br(), br() 

      
    )
    
    
    
    textos_1c[[3]] <- div(
      h3("Test de Wilcoxon (Una muestra)"), 
      "Se aplica sobre una columna de la base de datos.", br(),
      "La columna debe ser numérica.", br(), 
      "La naturaleza de los datos puede ser cuantitativa o cualitativa ordinal.", br(),
      "Internamente el test de Wilcoxon estima la posición de la mediana a partir de la media del ranking de los datos.", br(),
      "Utiliza la mediana de los datos ingresados y compara esta mediana con un valor de referencia a elección.", br(),
      "Plantea si la mediana de la muestra es igual al valor seleccionado.",br(), br(),
      
      h3("Juego de Hipótesis"),
      "Hay tres formas de generar hipótesis en el test de Wilcoxon para una muestra:", br(), br(),
      h4("Bilateral"),
      "Hipótesis Nula (Ho): La mediana de la muestra es igual al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La mediana de la muestra es diferente al valor elegido", br(), br(),
      h4("Unilateral Izquierda"), 
      "Hipótesis Nula (Ho): La mediana de la muestra es igual o mayor al valor elegido", br(),
      "Hipótesis Nula (Hi): La mediana de la muestra es menor al valor elegido", br(), br(),
      h4("Unilateral Derecha"),
      "Hipótesis Nula (Ho): La mediana de la muestra es igual o menor al valor elegido", br(),
      "Hipótesis Alternativa (Hi): La mediana de la muestra es mayor al valor elegido", br()
      
      
      
      
    )
    
    
    textos_1c[[4]] <- div(
      h3("Test de Normalidad de Shapiro-Wilk"), 
      "Se aplica sobre una columna de la base de datos.", br(),
      "La columna debe ser numérica.", br(),
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "Es un test sobre la distribución de los datos obtenidos.", br(),
      "Plantea si la variable posee distribución Normal.",br(), 
      
      h3("Juego de Hipótesis"),
      "Hipótesis Nula (Ho): La variable posee una distribución Normal", br(),
      "Hipótesis Alternativa (Hi): La variable no posee una distribución Normal", br()
      
      
      
    )
    
    
  } # Fin Textos 1c
  #######################
  
  output$Help_ho_1c <- renderUI({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
      
      nombre_test <- c("RMedic here!", "Test t (Una muestra)", "Test de Wilcoxon (Una muestra)", "Test de Normalidad de Shapiro-Wilk")
      
      div(
        h3("Selección de Ayuda Automática"),
        radioButtons(inputId = "radio_help", 
                     label = h3(" "),
                     choices = namel2(nombre_test), 
                     selected = 1)  
        
      )
      
    } else return(NULL)
    
  })
  
  salida_texto_1c <- reactive({ 
    
    if (!is.null(input$radio_help)) {
      
      textos_1c[[as.numeric(input$radio_help)]]
      
    } else return(NULL)
    
  })
  
  # Salida de la ayuda
  observe( output$salida_texto_1c <- renderUI({
    if(!is.null(salida_texto_1c())) {
      salida_texto_1c()
    } else return(NULL)
  }))
  
  
  Help_ho_1c <- reactive({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 1) {
      
      
      div(
          fluidRow(
            column(4, uiOutput("Help_ho_1c")),
            column(8, br(), uiOutput("salida_texto_1c"))
            )
      )
      
      
      
    } else return(NULL)
    
  })
  
  ###
} # Fin Ayuda 1c
###################################################################



# 3) Ayuda 2Q
{
  ###
  
  # Textos 2q
  {
    textos_2q <- list()
    
    
    textos_2q[[1]] <- div(
      "Los test más utilizados aplicados a dos variables categóricas son tres:", br(),
      "el 'Test Chi Cuadrado', el 'Test de Diferencia de Proporciones' y ", br(), 
      "'Regresión Logística Simple'. Además se presenta un compendio de herramientas agrupadas en la solapa 'Otros' ('Valores Predictivos' y 'Sensibilidad y Especificidad').", br(),
      br(),
      "En cada test estadístico encontrarás un miniresumen con ", 
      "detalles teóricos, requisitos, estructura de la base de datos y el juego de hipótesis.", br(),
      "Estos te ayudarán a determinar si estas herramientas pueden ser aplicadas en tu trabajo.", br()
      
      
    )
    
    
    textos_2q[[2]] <- div(
      h3("Test Chi Cuadrado"), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Utiliza una tabla de doble entrada (contingencia) sobre la cual se realizan diferentes estimaciones.", br(),
      "Una de las variables tendrá sus categorías en filas y la otra en columnas.", br(),
      "Para el análisis estadístico, el ingreso en filas o en columnas de ambas variables es indistinto ya que se obtienen los mismos resultados.", br(),
      "Si el marco del estudio lo permite, se recomienda fuertemente colocar en filas la variable 'Causa' y en columnas 'Efecto' ya que facilita la interpretación de los resultados obtenidos.", br(),
      "El test Chi Cuadrado plantea la asociación entre ambas variables.",br(), 
      
      
      h3("Juego de Hipótesis"), 
      "Hipótesis Nula (Ho): Las variables son independientes", br(),
      "Hipótesis Alternativa (Hi): Las variables no son independientes", br()
      
      
      
    )
    
    
    textos_2q[[3]] <- div(
      h3("Diferencia de Proporciones"), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Cada variable debe contener solo dos categorías.", br(),
      "Una variable determina dos grupos a comparar.", br(),
      "La otra variable determina los éxitos dentro de cada grupo.", br(),
      "Plantea si las proporciones de éxito de ambos grupos son iguales entre si a través de la siguiente idea:", br(),
      "Dos proporciones son iguales si la diferencia entre ellas es cero.",br(),
      
      h3("Juego de Hipótesis"), 
      "Hipótesis Nula (Ho): La diferencia de proporciones es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La diferencia de proporciones es distinta de cero", br()
      
      
      
    )
    
    
    
    textos_2q[[4]] <- div(
      h3("Regresión Logística Simple"), 
      "La Regresión Logística Simple  posee varias formas de ser utilizada.", br(),
      "Las siguientes recomendaciones corresponden sólo al caso en que ambas variables son categóricas.", br(),
      "Es un requisito que cada variable contenga sólo dos categorías.", br(),
      "Es un modelo de predicción en el que cada variable cumple un rol específico.", br(),
      "Por defecto, la primera variable ingresada es la variable independiente (X) y la segunda es la variable dependiente (Y).", br(),
      "Las dos categorías de cada variable serán transformadas a '0' y '1' según se seleccionen las opciones del test.", br(),
      "El valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio.", br(),
      "El marco de aplicación (y no la estadística) determina cual de las variables ", br(),
      "es independiente (X) y cual dependiente (Y).", br(),
      "Como toda regresión, tendrá un juego de hipótesis para la pendiente y otra para la ordenada al origen.", br(),
      "Por lo general sólo tiene relevancia el juego de hipótesis de la pendiente.", br(),

      

      
      
      br(),
      h3("Juego de Hipótesis"), br(),
      h4("Pendiente"),
      "Hipótesis Nula (Ho): Pendiente igual a cero (No existe una relación logística entre las variables)", br(),
      "Hipótesis Alternativa (Hi): Pendiente distinta de cero (Existe una relación logística entre las variables)", br(),
      
      h4("Ordenada"),
      "Hipótesis Nula (Ho): Ordenada al origen igual a cero", br(),
      "Hipótesis Alternativa (Hi): Ordenada al origen distinta de cero", br()
      
      
    )
    
    
    
   
    
    textos_2q[[5]] <- div(
      h3("Otros"), 
      "La pestaña 'Otros' contiene un conjunto de herramientas aplicables a tablas de contingencia.", br(),
      "Encontraremos: Odd Ratios (OR), Riego Relativo (RR), Valores Predictivos (VP), Sensibilidad y Especificidad.", br(),
      "Estas herramientas son aplicadas sobre dos columna de la base de datos.", br(),
      "Cada variable debe contener solo dos categorías.", br(),
      "Generalmente estas herramientas suelen aplicarse luego de aplicar un Test Chi Cuadrado.", br(), br(),
      "Se detalla de cada herramienta su fórmula y resolución paso a paso.", br(),
      "Encontraremos:", br(),
      "1) Odd Ratios (OR): Se presenta el estimado y el intervalo de confianza (95%)", br(),
      "2) Riego Relativo (RR): Se presenta el estimados y el intervalo de confianza (95%). Además  presenta la prueba de hipótesis para contrastar si el Riesgo Relativo es igual a 1.", br(), br(),
      h3("Juego de Hipótesis"), br(),
      "Hipótesis Nula (H0): Riesgo Relativo poblacional igual a 1", br(),
      "Hipotesis Alternativa: Riesgo Relativo poblacional diferente de 1", br(), br(),
      "3) Valores Predictivos (VP): Se presenta el estimado de Valor predictivo positivo y negativo.", br(),
      "4) Sensibilidad y Especificidad: Se presenta el estimado de sensibilidad y de especificidad.", br(), br()
      
    )
    
    
  } # Fin Textos 2q
  #######################
  
  output$Help_ho_2q <- renderUI({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      nombre_test <- c("RMedic here!", "Test Chi Cuadrado", "Diferencia de Proporciones", "Regresión Logística Simple", "Otros")
      
      div(
        h3("Selección de Ayuda Automática"),
        radioButtons(inputId = "radio_help", 
                     label = h3(" "),
                     choices = namel2(nombre_test), 
                     selected = 1)  
        
      )
      
    } else return(NULL)
    
  })
  
  salida_texto_2q <- reactive({ 
    
    if (!is.null(input$radio_help)) {
      
      textos_2q[[as.numeric(input$radio_help)]]
      
    } else return(NULL)
    
  })
  
  # Salida de la ayuda
  observe( output$salida_texto_2q <- renderUI({
    if(!is.null(salida_texto_2q())) {
      salida_texto_2q()
    } else return(NULL)
  }))
  
  
  Help_ho_2q <- reactive({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      
      div(
        fluidRow(
          column(4, uiOutput("Help_ho_2q")),
          column(8,  br(), uiOutput("salida_texto_2q"))
        )
      )
      
      
      
    } else return(NULL)
    
  })
  
  ###
} # Fin Ayuda 2Q
###################################################################



# 4) Ayuda 2c
{
  ###
  
  # Textos 2c
  {
    textos_2c <- list()
    
    
    textos_2c[[1]] <- div(
      "Los test más utilizados aplicados a dos variable numéricas son siete:", br(),
      "'Correlación de Pearson', 'Correlacion de Spearman',", br(),
      "'Regresion Lineal Simple', 'Regresion Logistica Simple', 'Test t apareado'", br(),
      "'Test Wilcoxon apareado', y 'Test de Homogeneidad de Varianzas de Bartlett'.", br(),
      br(),
      "En cada test estadístico encontrarás un miniresumen con ", 
      "detalles teóricos, requisitos, estructura de la base de datos y el juego de hipótesis.", br(),
      "Estos te ayudarán a determinar si estas herramientas pueden ser aplicadas en tu trabajo.", br()
    )

    
    
    textos_2c[[2]] <- div(
      h3("Correlación de Pearson"),
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Ambas variables deben ser numéricas.", br(),
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "Son requisitos que las variables presenten homogeneidad de varianzas y que ", br(),
      "cada una posea distribución normal.", br(),
      "Estima el valor del índice de correlación lineal de Pearson entre las variables.", br(),
      "Plantea si dicha correlación es igual a cero.",br(), 
      
      h3("Juego de Hipótesis"), 
      "Hipótesis Nula (Ho): La correlación lineal de Pearson es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La correlación lineal de Pearson es distinta a cero", br()
    )
    
    
    textos_2c[[3]] <- div(
      h3("Correlación de Spearman"),
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Ambas variables deben ser numéricas.", br(),
      "La naturaleza de los datos puede ser cuantitativa o cualitativa ordinal.", br(),
      "Estima el valor del índice de correlación de Spearman entre las variables.", br(),
      "Plantea si dicha correlación es igual a cero.",br(), 
      
      h3("Juego de Hipótesis"), 
      "Hipótesis Nula (Ho): La correlación de Spearman es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La correlación de Spearman es distinta a cero", br()
    )
    
    
    
    
    textos_2c[[4]] <- div(
      h3("Regresión Lineal Simple"),
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Ambas variables deben ser numéricas.", br(),
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "Regresión Lineal Simple es un modelo de predicción basado en una recta ", br(),
      "en el que cada variable cumple un rol específico.", br(),
      "Una de las variables cumple el rol de variable independiente (X) y la otra, el de variable dependiente (Y).", br(),
      "El marco de aplicación (y no la estadística) determina cual de las variables ", br(),
      "es independiente (X) y cual dependiente (Y).", br(),
      "Toda recta está definida por una pendiente y una ordenada al origen.", br(),
      "Se genera un juego de hipótesis para la pendiente y otro para la ordenada.", br(),
      "Por defecto, se plantea si tanto la pendiente como la ordenada son iguales a cero.", br(),
      "Por lo general solo el juego de hipótesis de la pendiente tiene relevancia.", br(),
      "El modelo tiene requisitos de homogeneidad y distribución normal de los residuos.", br(),
      "(Un residuo es la diferencia entre el valor observado de 'Y' y el valor esperado en la recta.)", br(),
      "Se agrega un tercer juego de hipótesis sobre el 'R cuadrado ajustado' (R^2 ajustado).", br(),
      "'R^2 ajustado' es un valor entre 0 y 1, donde 0 es un ajuste nulo de los datos a la recta", br(),
      "y 1 es un ajuste perfecto de todos los datos sobre la recta.", br(),
      
      
      
      br(), 
      h3("Juego de Hipótesis"), br(), 
      h4("Pendiente"),
      "Hipótesis Nula (Ho): La pendiente es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La pendiente es distinta a cero", br(),
      h4("Ordenada"),
      "Hipótesis Nula (Ho): La ordenada es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La ordenada es distinta a cero", br(),
      h4("R Cuadrado Ajustado"),
      "Hipótesis Nula (Ho): R Cuadrado Ajustado igual a cero", br(),
      "Hipótesis Alternativa (Hi): R Cuadrado Ajustado diferente a cero", br()
      
      
    )
    
    
    textos_2c[[5]] <- div(
      h3("Regresión Logística Simple"), 
      "La Regresión Logística Simple  posee varias formas de ser utilizada.", br(),
      "En esta ocasión ambas variables deben ser numéricas.", br(),
      "Es un modelo de predicción en el que cada variable cumple un rol específico.", br(),
      "Una variable cumplirá el rol de variable independiente (X) y la otra, variable dependiente (Y).", br(),
      "El marco de aplicación (y no la estadística) determina cuál de las variables ", br(),
      "es independiente (X) y cuál dependiente (Y).", br(),
      "Como toda regresión, tendrá un juego de hipótesis para la pendiente y otra para la ordenada al origen.", br(),
      "Por lo general sólo tiene relevancia el juego de hipótesis de la pendiente.", br(),
      
      
      "La primer variable ingresada será considerada 'X'.", br(),
      "Se generan recomendaciones a partir de dos casos posibles:", br(),br(),
      "Caso 1:", br(),
      "Variables (X) e (Y), ambas poseen exclusivamente dos valores: 0 y 1.", br(),br(),
      
      "Caso 2:", br(),
      "Variable (X) debe ser cuantitativa.", br(),
      "Variable (Y) debe ser cuantitativa y poseer exclusivamente dos valores: 0 y 1.", br(),
      br(),
      "En ambos casos, en la variable 'Y', el valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio.", br(),
      
      
      
      
      
      
      
      br(),
      h3("Juego de Hipótesis"), br(),
      h4("Pendiente"),
      "Hipótesis Nula (Ho): Pendiente igual a cero (No existe una relación logística entre las variables)", br(),
      "Hipótesis Alternativa (Hi): Pendiente distinta de cero (Existe una relación logística entre las variables)", br(),
      
      h4("Ordenada"),
      "Hipótesis Nula (Ho): Ordenada al origen igual a cero", br(),
      "Hipótesis Alternativa (Hi): Ordenada al origen distinta de cero", br()
    )
    
    textos_2c[[6]] <- div(
      h3("Test t Apareado"), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Ambas variables deben ser numéricas.", br(),
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "A partir de cada par de valores de las variables ingresadas (tomados sobre la ", br(),
      "misma unidad o paciente), el análisis internamente obtiene una nueva", br(),
      "variable que es la diferencia entre cada par de valores.", br(),
      "El test plantea si la media de estas diferencias es igual a cero.",br(),
      "Para que los resultados estadístico del Test t apareado sean válidos, es un ", br(),
      "requisito que la nueva variable tenga distribución normal.", br(),
      
      
      
      h3("Juego de Hipótesis"),
      "Hipótesis Nula (Ho): La media de las diferencias es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La media de las diferencias es diferente a cero", br()
    )
    
    
    textos_2c[[7]] <- div(
      h3("Test Wilcoxon Apareado"), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Ambas variables deben ser numéricas.", br(),
      "La naturaleza de los datos debe ser cuantitativa o cualitativa ordinal.", br(),
      "A partir de cada par de valores de las variables ingresadas (tomados sobre la misma unidad o paciente),", br(),
      "el análisis internamente obtiene una nueva variable que es la diferencia entre cada par de valores.", br(),
      "El test de Wilcoxon apareado rankea los datos de esta nueva variable y estima",
      "la posición de la mediana a partir de la media del ranking.", br(),
      "Plantea si la mediana de las diferencias es igual a cero.",br(),
      
      h3("Juego de Hipótesis"),
      "Hipótesis Nula (Ho): La mediana de las diferencias es igual a cero", br(),
      "Hipótesis Alternativa (Hi): La mediana de las diferencias es diferente a cero", br()
    )
    
    
    
    
   
    
        
    textos_2c[[8]] <- div(
      h3("Test de Homogeneidad de Varianzas de Bartlett"),
      "El Test de Homogeneidad de Bartlett posee varias formas de ser utilizado.", br(),
      "Las siguientes recomendaciones y juegos de hipótesis corresponden sólo al caso ", br(),
      "en que ambas variables son numéricas.", br(),
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "La naturaleza de los datos debe ser cuantitativa.", br(),
      "Utiliza una estimación de varianza de cada columna.", br(),
      "Plantea si las varianzas poblaciones son iguales entre si.",br(), br(),
      
      h3("Juego de Hipótesis"),
      "Hipótesis Nula (Ho): Las dos varianzas son iguales", br(),
      "Hipótesis Alternativa (Hi): Las dos varianzas son diferentes", br()
      
      
      
    )
    
    
    
    
    
    

  } # Fin Textos 2c
  #######################
  
  output$Help_ho_2c <- renderUI({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      nombre_test <- c("RMedic here!", 
                       "Correlación de Pearson", 
                       "Correlación de Spearman",
                       "Regresión Lineal Simple",
                       "Regresión Logística Simple",
                       "Test t Apareado",
                       "Test de Wilcoxon Apareado",
                       "Test de Homogeneidad de Varianzas")
      
      div(
        h3("Selección de Ayuda Automática"),
        radioButtons(inputId = "radio_help", 
                     label = h3(" "),
                     choices = namel2(nombre_test), 
                     selected = 1)  
        
      )
      
    } else return(NULL)
    
  })
  
  salida_texto_2c <- reactive({ 
    
    if (!is.null(input$radio_help)) {
      
      textos_2c[[as.numeric(input$radio_help)]]
      
    } else return(NULL)
    
  })
  
  # Salida de la ayuda
  observe( output$salida_texto_2c <- renderUI({
    if(!is.null(salida_texto_2c())) {
      salida_texto_2c()
    } else return(NULL)
  }))
  
  
  Help_ho_2c <- reactive({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      
      div(
        fluidRow(
          column(4, uiOutput("Help_ho_2c")), 
          column(8, br(), uiOutput("salida_texto_2c"))
          )
      )
      
      
      
    } else return(NULL)
    
  })
  
  ###
} # Fin Ayuda 2c
###################################################################





# 5) Ayuda qc
{
  ###
  
  # Textos qc
  {
    textos_qc <- list()
    
 
    textos_qc[[1]] <- div(
      "Los test más utilizados aplicados a dos variable, siendo una numérica y otra categórica, son siete:", br(),
      "'Test t (dos muestras independentes)', 'Test Wilcoxon (dos muestras independientes)', 'Anova a 1 Factor (Anova 1 Way)', ", 
      "'Test de Kruskal-Wallis', 'Regresion Logistica Simple', 'Test de Homogeneidad de Varianzas de Bartlett',", br(),
      "y 'Test de Normalidad de Shapiro-Wilk (particionado)'.", br(),
      br(),
      "En cada test estadístico encontrarás un miniresumen con ", 
      "detalles teóricos, requisitos, estructura de la base de datos y el juego de hipótesis.", br(),
      "Estos te ayudarán a determinar si estas herramientas pueden ser aplicadas en tu trabajo.", br()
    )
    
    textos_qc[[2]] <- div(
      h3("Test t (dos muestras independientes)"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa.", br(),
      "La variable categórica debe tener solo dos categorías.", br(),
      "Cada categoría será considerada un grupo diferente.", br(),
      "Utiliza la media de cada grupo.", br(),
      "Plantea si las medias de ambos grupos son estadísticamente iguales entre si.",br(),
      "Tiene como requisito verificar previamente que las variables tengas distribución normal y homogeneidad de varianza.", br(),
      "El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.", br(),
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho): Las medias de ambos grupos son iguales", br(),
      "Hipótesis Alternativa (Hi): Las medias de ambos grupos son diferentes", br()
    )
    
    textos_qc[[3]] <- div(
      h3("Test de Mann-Whitney (dos muestras independientes)"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa o cualitativa ordinal representada con números.", br(),
      "La variable categórica debe tener solo dos categorías.", br(),
      "Cada categoría será considerada un grupo diferente.", br(),
      "Internamente el test rankea todos los datos juntos y luego obtiene la media del ranking de cada grupo", 
      "para determinar la posición de la mediana de cada grupo.", br(),
      "Plantea si las medianas de ambos grupos son iguales entre si.",br(),br(),
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho): Las medianas de ambos grupos son estadísticamente iguales", br(),
      "Hipótesis Alternativa (Hi): Las medianas de ambos grupos son estadísticamente diferentes", br()
    )
       
  
 
    
    
    textos_qc[[4]] <- div(
      h3("Anova a 1 Factor (Anova 1 Way)"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa.", br(),
      "La variable categórica debe tener dos o más categorías.", br(),
      "Cada categoría será considerada un grupo diferente.", br(),
      "Utiliza la media de cada grupo.", br(),
      "Plantea si las medias de todos los grupos son estadísticamente iguales entre si.",br(),
      "Tiene como requisito verificar que los residuos tengan distribución normal y homogeneidad de varianzas entre los grupos.", br(),
      "Se denomina residuo a la diferencia entre el valor observado y el valor de la media del grupo al cual pertence el dato.", br(),
      "El incumplimiento de los requisitos invalida los resultados estadísticos obtenidos y las conclusiones.", br(),br(),
     
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho): Las medias de todos los grupos son estadísticamente iguales", br(),
      "Hipótesis Alternativa (Hi): Al menos una media de un grupo es estadísticamente diferente", br(), br(),
      h3("Comparaciones Múltiples"), br(),
      "Acompaña al test de Anova un test de comparaciones múltiples de las medias: Test de Tukey", br(),
      "El test de Tukey solo es válido si se rechaza la hipótesis nula de Anova.", br()
    )
    
    
    
    textos_qc[[5]] <- div(
      h3("Test de Kruskal-Wallis"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa o cualitativa ordinal representada con números.", br(),
      "La variable categórica debe tener dos o más categorías.", br(),
      "Cada categoría será considerada un grupo diferente.", br(),
      "Internamente el test de Kruskal-Wallis rankea los datos originales y estima la posición de la mediana de cada grupo ",
      "a partir de la media del ranking de cada grupo.", br(),
      "Plantea si las medianas de todos los grupos son iguales entre si.", br(), br(),
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho): Las medianas de todos los grupos son estadísticamente iguales", br(),
      "Hipótesis Alternativa (Hi): Al menos una mediana de un grupo es estadísticamente diferente", br(), br(),
      h3("Comparaciones Múltiples"), br(),
      "Acompaña al test de Kruskal-Wallis un test de comparaciones múltiples: Test de Dunn.", br(),
      "El test de Dunn solo es válido si se rechaza la hipótesis nula del test de Kruskal-Wallis.", br()
    )
    
    
    
    textos_qc[[6]] <- div(
      h3("Regresión Logística Simple"), 
      
      "La Regresión Logística Simple  posee varias formas de ser utilizada.", br(),
      "Es un modelo de predicción en el que cada variable cumple un rol específico.", br(),
      
      "Las siguientes recomendaciones corresponden al caso en que una variable es numérica y otra categórica.", br(),
      "Una de las variables como variable independiente (X) y la otra como variable dependiente (Y)", br(),
      "El marco de aplicación (y no la estadística) determina cual de las variables ", br(),
      "es independiente (X) y cual dependiente (Y).", br(),
      "Para este caso, la variable numérica será (Y) y la variable categórica será (X).", br(),
      "Es un requisito que la variable categórica (Y) contenga sólo dos categorías.", br(),
      "Las dos categorías de (Y) serán transformadas a '0' y '1' según se seleccionen las opciones del test.", br(),
      "El valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio.", br(),
      "Como toda regresión, tendrá un juego de hipótesis para la pendiente y otra para la ordenada al origen.", br(),
      "Por lo general sólo tiene relevancia el juego de hipótesis de la pendiente.", br(),
      
      "La primer variable ingresada será considerada 'X'.", br(),
      "Se generan recomendaciones a partir de tres casos posibles:", br(),br(),
      
      "Caso 1:", br(),
      "Variable (X) debe ser cuantitativa.", br(),
      "Variable (Y) debe ser cualitativa y poseer exclusivamente dos categorías.", br(), br(),

      "Caso 2:", br(),
      "Variable (X) debe ser cuantitativa y poseer solo dos valores: 0 y 1.", br(),
      "Variable (Y) debe ser cualitativa y poseer exclusivamente dos categorías.", br(), br(),

      "Caso 3:", br(),
      "Variable (X) debe ser cualitativa y poseer solo dos categorías.", br(),
      "Variable (Y) debe ser cuantitativa y poseer solo dos valores: 0 y 1.", br(), br(),
      
      "En los tres casos, en la variable cualitativa, el valor '1' corresponderá a la categoría considerada como 'Éxito' dentro del estudio.", br(),
      
      
      
      
      br(),
      h3("Juego de Hipótesis"), br(),
      h4("Pendiente"),
      "Hipótesis Nula (Ho): Pendiente igual a cero (No existe una relación logística entre las variables)", br(),
      "Hipótesis Alternativa (Hi): Pendiente distinta de cero (Existe una relación logística entre las variables)", br(),
      
      h4("Ordenada"),
      "Hipótesis Nula (Ho): Ordenada al origen igual a cero", br(),
      "Hipótesis Alternativa (Hi): Ordenada al origen distinta de cero", br()
    

    )
  
    
    textos_qc[[7]] <- div(
      h3("Test de Homogeneidad de Varianzas de Bartlett"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa.", br(),
      "La variable categórica debe contener al menos dos categorías.", br(),
      "Cada categoría será considerada un grupo diferente.", br(),
      "Obtiene una estimación de la varianza para cada grupo.", br(),
      "Plantea si dichas varianzas son todas iguales entre si.",br(), br(),
      
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho): las varianzas de todos los grupos son iguales", br(),
      "Hipótesis Alternativa (Hi): Al menos una varianza de un grupo es diferente", br()
    )
    
    
    
    textos_qc[[8]] <- div(
      h3("Test de Normalidad de Shapiro-Wilk (Particionado)"), br(), 
      "Se aplica sobre dos columnas de la base de datos.", br(),
      "Una de ellas numérica y otra categórica.", br(),
      "La variable numérica debe ser de naturaleza cuantitativa.", br(),
      "No hay restricciones sobre la variable categórica. Cada categoría será considerada un grupo diferente.", br(),
      "En este caso, se aplica por un lado un test de normalidad a cada uno de los grupos (particionado)",
      "y por otro lado se aplica un test de normalidad general (sin particionar).", br(),
      "Se informan todos los resultados obtenidos.", br(),
      "El juego de hipótesis es para cada nivel de la variable categórica", br(),
      h3("Juego de Hipótesis"), br(), 
      "Hipótesis Nula (Ho) El grupo presenta una distribución normal", br(),
      "Hipótesis Alternativa (Hi) El grupo no presenta una distribución normal", br()
    )
    
      
    
    
  } # Fin Textos qc
  #######################
  
  output$Help_ho_qc <- renderUI({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      nombre_test <- c("RMedic here!",
                       "Test t (dos muestras independientes)",
                       "Test de Mann-Whitney (dos muestras independientes)",
                       "Anova a 1 Factor (Anova 1 Way)         ",
                       "Test de Kruskal-Wallis",
                       "Regresión Logística Simple",
                       "Test de Homogeneidad de Varianzas de Bartlett", 
                       "Test de Normalidad de Shapiro-Wilk (Particionado)")
      
      div(
        h3("Selección de Ayuda Automática"),
        radioButtons(inputId = "radio_help", 
                     label = h3(" "),
                     choices = namel2(nombre_test), 
                     selected = 1)  
        
      )
      
    } else return(NULL)
    
  })
  
  salida_texto_qc <- reactive({ 
    
    if (!is.null(input$radio_help)) {
      
      textos_qc[[as.numeric(input$radio_help)]]
      
    } else return(NULL)
    
  })
  
  # Salida de la ayuda
  observe( output$salida_texto_qc <- renderUI({
    if(!is.null(salida_texto_qc())) {
      salida_texto_qc()
    } else return(NULL)
  }))
  
  
  Help_ho_qc <- reactive({
    
    if(paso_BASE(BASE_goku()) && ncol(BASE_goku()) == 2) {
      
      
      div(
        fluidRow(
          column(4, uiOutput("Help_ho_qc"), align = 'left'), br(),
          column(8, br(), uiOutput("salida_texto_qc"))
          )
      )
      
      
      
    } else return(NULL)
    
  })
  
  ###
} # Fin Ayuda qc
###################################################################




# 6) Ayuda Sobrevida Kaplan-Meier
{
###  

  # Textos km
  {
  ###
    
  textos_km <- list()
  
  textos_km[[1]] <- div(
    "El test de Sobrevida de Kaplan-Meier es utilizado en la estimación de Sobrevida General y Sobrevida por grupos.", br(),
    "Se aplica sobre dos columnas de la base de datos.", br(),
    "Una variable es el 'Tiempo'.", br(),
    "La otra variable es 'Muerte', que debe contener solo dos valores: 0 y 1.", br(),
    "El '0' codifica el estado inicial del paciente: Vivo.", br(),
    "El '1' codifica el estado la muerte del paciente.", br(),
    "Con esta información se calcula el tiempo mediano de sobrevida de la muestra (Mediana de Kaplan-Meier).", br(), br(),
    
    "Se presentan dos casos:", br(),
    "1) Sobrevida General", br(),
    "Se toman las dos variables ingresadas y el valor de mediana de Kaplan-Meier es la sobrevida general.", br(),br(),
  
    "2) Sobrevida por Grupo", br(),
    "Se agrega una 3ra columna que indica a qué grupo pertenece cada dato.", br(),
    "Se genera entonces una estimación de sobrevida para cada grupo (Mediana por grupo)", br(),
    "La variable indicadora de grupo debe contener al menos dos grupos.", br(),
    "Se genera una prueba de hipótesis para determinar si la curvas de sobrevida de los grupos son iguales entre si.", br(), br(),

    h3("Juego de Hipótesis"), br(),
    "Hipótesis Nula (Ho): Las curvas de sobrevida de todos los grupos son iguales", br(),
    "Hipótesis Alternativa (Hi): Al menos una curva de sobrevida es diferente", br()
  
    
  )
  
  ###
  } # Fin Textos km
  ############################################################
  
 
  salida_texto_km <- reactive({ 
    
      
      textos_km[[1]]
      
  })
   
  # Salida de la ayuda
  observe( output$salida_texto_km <- renderUI({
    if(!is.null(salida_texto_km())) {
      salida_texto_km()
    } else return(NULL)
  }))
  
 
  
  
 
  
###
} # Fin Ayuda Sobrevida Kaplan-Meier
##########################################################################
