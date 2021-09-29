
nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
opciones_carga1 <- namel(nombres1)

nombres3 <- c("Categórica", "Numérica")
opciones_carga3 <- namel(nombres3)
# nombres3 <- c("Categórica", "Numérica")
# opciones_carga3 <- c(1, 7)
# names(opciones_carga3) <- nombres3

TextServer_Variables <- '
        # Update Menu1  
        # nombres1 <- c("Orden Original", "Alfabético Ascendente", "Alfabético Descendente")
        # opciones_carga1 <- namel(nombres1)
        
        # Update Menu2  
        nombres2_original <- colnames(_MY_BASE_)
        opciones_carga2 <- OpcionesDeColumnas(nombres2_original)
        
        # nombres3 <- c("Categórica", "Numérica")
        # opciones_carga3 <- namel(nombres3) 
        
        
           observeEvent(_MENU1_, {
          
          
          nuevo_orden <- c(1:length(nombres2_original))
          if (_MENU1_ == "Alfabético Ascendente")  nuevo_orden <- order(nombres2_original, decreasing = F) else
            if (_MENU1_ == "Alfabético Descendente") nuevo_orden <- order(nombres2_original, decreasing = T)
            
            opciones_carga2 <- opciones_carga2[nuevo_orden]
      
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_VAR1_")
            updateSelectInput(session, 
                              inputId = "_ONLY_NAME_INPUT_VAR1_",
                              label = "Variable 1",
                              choices=  c("Seleccione una variable..." = "", opciones_carga2),
                              selected = NA)  
            
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_VAR2_")
            updateSelectInput(session, 
                              inputId = "_ONLY_NAME_INPUT_VAR2_",
                              label = "Variable 2",
                              choices=  c("Seleccione una variable..." = "", opciones_carga2),
                              selected = NA) 
            
            ###########
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_TIPO_VAR1_")
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR1_",
                               label = "Tipo de Variable1",
                               choices = opciones_carga3,
                               selected = opciones_carga3[1])
                               
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_TIPO_VAR2_")                   
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR2_",
                               label = "Tipo de Variable2",
                               choices = opciones_carga3,
                               selected = opciones_carga3[1])  
            ############
        })
        
        
           observeEvent(_INPUT_VAR1_, {
          
          if(AllEyesOnMe(ListBase = _THE_LISTBASE_, the_col = _INPUT_VAR1_)) {
            
            my_option <- 1
            if (is.numeric(_THE_LISTBASE_[[1]][,_INPUT_VAR1_])) my_option <- 2
            
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_TIPO_VAR1_")
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR1_",
                               label = "Tipo de Variable1",
                               choices = opciones_carga3,
                               selected = opciones_carga3[my_option])
            
            # 
            
          }
        })
        
        
        
           observeEvent(_INPUT_VAR2_, {
          
          if(AllEyesOnMe(ListBase = _THE_LISTBASE_, the_col = _INPUT_VAR2_)) {
            
            my_option <- 1
            if (is.numeric(_MY_BASE_[,_INPUT_VAR2_])) my_option <- 2
            
            # freezeReactiveValue(input, "_ONLY_NAME_INPUT_TIPO_VAR2_")
            updateRadioButtons(session,
                               inputId = "_ONLY_NAME_INPUT_TIPO_VAR2_",
                               label = "Tipo de Variable2",
                               choices = opciones_carga3,
                               selected = opciones_carga3[my_option])
            
            # 
            
          }
          
          
          
        })
        ##################################################
        '



TextUI_Variables <- 'div(
  fluidRow(
    column(4,
           # Menu1: Orden de las seleccion de variables         
           selectInput(inputId = "menu1_control",
                       label = "Orden de las variables",
                       choices = opciones_carga1,                                                    selected = opciones_carga1[1]
           )
    ),
    column(4, 
           selectInput(inputId = "qtty_var_control",
                       label = "Cantidad de Variables",
                       choices = c("Seleccione..." = "", "Una" = 1, "Dos" = 2)
           )
    ),           
    column(4, 
           numericInput(inputId = "decimales_control", 
                        label = "Decimales:", 
                        min = 0,  max = 100, step = 1, value = 2
           )
    )
  ) # End FluidRow
  ,br(),
  fluidRow(
    conditionalPanel(condition = "input.qtty_var_control != \'\'",
                     conditionalPanel(condition = "input.qtty_var_control >= 1",
                                      fluidRow(
                                        column(4,
                                               selectInput(inputId = "var1_control",
                                                           label = "Variable 1",
                                                           choices=c("Seleccione una variable..." = "", opciones_carga2),
                                                           selected = NA
                                               )
                                        ),
                                        column(4,
                                               radioButtons(inputId = "tipo_var1_control",
                                                            label = "Tipo de Variable1",
                                                            choices = opciones_carga3,
                                                            selected = opciones_carga3[1]
                                               )
                                        )
                                      ) # End FluidRow
                     ), # End ConditionalPanel
                     conditionalPanel(condition = "input.qtty_var_control >= 2",
                                      fluidRow(
                                        column(4,
                                               selectInput(inputId = "var2_control",
                                                           label = "Variable 2",
                                                           choices=c("Seleccione una variable..." = "", opciones_carga2),
                                                           selected = NA
                                               )
                                        ),
                                        column(4,
                                               radioButtons(inputId = "tipo_var2_control",
                                                            label = "Tipo de Variable2",
                                                            choices = opciones_carga3,
                                                            selected = opciones_carga3[1]
                                               )
                                        )
                                      ) # End FluidRow
                     ) #End conditionalPanel
    ) #End conditionalPanel
  ) # End FluidRow
  
)'