

library(DT)
library(htmltools)
library(shiny)
library(shinyjs)
library(bslib)
library(readxl)
library(datasets)
# library(shinydashboard)



num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}

let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}


#helper function (convert vector to named list)
namel<-function (vec){
  tmp<-as.list(vec)
  names(tmp)<-as.character(unlist(vec))
  tmp
}

OpcionesDeColumnas <- function(my_names = ""){
  
  # Letras
  letras_elegidas <- paste0("(", num2let(c(1:length(my_names))), ")")
  
  # Visual del usuario
  visual_usuario <- paste0(letras_elegidas, " - ", my_names)

  
  # Armamos el vector de salida
  vector_salida <- my_names
  names(vector_salida) <- visual_usuario
  
  return(vector_salida)
}


AllEyesOnMe <- function(ListBase = NULL, the_col = NULL) {
  
  
  dt_ok <- FALSE
  
  if(!is.null(ListBase))
    if(!is.null(the_col))
      if(the_col != "")
        if(sum(colnames(ListBase[[1]]) == the_col) > 0)
          dt_ok <- TRUE
  
  
  dt_ok
}


ModifyMe <- function(the_text = NULL, end_var = NULL){
  
  
  
  internal_text <- '
  the_text <- gsub("_THE_LISTBASE_", "BaseSalida()", the_text)
  the_text <- gsub("_MY_BASE_", "BaseSalida()[[1]]", the_text)
  the_text <- gsub("_MENU1_", "input$menu1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_VAR1_", "var1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_VAR2_", "var2_control", the_text)
  the_text <- gsub("_INPUT_VAR1_", "input$var1_control", the_text)
  the_text <- gsub("_INPUT_VAR2_", "input$var2_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_TIPO_VAR1_", "tipo_var1_control", the_text)
  the_text <- gsub("_ONLY_NAME_INPUT_TIPO_VAR2_", "tipo_var2_control", the_text)
  '
  if (!is.null(end_var)) internal_text <- gsub("_control", end_var, internal_text)
  eval(parse(text = internal_text))
  
  return(the_text)
  
}
