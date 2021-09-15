

cat("Pre Aula", "\n")

reactive(input$nav == "Herramientas", { 
  source("../009App/AppGeneral/script/004_lib.R", local = TRUE)$value
  source("../009App/AppGeneral/script/001_server.R", local = TRUE)$value
})

cat("Post Aula", "\n")