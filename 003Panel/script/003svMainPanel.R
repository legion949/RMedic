
# # # 003 - Server - Main Panel



output$valuesMainPanel <- renderPrint({
  list("Tipo de archivo" = input$selector_tipo_archivo,
       "Curso 2018" = input$selector_curso2018,
       "Archivo Ejemplo" = input$ejemplo_file,
       "Archivo Excel" = input$xls_file,
       "Archivo CSV" = input$csv_file,
       "Archivo Curso 2018" = input$curso2018_file,
       "Archivo UCC" = input$bases_ucc
  )
})






