library(shiny)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  output$sum <- renderPrint({
    search(input$Textarea, tfidf.matrix, doc.list)
  })

})