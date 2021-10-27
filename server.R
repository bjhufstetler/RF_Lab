server <- function(input, output){
  outputValues <- reactive({formatData(dat = input$txtData, bps = input$numBPS, sampleRate = sampleRate)})
  output$plotEnc <- renderPlot(outputValues()$pEnc)
  output$plotAM <- renderPlot(outputValues()$pAM)
  output$plotFM <- renderPlot(outputValues()$pFM)
}