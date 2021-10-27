server <- function(input, output){
  output$plotDataInput <- renderPlot(
    encode(input$txtData, schema = input$radioEncode, bps = input$numBPS, rate = input$numSampleRate) %>%
      ggplot() +
      geom_line(aes(x = x, y = temp))
  )
  
  output$DTOutput(DT::renderDataTable(encode(input$txtData, schema = input$radioEncode, bps = input$numBPS, rate = input$numSampleRate)))
}