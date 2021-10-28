server <- function(input, output, session){
  
  observeEvent(input$btnSubmit, {
    set.seed(input$txtSeed)
    targetWaveBase <- runif(as.integer(3)) * 2
    print(targetWaveBase)
    targetWave <- tibble(x = 0:3000/1000) %>%
        mutate(temp = sin(pi * x * targetWaveBase[1]) +
                      sin(pi * x * targetWaveBase[2]) +
                      sin(pi * x * targetWaveBase[3]),
               temp2 = temp + abs(min(temp)),
               y = 2 * temp2 / max(temp2) - 1) %>%
        select(x, y)
    
    print(targetWave)
    outputValues <- formatData(dat_in = input$txtData,
                                         bps = input$numBPS,
                                         sampleRate = input$txtSampleRate,
                                         datProb = targetWave,
                                         freqBase = 10)
    output$plotEnc <- renderPlot(outputValues$pEnc)
    output$plotAM <- renderPlot(outputValues$pAM)
    output$plotFM <- renderPlot(outputValues$pFM)
    output$plotDec <- renderPlot(outputValues$pDec)
    
    updateActionButton(session, inputId = "btnSubmit", label = "Update", icon = NULL)
  })
}