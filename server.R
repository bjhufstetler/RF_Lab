server <- function(input, output, session){
  
  observeEvent(input$btnSubmit, {
    set.seed(input$txtSeed)
    targetWaveBase <- runif(as.integer(3)) * 2
    targetWave <- tibble(x = 0:3000/1000) %>%
        mutate(temp = sin(pi * x * targetWaveBase[1]) +
                      sin(pi * x * targetWaveBase[2]) +
                      sin(pi * x * targetWaveBase[3]),
               temp2 = temp + abs(min(temp)),
               y = 2 * temp2 / max(temp2) - 1) %>%
        select(x, y)
    
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
  
  observeEvent(input$btnHelp, {
    showModal(modalDialog(
      title = HTML("<h2>Getting Started</h2>"),
      HTML('
      Welcome to the virtual RF Encoding and Modulation Lab.
      <h3>Select a Problem</h3>
      <p>To begin, type a number into the "Problem Number" box and press "Start". The default data is the solution for problem number 42.</p>
      <h3>Goal</h3>
      <p>Your goal is to create a binary (ones and zeros) string of data that properly encodes the red analog waveform. To check your answer 
      click "Submit" and compare the encoded signal (black) and the decoded signal (blue) in the top and bottom graphs.</p>
      <p>You do not have to have the entire problem solved when you click submit. Every time you click submit, the graphs will refresh to include 
      the data you have entered.</p>'),
      tags$img(src = base64enc::dataURI(file = "www/problem32start.png", mime = "image/png"), width = "500"),
      tags$img(src = base64enc::dataURI(file = "www/problem32finish.png", mime = "image/png"), width = "500"),
      HTML('<h4>Successful encoding for problem 32</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/pass32.png", mime = "image/png"), width = "500"),
      HTML('<h4>Unsuccessful encoding for problem 32</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/fail32.png", mime = "image/png"), width = "500"),
      HTML('
      <h3>Bits Per Sample</h3>
      <p>The "Bits Per Sample" will determine how many vertical levels the encoded data can take. With more bits per sample 
      more data that can be expressed in each sampling interval, but it becomes harder to distinguish the individual quantized 
      levels from the modulated signal.</p>
      <p>All possible values for each of the allowed bits per sample are illustrated below.</p>
      <h3>Sampling Rate</h3>
      <p>The "Sampling Rate" will determine how much time passes between each sample. A higher sampling rate will allow for better resolution 
      of the encoded data, but will mean the carrier wave will have to have a higher frequency as well.</p>
      <h4>1 Bit Per Sample at 1 Hz Sampling Rate (1 sample per second)</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/bps1.png", mime = "image/png"), width = "500"),
      HTML('<h4>2 Bits Per Sample at 3 Hz Sampling Rate (3 samples per second)</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/bps2.png", mime = "image/png"), width = "500"),
      HTML('<h4>3 Bits Per Sample at 5 Hz Sampling Rate (5 samples per second)</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/bps3.png", mime = "image/png"), width = "500"),
      HTML('<h4>4 Bits Per Sample at 10 Hz Sampling Rate (10 samples per second)</h4>'),
      tags$img(src = base64enc::dataURI(file = "www/bps4.png", mime = "image/png"), width = "500"),
      HTML('
      <h3>Carrier Wave</h3>
      <p>A carrier wave is a simple sinusoid with a constant amplitude and frequency. The power and frequency of the carrier wave can be 
      modulated (altered) in order to transmit digital data. The carrier wave used in this virtual lab operates at 10 Hz.</p>'),
      tags$img(src = base64enc::dataURI(file = "www/carrier.png", mime = "image/png"), width = "500"),
      HTML('
      <h3>Amplitude Modulation</h3>
      <p>Amplitude modulation (AM) is the act of altering the power of a carrier wave. Pulse code modulation (PCM) can be applied to the amplitude of a 
      carrier wave to create a digital Amplitude Shift Key (ASK) signal. Each discrete power level corresponds to the bits encoded during a sample.</p>'),
      tags$img(src = base64enc::dataURI(file = "www/am32.png", mime = "image/png"), width = "500"),
      HTML('
      <h3>Frequency Modulation</h3>
      <p>Frequency modulation (FM) is the act of altering the frequency of a carrier wave. Pulse code modulation (PCM) can be applied to the frequency of a 
      carrier wave to create a digital Frequency Shift Key (FSK) signal. Each discrete frequency corresponds to the bits encoded during a sample.</p>
           '),
      tags$img(src = base64enc::dataURI(file = "www/fm32.png", mime = "image/png"), width = "500"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}