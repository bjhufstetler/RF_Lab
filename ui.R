
ui <- fluidPage(
  title = "RF Lab",
  titlePanel("Virtual Encoding and Modulation Lab for Centurions!"),
  sidebarPanel(
    textInput("txtSeed", "Problem Number:", value = "42"),
    textAreaInput("txtData", "Change this data to encode the red analog signal:", value = "1000 1011 1110 1111 1110 1011 1000 0110 0101 0110 1000 1011 1101 1111 1111 1101 1001 0110 0011 0001 0010 0100 0111 1001 1010 1010 1000 0101 0010 0000 0000"),
    sliderInput("numBPS", label = "Bits Per Sample:", value = 4, min = 1, max = 4, step = 1),
    sliderInput("txtSampleRate", "Sample Rate (Hz):", value = 10, min = 1, max = 10, step = 1),
    actionButton("btnSubmit", "Start")
  ),
  mainPanel(
    plotOutput("plotEnc"),
    plotOutput("plotAM"),
    plotOutput("plotFM"),
    plotOutput("plotDec")
  ))