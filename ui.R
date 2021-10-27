
ui <- fluidPage(
  title = "RF Lab",
  titlePanel("Virtual Encoding and Modulation Lab for Centurions!"),
  sidebarPanel(
    textAreaInput("txtData", "Data", value = "0000 1011 0010 1101 0100 0111 0110 0101 1000 1010 0001 1110"),
    radioButtons("radioEncode", label = "Encoding Schema", choices = c("NRZ")),
    sliderInput("numBPS", label = "Bits Per Sample", value = 4, min = 1, max = 4, step = 1),
    textInput("txtSampleRate", "Sample Rate (Hz)", value = "10")
  ),
  mainPanel(
    plotOutput("plotEnc"),
    plotOutput("plotAM"),
    plotOutput("plotFM"),
    plotOutput("plotDec")
  ))