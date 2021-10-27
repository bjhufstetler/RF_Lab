
ui <- fluidPage(
  title = "RF Lab",
  titlePanel("Centurion Virtual RF Lab"),
  sidebarPanel(
    textAreaInput("txtData", "Data", value = "00100100"),
    radioButtons("radioEncode", label = "Encoding Schema", choices = c("NRZ", "Manchester")),
    sliderInput("numBPS", label = "Bits Per Sample", value = 1, min = 1, max = 3, step = 1),
    sliderInput("numSampleRate", label = "Sample Rate (Hz)", value = 1, min = 1, max = 5, step = 1),
    radioButtons("radioModulation", label = "Modulation Type", choices = c("AM", "FM"))
  ),
  mainPanel(
    plotOutput("plotDataInput"),
    plotOutput("plotEncodedData"),
    DT::DTOutput("DTEncodedData")
  ))