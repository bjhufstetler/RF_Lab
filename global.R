library(dplyr)
library(tibble)
library(shiny)
library(ggplot2)
library(plotly)
# library(DescTools)

# dat_in <- "000001010011100101110111"
# dat <- "010001010100111000100101"

sampleRate = 10
pps <- 1000 # points per sample
freqBase <- 10 # Hz

formatData <- function(dat_in, bps, sampleRate){
  dat <- gsub(x = dat_in, pattern = " ", replacement = "")
  # Calculate required number of leading variables
  lead <- ifelse(nchar(dat) %% bps == 0, 0, bps - nchar(dat) %% bps) 
  lvlMag <- 2 / (2 ^ bps - 1) # height of each bin
  stepSize <- 1 / sampleRate # width of each bin
  
  # Add leading zeros to data input and convert to numeric
  dat <- paste0(rep("0", lead), dat) %>%
    strsplit(split = "", fixed = T) %>%
    unlist() %>%
    as.integer()
  
  # Parse samples
  datParsed <- tibble(txtSample = as.character(),
                      sample = as.numeric())
  for(i in seq_len(length(dat) / bps)){
    sampleDat <- paste0(dat[1:bps], collapse = "")
    datParsed <- datParsed %>%
      add_row(txtSample = sampleDat,
              sample = i)
    dat <- dat[-1:-bps]
  }
  datParsed <- datParsed %>% mutate(lvl = txtSample %>% as.integer() %>% DescTools::BinToDec(),
                                    xParsed = (sample - 1) * stepSize + stepSize / 2,
                                    yParsed = lvlMag * lvl - .9)
  # Encode
  datEnc <- tibble(x = c(rbind(datParsed$xParsed - stepSize / 2, datParsed$xParsed + stepSize / 2)),
                   y = rep(datParsed$lvl * lvlMag - 1, each = 2),
                   lineColor = rep(datParsed$lvl, each = 2))

  pEnc <- ggplot() +
    geom_path(data = datEnc,
              aes(x = x, y = y)) +
    geom_text(data = datParsed,
              aes(x = xParsed,
                  y = yParsed,
                  label = txtSample)) +
    labs(x = "Time (s)",
         y = "Output Channel",
         title = paste0("Encoding Schema - ", 2^bps, " NRZ")) +
    ylim(-1,1.2) +
    theme_bw() +
    theme(legend.position = "none")
    
  
  datOutputAM <- tibble(x = 1:(nrow(datParsed) * pps) / (sampleRate * pps)) %>%
    mutate(y = (rep(datParsed$yParsed, each = pps) - lvlMag/2 + 2.5) *  sin(2 * pi * x * freqBase),
           lineColor = rep(datParsed$lvl, each = pps))
  
  pAM <- ggplot() +
    geom_path(data = datOutputAM,
              aes(x = x,
                  y = y,
                  color = lineColor)) +
    geom_path(data = datEnc,
              aes(x = x, y = y - lvlMag/2 + 2.6),
              linetype = "dashed",
              alpha = .6) +
    geom_text(data = datParsed,
              aes(x = xParsed,
                  y = yParsed + -lvlMag/2 + 2.6,
                  label = txtSample)) +
    labs(x = "Time (s)",
         y = "Power",
         title = paste0("Modulation Schema - ", 2^bps, " ASK")) +
    theme_bw() +
    theme(legend.position = "none")
  
  datOutputFM <- tibble(x = 1:(nrow(datParsed) * pps) / (sampleRate * pps)) %>%
    mutate(y = sin(2 * pi * x * (freqBase * rep(datParsed$lvl + 1, each = pps))),#+ freqDev * (rep(datParsed$yParsed, each = pps) - lvlMag/2))),
           lineColor = rep(datParsed$lvl, each = pps))
  
  pFM <- ggplot() +
    geom_path(data = datOutputFM,
              aes(x = x,
                  y = y,
                  color = lineColor)) +
    geom_text(data = datParsed,
              aes(x = xParsed,
                  label = txtSample),
              y = 1.1) +
    ylim(-1, 1.2) +
    labs(x = "Time (s)",
         y = "Power",
         title = paste0("Modulation Schema - ", 2^bps, " FSK")) +
    theme_bw() +
    theme(legend.position = "none")
  return(list(pEnc = pEnc,
              pAM = pAM,
              pFM = pFM))
}  
