library(dplyr)
library(tibble)
library(shiny)
library(ggplot2)
#library(scales)
# library(DescTools)

pps <- 1000 # points per sample

formatData <- function(dat_in, bps, sampleRate, datProb, freqBase){
  freqBase <- as.numeric(freqBase)
  if(is.null(dat_in) | dat_in == "") dat_in <- "0000"
  sampleRate <- as.numeric(sampleRate)
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
  datParsed <- datParsed %>% 
    mutate(lvl = txtSample %>% as.integer() %>% DescTools::BinToDec(),
           xParsed = (sample - 1) * stepSize + stepSize / 2,
           yParsed = lvlMag * lvl - .9) %>%
    filter(xParsed <= 3)
  # Encode
  datEnc <- tibble(x = c(rbind(datParsed$xParsed - stepSize / 2, datParsed$xParsed + stepSize / 2)),
                   y = rep(datParsed$lvl * lvlMag - 1, each = 2),
                   lineColor = rep(datParsed$lvl, each = 2))

  pEnc <- ggplot() +
    geom_path(data = datProb,
              aes(x = x,
                  y = y),
              color = "red",
              alpha = .6) +
    geom_path(data = datEnc,
              aes(x = x, y = y)) +
    geom_text(data = datParsed,
              aes(x = xParsed,
                  y = yParsed,
                  label = txtSample)) +
    labs(x = "Time (s)",
         y = "Output Channel",
         title = paste0("Encoding Schema - ", 2^bps, " NRZ with a sample rate of ", sampleRate, " Hz. \nTotal data transfer speed: ", sampleRate * bps, " bits per second.")) +
    ylim(-1,1.2) +
    xlim(0,3) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_x_continuous(breaks = seq(0,3,1/sampleRate),
                       labels = scales::number_format(accuracy = .01)) +
    scale_y_continuous(breaks = seq(-1, 1, lvlMag),
                       labels = scales::number_format(accuracy = .01))
    
  
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
         title = paste0(2^bps, " ASK Modulated onto a ", freqBase, " Hz carrier wave.")) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(0,3,1/sampleRate)) +
    xlim(0, 3)
  
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
         title = paste0(2^bps, " FSK Modulated onto a ", freqBase, " Hz carrier wave.")) +
    theme_bw() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(0,3,1/sampleRate)) +
    xlim(0, 3)

  pDec <- ggplot() +
    geom_path(data = datProb,
              aes(x = x,
                  y = y),
              color = "red",
              alpha = .6) +
    geom_line(data = data.frame(spline(datParsed %>% mutate(x = xParsed - 1/(2*sampleRate),
                                                            y = yParsed - 0.1) %>% 
                                         select(x,y),
                                       n = 3 * sampleRate * 5)),
              aes(x = x, y = y),
              color = "blue") +
    labs(x = "Time (s)",
         y = "Power",
         title = paste0("Demodulated Decoded Wave (Blue) vs Analog Input (Red)")) +
    theme_bw() +
    xlim(0,3)
  
  return(list(pEnc = pEnc,
              pAM = pAM,
              pFM = pFM,
              pDec = pDec))
}  
