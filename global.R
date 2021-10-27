library(shiny)
library(dplyr)
library(ggplot2)

encode <- function(x, schema, bps, rate){
  front <- ifelse(nchar(x) %% bps == 0, 0, bps - nchar(x) %% bps)
  output <- tibble::tibble(input = rep(0, front)) %>%
    tibble::add_row(input = x %>%
                      strsplit(split = "",fixed = T) %>%
                      unlist() %>%
                      as.integer()) %>%
    dplyr::mutate(bit = dplyr::row_number(),
                  sample = rep(seq_len(n()/bps), each = bps))
  if(schema == "NRZ"){
    output <- output %>%
      group_by(sample) %>%
      mutate(temp = (paste0(input, collapse = "") %>% as.integer() %>% DescTools::BinToDec())/(2^bps - 1),
             x = (1 / bps) * (1 / rate) * ((sample - 1)) + (row_number() - 1))
  }
}

# 
# t <- seq(0,200,by=.1)
# x <- cos(2*pi*t/16) + 0.75*sin(2*pi*t/5)
# par(mfrow=c(2,1))
# plot(t,x,'l')
# spectrum(x)
# 
# del <- 0.1 # sampling interval
# x.spec <- spectrum(x,log='no',span=10,plot=F)
# spx <- x.spec$freq/del
# spy <- 2*x.spec$spec
# plot(spy~spx,xlab='frequency',ylab='spectral density',type='l')
# 
