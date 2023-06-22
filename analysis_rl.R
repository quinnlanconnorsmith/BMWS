setwd("C:/Users/ladwi/Documents/Projects/R/Quinn")
library(tidyverse)
library(biwavelet)
library(rLakeAnalyzer)

df <- read_csv('mcd_lit_pel_RL.csv')
head(df)

df <- df %>%
  mutate(datetime = as.POSIXct(paste0(date," ",sprintf("%02d", hour),":00:00")))

ggplot(df) +
  geom_line(aes(datetime, littoral6, col = 'littoral6')) +
  geom_line(aes(datetime, littoral8, col = 'littoral8')) +
  geom_line(aes(datetime, pelagic, col = 'pelagic')) +
  theme_bw()

littoral6 <- ts(df$littoral6)
littoral8 <- ts(df$littoral8)
pelagic <- ts(df$pelagic)

ccf(littoral6, littoral8, lag.max = 24*30)
ccf(pelagic, littoral8, lag.max = 24*30)
ccf(pelagic, littoral6, lag.max = 24*30)

# Define two sets of variables with time stamps
lit6 = df %>%
  rename(Date = datetime) %>%
  mutate(Date = (as.numeric(Date)- as.numeric(Date)[1]) / 3600) %>%
  select(Date, littoral6)

lit8 = df %>%
  rename(Date = datetime) %>%
  mutate(Date = (as.numeric(Date) - as.numeric(Date)[1]) / 3600) %>%
  select(Date, littoral8)
# Specify the number of iterations. The more, the better (>1000).  For the
# purpose of this tutorial, we just set it = 10
nrands = 10

wtc.AB = wtc(as.data.frame(lit6), as.data.frame(lit8), nrands = nrands)
# Plotting a graph
par(oma = c(0, 0, 0, 1), mar = c(5, 4, 5, 5) + 0.1)
plot(wtc.AB, plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE, main = "Wavelet Coherence: littoral6 vs littoral8")
# 
# # Adding grid lines
# n = length(lit6[, 1])
# abline(v = seq(260, n, 260), h = 1:16, col = "brown", lty = 1, lwd = 1)
# 
# # Defining x labels
# axis(side = 3, at = c(seq(0, n, 260)), labels = c(seq(1999, 2015, 1)))