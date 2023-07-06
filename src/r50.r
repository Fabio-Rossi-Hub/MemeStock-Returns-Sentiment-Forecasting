library(tidyverse)
library(quantmod)
library(tidyquant)

wsb <- read.csv(file = "data/vader_sentiment.csv",
              header = TRUE,
              sep = ",",
              dec = ".")

wsb$timestamp <- parse_date(wsb$timestamp)
wsb <- wsb[c(5, 2, 4, 8)] %>% distinct()
#List of the 50 most popular stocks on robinhood - March 2021
r50 <- c("AAPL", "TSLA", "AMC", "SNDL", "F", "GE", "NIO",
        "MSFT", "DIS", "AMZN", "NOK", "APHA", "GME", "ZOM",
        "AAL", "PLUG", "PFE", "ACB", "CCVI", "CCL", "GPRO", "DAL",
        "OGI", "PLTR", "NAKD", "SNAP", "CTRM", "BABA",
        "MRNA", "BAC", "NFLX", "BB", "CGC", "FCEL", "IDEX",
        "AMD", "TLRY", "META", "TWTR", "NCLH", "T", "GM",
        "SPCE", "ZNGA", "UAL", "BA", "KO", "SBUX", "CRON", "WKHS")

weight <- 1 - plnorm(rep(2:51), meanlog = 2)
weight <- weight/sum(weight) #R50 stock weights

pfolio <- cbind(r50,weight) %>% as.data.frame()

stocks <- r50 %>%
tq_get(get = "stock.prices",
        from = "2021-01-27",
        to   = "2021-08-16")

stocks <- left_join(stocks, pfolio, by = c("symbol" = "r50"))
stocks$weight <- as.numeric(stocks$weight)

#Differiantiation of price and volume
stocks$r <- Delt(stocks$close) #both in arithmetic and
stocks$logr <- Delt(stocks$close, type = "log") #log scales
stocks$D_vol <- Delt(stocks$volume)
stocks$logD_vol <- Delt(stocks$volume, type = "log")


stocks <- filter(stocks, date != "2021-01-27")

stocks <- stocks %>%
  na.omit() %>%
  group_by(date) %>%
  summarise(r = sum(r * weight), logr = sum(logr * weight),
            Dvol = sum(D_vol * weight), logDvol = sum(logD_vol * weight))


df <- full_join(wsb, stocks, by = c("timestamp" = "date"))
