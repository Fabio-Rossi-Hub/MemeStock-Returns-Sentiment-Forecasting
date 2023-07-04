library(dplyr)
library(tidyverse)

#Reddit WallStreetBets Posts
wsb <- read.csv(file <- "data/reddit_wsb.csv",
               header = TRUE,
               sep = ",",
               dec = ".")

#concatenating title and body
wsb$text <- paste(wsb$title, ":", wsb$body)

wsb$timestamp <- as.POSIXct(wsb$created,
                            origin = "1970-01-01")
wsb$timestamp <- as.Date(wsb$timestamp)

wsb <- wsb %>% select(-c(title, body, url, created))

#Nasdaq stock lists
stocks <- read.csv(file = "data/nasdaq_screener_1647887652553.csv",
               header = TRUE,
               sep = ",",
               dec = ".")

#Finds mentioned stock symbols
reg_expression <- regex(
   paste0("\\b(?:", paste(stocks$Symbol, collapse = "|"),
   "BTC|ETH|DOGE|SHIB|LTC|AVAX|ADA|XPR|USDT
   |BCH|BSV|EOS|BNB|XTZ|SOL|LUNA|DOT|MATIC|WBTC
   |DOGECOIN)\\b"))

reddit_mentions <- wsb %>%
   mutate(stock_mention = str_extract_all(text, reg_expression)) %>%
   unnest(stock_mention) %>%
   distinct()

#False positives: unrelated words recognized as stock symbols
fp <- c("RH", "DD", "CEO", "IMO", "EV", "PM", "TD", "ALL",
       "USA", "IT", "WE", "IS", "YOU", "ON", "ARE", "CAN", "NOW",
       "GET", "ME", "BE", "UK", "GO", "UP", LETTERS, "FOR", "AI",
       "EDIT", "OR", "AM", "RSI", "SO", "OUT", "TA", "BIG", "ONE",
       "HUGE", "HAS", "NEW", "NEXT", "LOVE", "VERY", "BY", "LIVE",
       "LINK", "DTC", "ANY", "PT", "RE", "OI", "OPEN", "ET", "TV",
       "AKA", "PSA", "SKT", "AN", "GOOD", "LOW", "PLAY", "REAL",
       "SEE", "IQ", "IBKR", "RIDE", "APP", "OG", "CASH",
       "FREE", "EVER", "LIFE", "CASH", "MOVE", "III",
       "HOOD", "JP", "JPM")

mentions <- reddit_mentions %>%
   filter(!(stock_mention %in% fp)) %>%
   group_by(stock_mention) %>%
   count() %>%
   arrange(-n) %>%
   print(n = 50)

reddit_mention_counts <- reddit_mentions %>%
   group_by(timestamp, stock_mention) %>%
   count()


top10 <- reddit_mention_counts %>%
   group_by(stock_mention) %>%
   summarise(n = sum(n)) %>%
   ungroup() %>%
   arrange(-n) %>%
   filter(!(stock_mention %in% fp)) %>%
   head(10) %>%
   pull(stock_mention)

#text mining---------

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

text <- select(wsb, c("text"))
text <- distinct(text)

corpus <- corpus(text, text_field = "text")


token <-
   tokens(
      corpus,
      remove_numbers = TRUE,
      remove_punct = TRUE,
      remove_symbols = TRUE,
      remove_url = TRUE,
      include_docvars = TRUE
   )

mydfm <- dfm(token,
             tolower = TRUE
) %>%
dfm_remove(stopwords("english")) %>%
dfm_wordstem()

tstat_freq <- textstat_frequency(mydfm)
textplot_wordcloud(mydfm)