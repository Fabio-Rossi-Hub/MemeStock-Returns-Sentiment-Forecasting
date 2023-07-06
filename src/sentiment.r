library(tidyverse)

#Lexicon chabges-------
load("vader/R/sysdata.rda")

vader_lexicon %>%
  as_tibble()

#VADER lexicon changes and additions
wsb_lexicon <-
  bind_rows(
    tibble(
      V1 = c(
        "retard",
        "retarded",
        "fuck",
        "fucking", #neutral words
        "autist",
        "fag",
        "gay",
        "stonk",
        "market"
      ),
      V2 = 0,
      V3 = 0.5
    ),
    tibble(
      V1 = c(
        "bull",
        "bullish",
        "tendie",
        "tendies",
        "call",
        "long",
        "buy",
        "moon",
        "hold",
        "diamond",
        "hands",
        "yolo",
        "yoloed",
        "free",
        "btfd",
        "rocket", #positive words
        "elon",
        "gain",
        "420",
        "calls",
        "longs",
        "sky",
        "space",
        "roof",
        "squeeze",
        "balls",
        "yoloing",
        "holding",
        "��s",
        "#x200b"
      ),
      V2 = 1.5,
      V3 = 0.5
    ),
    tibble(
      V1 = c(
        "bear",
        "sell",
        "put",
        "short",
        "shorts",
        "puts",
        "bagholder",
        "wife",
        "boyfriend", #negative words
        "shorting",
        "citron",
        "hedge",
        "fake",
        "citadel",
        "halt",
        "halted",
        "rh",
        "robinhood"
      ),
      V2 = -1.5,
      V3 = 0.5
    )
  )


wsb_lexicon_wsb <- wsb_lexicon %>%
  as_tibble() %>%
  filter(!(V1 %in% wsb_lexicon$V1)) %>%
  bind_rows(wsb_lexicon) %>%
  as.data.frame()

wsb_lexicon <- wsb_lexicon_wsb

save(wsb_lexicon, file = "vader/R/sysdata.rda")

install.packages("vader/", repos = NULL, type = "source")

#Sentiment Estimations------
library(vader)

reddit_mentions <- read.csv(file = "data/stock_mentions.csv",
                          header = TRUE,
                          sep = ",",
                          dec = ".")

  vader <- reddit_mentions %>%
    select(text) %>%
    distinct() %>% 
    mutate(
      comment_clean = str_replace_all(text, "\\\\", " "
                                      )) %>%
    mutate(sentiment = vader_df(comment_clean)$compound)

reddit_mentions_sentiment <- reddit_mentions %>%
  left_join(vader %>% select(-comment_clean),
            by = "text")

reddit_mentions_sentiment <- select(reddit_mentions_sentiment, -X)
