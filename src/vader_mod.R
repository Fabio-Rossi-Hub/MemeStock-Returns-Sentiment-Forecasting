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

install.packages("src/vader/", repos = NULL, type = "source")