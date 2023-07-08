library(tidyverse)
library(recipes)
library(caret)
library(ggplot2)
library(dplyr)
library(xts)
library(MTS)
library(tseries)
library(forecast)
library(vars)
library(lmtest)
r50 <- read.csv(file = "data/r50.csv",
                           header = TRUE,
                           sep = ",",
                           dec = ".")

r50$count <- rep(1, nrow(r50))
r50 <- r50 %>% na.omit()

r50 <-  r50 %>%
    filter(abs(sentiment) > 0.2) %>%
    na.omit()

r50$timestamp <- as.Date(r50$timestamp)


r50 <- r50 %>%
  group_by(timestamp) %>%
  summarise(Sent.agg = sum(sentiment),
            Sent.var = var(sentiment),
            Upsent.avg = mean(sentiment * comms_num * score),
            Upsent.var = var(sentiment * score * comms_num),
            mentions.count = sum(count),
            r = mean(r),
            logr = mean(logr),
            Dvol = mean(Dvol),
            logDvol = mean(logDvol))

summary(r50)

r50$Wsent <- r50$Sent.agg * r50$mentions.count

# Replace Inf in data by NA
r50 <- do.call(data.frame,
                lapply(r50,
                  function(x)
                  replace(x,is.infinite(x), NA)))
# Center and scale
recipe <-
  recipe(~., data = r50) %>%
  step_center(all_numeric())  %>%
  step_scale(all_numeric())

recipe

recipe <- prep(recipe, training = r50)

r50_clean <- bake(recipe, r50)

# Stationarity and Granger Test --------------

r50_clean <- na.omit(r50_clean[-c(1, 6, 7, 9)])

adf.test(r50_clean$logr)
adf.test(r50_clean$logDvol)
adf.test(r50_clean$Sent.agg)
adf.test(r50_clean$Sent.var)
adf.test(r50_clean$Upsent.avg)
adf.test(r50_clean$Upsent.var)
adf.test(r50_clean$Wsent)


#Train/Test split ---------------
idx <- round(nrow(r50_clean) * 0.95)

train <- head(r50_clean, idx) %>% na.omit()
test <- tail(r50_clean,
            nrow(r50_clean) - idx) %>%
            na.omit()

#VAR
train_logr <- train[-c(6)]
train.logDvol <- train[-c(5)]

#VARX Exogenous
X <- train[c(1, 2, 3, 4, 7)]
Xt <- test[c(1, 2, 3, 4, 7)] 
#VARX Endogenous
Y <- train[c(5, 6)]
Yt <- test[c(5, 6)]

#VAR-logr-----------------------
VARselect(train_logr)
VARorder(train_logr, 10)
VARorderI(train_logr, 10)

m1.logr <- VAR(train_logr, p = 5,  type = "none")
summary(m1.logr)

m2.logr <- restrict(m1.logr, thresh = 2)
summary(m2.logr)

irf.logr<- irf(m2.logr, response = "logr")
plot(irf.logr)

grangertest(train$Sent.agg, train$logr,  5)
grangertest(train$Upsent.avg, train$logr,  5)

#VAR-logDvol-----------------------
VARselect(train.logDvol)
VARorder(train.logDvol, 10)
VARorderI(train.logDvol, 10)

m1.logDvol <- VAR(train.logDvol, p = 5,  type = "none")
summary(m1.logDvol)

m2.logDvol <- restrict(m1.logDvol, thresh = 2)
summary(m2.logr)

irf.logDvol<- irf(m2.logDvol, response = "logr")
plot(irf.logr)

grangertest(train$Sent.agg, train$logDvol,  5)
grangertest(train$Upsent.avg, train$logDvol,  5)

#VARX model---------------

VARselect(Y, exogen = X)

x1 <- VAR(Y, 1, exogen = X, type = "none" )
summary(x1)

x2 <- restrict(x1, thresh = 1)
summary(x2)

pred <- predict(x2, n.ahead = 6, dumvar = Xt)


