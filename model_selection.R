## Date Transformation----------------------------------------------------------
market <- read.csv("market_index_clean.csv", header=TRUE)
appl2 <- market$AAPL[1:3103]
appl <- market$AAPL[2:3104]
return <- c(NA)
return <- c(return, (appl - appl2)/appl2)
return_standard <- c(NA)
return_standard <- c(return_standard, return[2:3104]/sqrt(market$VIX[1:3103]))
market <- cbind(market, return)
market <- cbind(market, return_standard)
library(lubridate)
market$Date <- market_origin$Date[2:3104]
date <- market$Date
first <- date[1]
day <- c()
for(i in 1:length(date)) {
  diff = round(difftime(date[i], first))
  day = c(day, diff)
}
market$Date <- day
fit <- lm(return~., data = market)
summary(fit)

nullmodel <- lm(return~1, data = market)
fullmodel <- lm(return~., data = market)
step(nullmodel,scope=list(upper=fullmodel),direction="both")
step(nullmodel,scope=list(upper=fullmodel),direction="forward")
step(fullmodel,scope=list(upper=nullmodel),direction="backward")


back_model <- lm(return ~ SPX + VIX + SPGSCITR + BNDGLB + EEM, data = market)
forward_model <- lm(return ~ VIX + SPX + SPGSCITR, data = market)
summary(back_model)
summary(forward_model)

plot(market$Date, return)
plot(fitted(forward_model), residuals(forward_model))
plot(fitted(back_model), residuals(back_model))
