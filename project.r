market <- read.csv("market_index_clean.csv", header=TRUE)
appl2 <- market$AAPL[1:3103]
appl <- market$AAPL[2:3104]
spx <- market$SPX[2:3104]
vix <- market$VIX[2:3104]
spgscitr <- market$SPGSCITR[2:3104]
bndglb <- market$BNDGLB[2:3104]
eem <- market$EEM[2:3104]

fit1 <- lm(appl~appl2)
# summary(fit1)
# plot(fitted(fit1), fit1$residuals)
## fan-shaped plot of residuals against fitted

return <- (appl - appl2)/appl2
# par(mfrow=c(1,2))
# plot(return)
# plot(market$Date, market$AAPL)

nullmodel <- lm(return~1)
fullmodel <- lm(return~appl+spx+vix+spgscitr+bndglb+eem)
step(nullmodel,scope=list(upper=fullmodel),direction="both")