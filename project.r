## setup --------------------------------------------------------------------------------
## this part creates a new variable called "return"
## and adds it to the data frame
market <- read.csv("market_index_clean.csv", header=TRUE)
appl2 <- market$AAPL[1:3103]
appl <- market$AAPL[2:3104]
return <- c(NA)
return <- c(return, (appl - appl2)/appl2)
market <- cbind(market, return)
# market <- market[2:3104,] ## gets rid of first row because return = NA for day 0

### Automatic model selection -----------------------------------------------------------
## error message "attempting model selection on an essentially perfect fit is nonsense"
nullmodel <- lm(return~1, data = market)
fullmodel <- lm(return~., data = market)
step(nullmodel,scope=list(upper=fullmodel),direction="both")


## Autoregressive model -----------------------------------------------------------------
dev.off()
auto_regressive_model <- lm(appl~appl2)
## WLS Autoregressive Model
wts <-1/(fitted(lm(abs(residuals(auto_regressive_model))~fitted(auto_regressive_model)))^2)
wls <- lm(appl~appl2, weights = wts)
par(mfrow=c(1,2))
plot(fitted(auto_regressive_model), auto_regressive_model$residuals)
plot(fitted(wls), wls$residuals)
## plots show no apparent change...interesting...

## Full Model ------------------------------------------------------------------
summary(fullmodel)
## both creating and displaying the model takes a LONG times
## conclusion: date variable should not be included as an independent variable
##    but should be treated as an observation index instead
fullmodel <- lm(return~AAPL+SPX+VIX+SPGSCITR+BNDGLB+EEM, data = market)
summary(fullmodel)
## intercept is suprisingly highly significant
## the model with only the intercept term in it is the 
##    null model (defined above)
## hence it's a case when the return rate is only dependent on the
##    time series (i.e. date), which makes sense since the apple stock price
##    is generally going up since it was listed

## Model with only VIX --------------------------------------------------------
VIX_only <- lm(return~VIX, data = market)
summary(VIX_only)

## Modified Automated Model Selection
nullmodel <- lm(return~1, data = market)
step(nullmodel,scope=list(upper=fullmodel),direction="both")
## THIS RETURNS REALLY GOOD RESULTS WITH NO TIMEOUT!!