library(marima2)
library(ggplot2)

files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])


# Make plots
X <- read.table("data/experiment1.csv", sep=",", header=TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]


############ 3.1 ############

# Function for making a formula for lm

model <- marima("Tinner ~ AR(1) + Ta(1)", data=X)
summary(model)
residuals <- resid(model)
plot(residuals[1,])
validate(model)

score(model)



############ 3.2 ############ 




# So we should have a burn-in, e.g. of 5 steps (in priciple the max order of the models)
score <- function(fitMarima, n, p, nburnin, penalty=2){
  res <- fitMarima$residuals[1, ]
  res <- res[!is.na(res)]
  sd <- sqrt(sum((res - mean(res))^2) / length(res) )
  loglik <- sum(log(dnorm(res, sd=sd)))
  loglik
  # Find the number of parameters
  p <- sum(fitMarima$ar.estimates[ , ,-1] != 0) + sum(fitMarima$ma.estimates[ , ,-1] != 0)
  p <- p + 1
  # AIC
  2*p - 2*loglik
}  

inputs <- c("AR", "MA", "Pinner", "Ta", "Touter","Pouter")
desc <- "Tinner ~ "
i <- 1
aic_history <- c()
desc_history <- c()


for (input in inputs){
  if (i == 1){
    desc <- paste(desc, input, "(1)", sep="")
  }
  else{
    desc <- paste(desc, " + ", input, "(1)", sep="")
  }
  
  model <- marima(desc, data=X)
  
  aic <- score(model, n=nrow(X), p=length(model$coefficients)+1, nburnin=1)
  
  print(desc)
  print(aic)
  
  aic_history[i] <- aic 
  desc_history[i] <- input
  
  i <- i+1
}

history_df <- data.frame(AIC = aic_history, 
                         input_added = desc_history)

history_df

ggplot(data=history_df, aes(x=input_added, y=AIC))+
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + 
  xlab("Sequential inputs added")
  

model <- marima("Tinner ~ AR(1) + Pinner(1) + MA(1) ", data=X)

summary(model)
residuals <- resid(model)
validate(model)

plot(residuals[1,])

## using built in step 

model <- marima("Tinner ~ AR(1) + Ta(1) + Touter(1) + Pinner(1) + MA(1)", data=X, penalty = 2)

summary(model)
score(model, n=nrow(X), p=length(model$coefficients)+1, nburnin=1)

############ 3.3 ############ 


# the RMSE did not go down much with addition of Touter and Pouter

results <- c()
results2 <- c()

for (i in 1:20){
  order <- i
  
  desc <- paste("Tinner ~ AR(1:", as.character(order), ")",
                "+ Pinner(1:", as.character(order), ")", 
                "+ MA(1:", as.character(order), ")", 
          sep="")
  
  
  model <- marima(desc, data=X)
  
  aic <- score(model, n=nrow(X), p=length(model$coefficients)+1, nburnin=i)
  results[i] <- aic 
}

history_df <- data.frame(AIC = results, 
                         input_added = c(1:20))

ggplot(data=history_df, aes(x=input_added, y=AIC))+
  geom_line() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


history_df

plot(results, type="l")


# and lags should start from 4! 

# Our best model is 

model <- marima("Tinner ~ AR(1:4) + Pinner(1:4) + MA(1:4)", data=X)
score(model, n=nrow(X), p=length(model$coefficients)+1, nburnin=i)

model <- marima("Tinner ~ AR(1:4) + Pinner(1:4) + MA(1:4)", data=X)
# second best 

fit_tinner <- marima("Tinner ~ AR(1:10) + Pinner(1:10) + MA(1:10) + Ta(1:10) + Touter(1:10) + Pouter(1:10)", data=X, penalty=2)
score(fit_tinner, n=nrow(X), p=length(model$coefficients)+1, nburnin=i)


summary(fit_tinner)

validate(fit_tinner)

############ 3.4 ############

# discussion based on the ARX and ARMAX 

############ 3.5 ############

# Make a multi-step prediction with the selected model. 
# Can it predict the temperature through
# out the exeriment?

# We can use the predict function to predict the future values of the time series.


#model <- marima("Tinner ~ AR(1) + Pinner(1) + MA(1)", data=X)

val <- predict(model, nstep=nrow(X)-1)

plot(X$Tinner)
lines(val$forecasts[1, ], type="l", col=2)


############ 3.6 ############

## armax-simulate-step-response
input <- "Pinner"
Xs <- X[ ,2:6]
#Xs[ ,2:6] <- 0
Xs$Pinner <- 0
Xs$Pouter <- 0
#
Xs$Tinner <- 20
Xs$Touter <- 20
Xs$Ta <- 20
Xs[-1:-3,input] <- Xs[-1:-3, input] + 100
#
val <- predict(model, Xs, nstep=nrow(X)-1)
#plot(M$data[ ,input], type="l")
yhat <- val$forecasts[1, ]
se <- sqrt(val$pred.var[1,1, ])
plot(yhat, type="l", main="Step response of the model")
lines(yhat[-1] - qnorm(0.975)*se, lty=2)
lines(yhat[-1] + qnorm(0.975)*se, lty=2)

### simulating longer time scale 

input <- "Pinner"
Xs <- X[ ,2:6]
#Xs[ ,2:6] <- 0
Xs$Pinner <- 0
Xs$Pouter <- 0
#
Xs$Tinner <- 20
Xs$Touter <- 20
Xs$Ta <- 20

# taking 50 steps 
Xs[-1:-50,input] <- Xs[-1:-50, input] + 100

# add rows to data
Xl <- rbind(Xs,
            Xs[50:length(Xs$Pinner),], 
            Xs[50:length(Xs$Pinner),],
            Xs[50:length(Xs$Pinner),],
            Xs[50:length(Xs$Pinner),]
)


#
val <- predict(model, Xl, nstep=nrow(Xl)-1)
#plot(M$data[ ,input], type="l")
yhat <- val$forecasts[1, ]
se <- sqrt(val$pred.var[1,1, ])
plot(yhat, type="l", main="Step response of the model")
lines(yhat[-1] - qnorm(0.975)*se, lty=2)
lines(yhat[-1] + qnorm(0.975)*se, lty=2)

# Not a good physical model for the system. The curve resembles the log curve 
# and does not go to the steady state.






