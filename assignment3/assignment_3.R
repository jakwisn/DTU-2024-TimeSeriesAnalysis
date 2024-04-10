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

############ 3.2 ############ 

model <- marima("Tinner ~ AR(1) + Ta(1) + Touter(1) + MA(1) + Pinner(1) + Pouter(1)", data=X)

summary(model)

residuals <- resid(model)

plot(residuals[1,])

validate(model)

############ 3.3 ############ 

rmse <- function(residuals, count_from=20){
  r <- residuals[(count_from+1):length(residuals)]
  return(sqrt(sum(r**2))/length(r))
}



# select the inputs 

inputs <- c("AR", "MA", "Ta", "Touter", "Pinner", "Pouter")
desc <- "Tinner ~ "
i <- 1
rmse_history <- c()
desc_history <- c()
for (input in inputs){
  if (i == 1){
    desc <- paste(desc, input, "(1)", sep="")
  }
  else{
    desc <- paste(desc, " + ", input, "(1)", sep="")
  }
  
  model <- marima(desc, data=X)
  print(desc)
  #print(summary(model))
  residuals <- resid(model)[1,]
  print(rmse(residuals))
  
  rmse_history[i] <- rmse(residuals)
  desc_history[i] <- input
  #plot(residuals[1,])
  #validate(model)
  
  i <- i+1
}

# the RMSE did not go down much with addition of Touter and Pouter

rmse_history
history_change <- rmse_history[1:(length(rmse_history)-1)] - rmse_history[2:length(rmse_history)]

desc_history

history_df <- data.frame(rmse_drop = history_change, 
                        input_added = desc_history[2:length(desc_history)])

history_df

ggplot(data=history_df, aes(x=input_added, y=rmse_drop))+
  geom_bar(stat="identity", orientation="h") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Therefore we pick inputs: AR, MA, Pinner

results <- c()
results2 <- c()

for (i in 1:20){
  order <- i
  
  desc <- paste("Tinner ~ AR(1:", as.character(order), ")",
                "+ Pinner(1:", as.character(order), ")", 
                "+ MA(1:", as.character(order), ")", 
        sep="")
  
  
  model <- marima(desc, data=X)
  
  resid(model)[2, ]
  
  # Extract residuals
  residuals <- resid(model)[1, ]
  r <- residuals[(order+1):length(residuals)]
  r2 <- residuals[21:length(residuals)]
  
  # RMSE 
  results[i] <- sqrt(sum(r**2))/length(r)
  results2[i] <- sqrt(sum(r2**2))/length(r2)
}

plot(results, type="l")
plot(results2, type="l")

# and lags should start from 3! 

# Our best model is 

model <- marima("Tinner ~ AR(1:3) + Pinner(1:3) + MA(1:3)", data=X)

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






