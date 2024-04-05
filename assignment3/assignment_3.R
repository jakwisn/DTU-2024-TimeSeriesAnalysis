library(marima2)
library(ggplot2)

files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])


# Make plots
X <- read.table("data/experiment1.csv", sep=",", header=TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]


# 3.1

# Function for making a formula for lm

model <- marima("Tinner ~ AR(1) + Ta(1)", data=X)

summary(model)

residuals <- resid(model)

plot(residuals[1,])

# 3.2 

model <- marima("Tinner ~ AR(1) + Ta(1) + Touter(1) + MA(1) + Pinner(1) + Pouter(1)", data=X)

summary(model)

residuals <- resid(model)

plot(residuals[1,])

validate(model)

# 3.3 



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

ggplot(data=history_df, aes(x=input_added, y=rmse_drop)) 
  + geom_bar(stat="identity", orientation="h")
  + theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Therefore we pick inputs: AR, MA, Pinner

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





