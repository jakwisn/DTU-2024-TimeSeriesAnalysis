## Source the files in the "functions" folder
files <- dir("functions",full.names=TRUE)
for(i in 1:length(files)) source(files[i])


# Make plots
X <- read.table("experiment1.csv", sep=",", header=TRUE)
# Convert to t_1 = 0
X$t <- X$t - X$t[1]

par(mfrow=c(5,1), mgp=c(2.2,0.7,0), mar=c(3,3,1,1))
plot(X$t, X$Ta)
plot(X$t, X$Tinner)
plot(X$t, X$Touter)
plot(X$t, X$Pinner)
plot(X$t, X$Pouter)


## ccfs
ccf(X$Tinner, X$Pinner, lag.max=50, main="")


# ----------------------------------------------------------------
# Use the lagdf to make lags
lagdf(X[ ,2:6], 1)

# Add the lags to X
maxlag <- 10
for(i in 1:maxlag){
  tmp <- lagdf(X[ ,2:6], i)
  names(tmp) <- paste0(names(tmp),".l",i)
  X <- cbind(X, tmp)
}

# Fit an ARX
summary(lm("Tinner ~ Tinner.l1 + Pinner.l1 + Touter.l1", data=X))

# ----------------------------------------------------------------
# Function for making a formula for lm
ARX("Tinner", c("Touter"), 1)
ARX("Tinner", c("Touter","Pouter"), 1:10)

# Fit and print
fit <- lm(ARX("Tinner", c("Touter"), 1), data=X)
summary(fit)


# ----------------------------------------------------------------
# Validation plot
validate(fit)

# ----------------------------------------------------------------
# CCF plot
ccfplot(fit, X)

# ----------------------------------------------------------------
# Do you know the stepping function?
fit <- lm(ARX("Tinner", c("Touter"), 1:5), data=X)
summary(fit)
summary(step(fit))

# ----------------------------------------------------------------
# The AIC
AIC(fit)

# ----------------------------------------------------------------
# Use this to find a suitable ARX model and use it



# ----------------------------------------------------------------
# Use a modified version of the marima package!
# Install from file
download.file("https://02417.compute.dtu.dk/material/marima2_0.1.tar.gz", "marima2_0.1.tar.gz")
install.packages("marima2_0.1.tar.gz", repos=NULL)
library(marima2)


# ----------------------------------------------------------------
# ARMAX model example
# Note MARINA don't need the lagged (it makes them internally)
X <- X[ ,1:6]
names(X)
fit <- marima("Tinner ~ AR(1:2) + Touter(1:2) + MA(1:2)", data=X)

summary(fit)

validate(fit)


# ----------------------------------------------------------------
# Stepping in marima (always be careful...but very useful in many situations)
fit <- marima("Tinner ~ AR(1:10) + Touter(1:10) + Pinner(1:10) + MA(1:10)", data=X, penalty=2)
summary(fit)



# ----------------------------------------------------------------
# Multi-step forecasts
val <- predict(fit, nstep=nrow(X)-1)
plot(X$Tinner)
lines(val$forecasts[1, ], type="l", col=2)


# ----------------------------------------------------------------
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
Xs[-1:-10,input] <- Xs[-1:-10,input] + 100
#
val <- predict(fit, Xs, nstep=nrow(X)-1)
#plot(M$data[ ,input], type="l")
yhat <- val$forecasts[1, ]
se <- sqrt(val$pred.var[1,1, ])
plot(yhat, type="l")
lines(yhat[-1] - qnorm(0.975)*se, lty=2)
lines(yhat[-1] + qnorm(0.975)*se, lty=2)
# ----------------------------------------------------------------

# ----------------------------------------------------------------
## Steady state gain (DC-gain)
gain.marima <- function(fit, output, input){
  tbl <- summary(fit)[[output]]
  -sum(tbl[grep(input,tbl$Name), "Estimate"]) / sum(1,tbl[grep("AR",tbl$Name), "Estimate"])
}
gain.marima(fit, "Tinner", "Pinner")


# ----------------------------------------------------------------
## Multi-output model (coupled system)
ARMAX <- marima("Tinner ~ AR(1) + Touter(1) + MA(1)",
                "Touter ~ AR(1) + Tinner(1) + MA(1)", data=X,
                penalty=0)
summary(ARMAX)
validate(ARMAX)
#-----------------------------------------------------------------
library(marima2)
##############################3.1############################

##############################3.1.fixed#########################################

df <- read.table("experiment1.csv", sep=",", header=TRUE)
# Assuming Ti,t (Tinner) is your dependent variable and Ta,t (Ta) is the independent variable
# Define the ARX model of order 1 with ambient temperature as the independent variable
ar <- c(1)  # AR component of order 1
ma <- c(0)  # Excluding the MA component since we're focusing on ARX

# Define the model with adjusted parameters
# Assuming Ti,t is the first column and Ta,t is the second column in your dataframe
Model1 <- define.model(kvar=2, ar=ar, ma=ma, rem.var=1, indep=2)

# Estimate the model using marima.....IT FAILS AT THIS PART!
Marima1 <- marima(df, means=1, 
                  ar.pattern=Model1$ar.pattern, 
                  ma.pattern=Model1$ma.pattern, 
                  Check=FALSE, Plot=FALSE, penalty=0.0)
# Output the summary of the model estimation
summary(Marima1)

# Analyzing the residuals
residuals <- residuals(Marima1)

# Plotting the residuals to visually inspect their behavior
plot(residuals, type = 'p', main = 'Residuals of ARX(1) Model', ylab = 'Residuals', xlab = 'Time')

# For validation, check the residuals
residuals <- residuals(model)

# Plot residuals to visually inspect their behavior
plot(residuals, type = 'p', main = 'Residuals of ARX(1) Model', ylab = 'Residuals', xlab = 'Index')






