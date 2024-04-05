library(marima2)

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

model <- marima("Tinner ~ AR(1) + Ta(1) + MA(1)", data=X)

summary(model)

residuals <- resid(model)

plot(residuals[1,])

validate(model)

# 3.3 

results <- c()

for (i in 1:20){

order <- i

desc <- paste("Tinner ~ AR(1:", as.character(order), ")",
              "+ Ta(1:", as.character(order), ")", 
              "+ MA(1:", as.character(order), ")", 
      sep="")


model <- marima(desc, data=X)

residuals <- resid(model)[1, ]
r <- residuals[(order+1):length(residuals)]

results[i] <- sqrt(sum(r**2))/length(r)

}

plot(results, type="l")



summary(model)
summary(step(model))
validate(model)
# ----------------------------------------------------------------
# The AIC
AIC(model)
