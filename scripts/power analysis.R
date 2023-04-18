rm(list=ls())
library(simr)
library(tidyverse)

# Appendix S2 ----

# First set up some covariates with expand.grid.
# treatment <- rep(1:5)
# weight <- rnorm(n= 250, mean= 0.33, sd= 0.12)
# Site <- paste("Site_", 1:50, sep="")

df  <- data.frame(Site = paste("Site_", 1:50, sep=""), Treatment=rep(1:5), Weight = rnorm(n= 250, mean= 0.33, sd= 0.12))
str(df)

# Specify some fixed and random parameters.
b <- c(2, -0.1, 2.5, 0.3, 3.1, 0.9, 2, 0.7, 0.9, 1.5) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
V2 <- matrix(c(0.5,0.05,0.05,0.1), 2) # random intercept and slope variance-covariance matrix.
matrix.numbers <- round(runif(n=25, min = 0, max = 1), digits=2)
matrix.numbers <- round(runif(n=16, min = 0, max = 1), digits=2)
V3 <- matrix(matrix.numbers, 5) #In my case I have 5 categories (5x5 variance-covariance matrix)
s <- 1 # residual variance

# Build a model object
# Use the makeLmer or makeGlmer function to build an artificial lme4 object.

model1 <- makeLmer(y ~ Treatment + (1|Site), fixef=b, VarCorr=V1, sigma=s, data=df)
print(model1)

model2 <- makeGlmer(y ~ Treatment + (1|Site), family="binomial", fixef=b, VarCorr=V1, data=df)
print(model2)

# Now we have "pilot" models, which can be used with simr

powerSim(model2, nsim=20)

# Appendix S3:----

# Power analysis:
df.2 <- df %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Treatment= as.factor(Treatment))
str(df.2)

b <- c(2, -0.1, 1.9, 0.8, 1.7) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
V2 <- matrix(c(0.5,0.05,0.05,0.1), 2) # random intercept and slope variance-covariance matrix.
matrix.numbers <- round(runif(n=25, min = 0, max = 1), digits=2)
matrix.numbers <- round(runif(n=9, min = 0, max = 1), digits=2)
matrix.numbers <- round(runif(n=16, min = 0, max = 1), digits=2)
V3 <- matrix(matrix.numbers, 3) #In my case I have 5 categories (5x5 variance-covariance matrix)
s <- 1 # residual variance


model1 <- makeGlmer(y ~ Treatment + (1|Site), family="binomial", fixef=b, VarCorr=V1, data=df.2)
print(model1)
model1


f <- c(2, -0.1, 1.9, 0.8, 1.7, 2.5)
model2 <- makeGlmer(y ~ Treatment + Weight + (1|Site), family="binomial", fixef=f, VarCorr=V1, data=df.2)
print(model2)
summary(model2)

fixef(model2)["Treatment"] <- c(-0.05,  # here we indicate that we want to be able to detect an estimate of -0.05 for treatment
powerSim(model2)
pc2 <- powerCurve(model2, along = "Treatment", nsim=200) #calculate power curve, start with a small number of simulations and increase if it works fine
plot(pc)           

model3 <- extend(model2, along="")