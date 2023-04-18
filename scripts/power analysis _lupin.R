rm(list=ls())
library(simr)
library(tidyverse)

# Appendix S2 ----

# First set up some covariates with expand.grid.
# treatment <- rep(1:5)
# weight <- rnorm(n= 250, mean= 0.33, sd= 0.12)
# Site <- paste("Site_", 1:50, sep="")

df  <- data.frame(Site = paste("Site_", 1:10, sep=""), Treatment=rep(1:5), Weight = rnorm(n= 500, mean= 0.33, sd= 0.12))
str(df)

# Specify some fixed and random parameters.
# Build a model object
# Use the makeLmer or makeGlmer function to build an artificial lme4 object.
# Power analysis:
df.2 <- df %>% 
  mutate(Site = as.factor(Site)) %>% 
  mutate(Treatment= as.factor(Treatment))
str(df.2)

b <- c(2, -0.1, 1.9, 0.8, 1.7) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
s <- 1 # residual variance


f <- c(2, -0.1, 1.9, 1.8, 1.7, 2.5)
model1 <- makeGlmer(y ~ Treatment + Weight + (1|Site), family="binomial", fixef=f, VarCorr=V1, data=df.2)
print(model1)
summary(model1)

powerSim(model1)
pc1 <- powerCurve(model1, along = "Treatment", nsim=200) #calculate power curve, start with a small number of simulations and increase if it works fine
plot(pc1)           

model2 <- extend(model3, along = "Site", n=30)
pc2 <- powerCurve(model3, along = "Site", nsim=200)  # lets increase number of plants per treatment /site
plot(pc2)

model3 <- extend(model1, along = "Site", n=20)
pc3 <- powerCurve(model1, along = "Site", nsim=200)  # lets increase number of plants per treatment /site
plot(pc3)
