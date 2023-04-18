rm(list=ls())
library(here)
library(simr)
library(tidyverse)
library(lme4)
library(effects)
library(faux)
library(DHARMa)
library(glmmTMB)

# Appendix S2 ----

# First set up some covariates with expand.grid.
# treatment <- rep(1:5)
# weight <- rnorm(n= 250, mean= 0.33, sd= 0.12)
# Site <- paste("Site_", 1:50, sep="")

raw.data  <- read.csv2(here("data", "pilot2022.csv"))
str(raw.data)

# Assign a category to each treatment (0: 0 - 6 cm, 1 = 8- 15 cm, 2: 18-23 cm). Replace "vet ej" 
# values for the purposes of the analysis with "Ja".

data <- raw.data %>% 
  mutate(Lokal=as.factor(Lokal)) %>%
  mutate(Sträcka= as.factor(Sträcka)) %>% 
  mutate(Behandling = case_when(
    Behandling.djup..cm. <= 6 ~ 0,
    Behandling.djup..cm. <= 15 ~ 1, 
    Behandling.djup..cm. <= 23 ~ 2)) %>% 
  mutate(Behandling = as.factor(Behandling)) %>% 
  rename(Vikt = Vikt.grön...g.) %>% 
  rename(regrew = Kom.tillbaka.höst.2022) %>% 
  mutate(regrew = replace(regrew, regrew =="Vet ej", "Ja")) %>% 
  mutate(regrew = as.factor(regrew)) %>% 
  mutate(plantid = row_number())
str(data)

# Specify some fixed and random parameters:
# Fixed: behandling, vikt
# För simlurenigen tillade jag "sträcka" som random effect som om det vore lokalen.

b <- c(2, -0.1, 1.9, 0.8, 1.7) # fixed intercept and slope
V1 <- 0.5 # random intercept variance
s <- 1 # residual variance

# Build a model object
# Use the makeLmer or makeGlmer function to build an artificial lme4 object.
# Ideal model structure would be: plantid as a random effect, because I assume each plant has its own characteristics (t.ex. plant with
# a thicker root could have a higher chance of surviving), and site as well because I would assume plants in a site could be more
# similar to each other. So ideally, plantid nested in site. 
# Power analysis:

model1 <- glm(regrew ~ Behandling + Vikt,  family="binomial",  data=data)
summary(model1)
mod_dharma1 <- model1 %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plot(allEffects(model1))


# model.x <- glmmTMB(regrew ~ Behandling + Vikt + (1|plantid),  family=nbinom1,  data=data)
# summary(model.x)
# mod_dharma1 <- model.x %>% simulateResiduals(n=1000)
# plot(mod_dharma1)
# plot(allEffects(model.x))

model1 <- makeGlmer(respons ~ Behandling + Vikt + (1|Sträcka), family="binomial", fixef=f, VarCorr=V1, data=data)
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
