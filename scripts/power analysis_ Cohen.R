rm(list=ls())
library(here)
library(pwr)

# The effect size used here is f^2^ that has be categorized as follows (see Cohen 1988): 
# small ??? 0.02, medium ??? 0.15, and large ??? 0.35. So in order to determine if the data is sufficient
# to find a weak effect when comparing 3 groups with 40 plants in each group 
# (df_numerator_: 3-1; df_denominator_: (40-1) + (40-1)) and a significance 
# level at ?? = .05, we can use the following code:

# With a false positive error rate of 20% (power=0.8):

# small effect (The difference between 2 groups' means is less than 0.2)

r_power.small <- pwr.f2.test(u = 2, v = 72, f2 = 0.2, sig.level = 0.05, power =NULL)
r_power.small

plot(r_power)

# As a general rule of thumb, we want a data set that allows a model to find a medium sized
# effect with at least an accuracy of 80 percent (Field et al. 2007).

r_power.medium <- pwr.f2.test(u = 2, v = NULL, f2 = 0.5, sig.level = 0.05, power = 0.8)
r_power.medium


r_power.large <- pwr.f2.test(u = 2, v = NULL, f2 = 0.8, sig.level = 0.05, power = 0.8)
r_power.large
