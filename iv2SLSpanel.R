library(tidyverse)
library(tseires)

#load ivreg package for IV and 2SLS
library(ivreg)

#we can integrate two stages into one command
2sls <- ivreg(formular = y ~ x1 + x2 + x3,
              instruments = ~ z1 + x2 + x3,
              data = d99)
print(2sls)

#or two separate commands w/o ivreg package
s1 <- lm(
  x1 ~ z1 + x2 + x3,
  data = d99
)

print(s1)

x1_hat <- predict(stage1) #predict can be used to predict endogenous variable
s2 <- lm(
  y ~ x1_hat + x2 + x3,
  data = d99
)

summary(s2)

#Since ivreg automatically reports diagnostic tests, we should use ivreg for convienience

#Panel data, FE, RE
FE <- plm(y ~ x1 + x2 + x3, data = d1, effect = "individual", model = "within")

RE <- plm(y ~ x1 + x2 + x3, data = d1, effect = "individual", model = "random")

#though pooled is not usually used, show it here
Pooled <- plm(y ~ x1 + x2 + x3, data = d1, effect = "individual", model = "pooled")

#effect "individual" means we take individual specific error out, also called time inviriant error


