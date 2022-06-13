#http://www.flutterbys.com.au/stats/tut/tut10.5a.html

library(tidyverse)
source('~/Dropbox/Papers/theme_juan.R')
load("~/Dropbox/Papers/1 In progress/david_virus/data/roug_dat.RData")

str(dat)
# View(dat)

dat %>% 
  ggplot(aes(x = days, y = inf_plants/n_plants, col=type))+
  geom_point()+  
  geom_smooth(method = "glm", se = F, fullrange=FALSE, size=0.6,
              method.args = list(family = "binomial"))+
  scale_y_continuous(breaks=seq(0,1,.2), labels = function(x) paste0(x*100))+
  labs(x="Days after inoculation", y ="Infected plants (%)", 
       linetype = "Treatment", shape = "Treatment")+
  facet_grid(year ~ location)

# modeling packages ----
library(car)
library(epiDisplay)
library(sjPlot)
library(DHARMa)
library(gmodels)

# SP 2013 ----

dat %>% 
  filter(exp==1) %>% 
  # ggplot(aes(x=inf_plants))+ geom_histogram()
  glm(inf_plants/n_plants ~ days*type,
      weights=n_plants, 
      family="binomial", 
      data=.) -> sp_2013 

Anova(sp_2013)
summary(sp_2013)
plot_model(sp_2013, type = "pred", terms = c("days", "type"))
logistic.display(sp_2013, decimal = 2, simplified=TRUE)
cm1 <- rbind('Check vs Arbor'  = c(0, 0, 1, 1))
estimable(sp_2013, cm1)

# library(broom)
# tidy(sp_2013)
# glance(sp_2013)

# Diagnostics
simulationOutput <- simulateResiduals(fittedModel = sp_2013)
plot(simulationOutput, quantreg = FALSE)
testDispersion(simulationOutput, alternative = "less") # underdispersion
testZeroInflation(simulationOutput)
1 - (sp_2013$deviance/sp_2013$null)
plot(resid(sp_2013) ~ fitted(sp_2013))
lines(lowess(resid(sp_2013) ~ fitted(sp_2013)))

# SP 2016 ----
dat %>% 
  filter(exp==2) %>% 
  glm(inf_plants/n_plants ~ days*type,
      weights=n_plants, 
      family="binomial", 
      data=.) -> sp_2016

summary(sp_2016)
car::Anova(sp_2016)
logistic.display(sp_2016, decimal = 2, simplified=TRUE) #exp(coef(sp_2016))

cm2 <- rbind(
  'Check vs Arbor'   = c(0, 0, 1, 0, 1, 0),
  'Check vs Trellis' = c(0, 0, 0, 1, 0, 1), 
  'Arbor vs Trellis' = c(0, 0, 1, -1, 1, -1))
estimable(sp_2016, cm2)

# BA ------

dat %>%
  filter(exp==3 & days<181) %>%
  # ggplot(aes(x = days,
  #            y = inf_plants/n_plants,
  #            linetype=type, shape=type))+
  # geom_point(aes(shape=type))+
  # geom_smooth()
  # geom_smooth(method = "glm", se = F, fullrange=TRUE, size=0.8,
  #             method.args = list(family = "binomial"), col="black")

  glm(inf_plants/n_plants ~ days*type,
      weights=n_plants,
      family="binomial"(link = "logit"),
      data=.) -> ba_2013

car::Anova(ba_2013)
summary(ba_2013)
plot_model(ba_2013, type = "pred", terms = c("days", "type"))
logistic.display(ba_2013, decimal = 2, simplified=TRUE)

cm3 <- rbind('Check vs Arbor'  = c(0, 0, 1, 1))
estimable(ba_2013, cm3)

dat %>% 
  filter(exp==4) %>% 
  glm(inf_plants/n_plants ~ days*type,
      weights=n_plants, 
      family="quasibinomial", 
      data=.) -> ba_2014

car::Anova(ba_2014)
summary(ba_2014)
cm4 <- rbind(
  'Check vs Arbor'  = c(0, 1, 0, 1))
estimable(ba_2014, cm4)

plot_model(ba_2014, type = "pred", terms = c("days", "type"))
logistic.display(ba_2014, decimal = 2, simplified=TRUE)

dat %>% 
  filter(exp==5) %>% 
  glm(inf_plants/n_plants ~ days*type,
      weights=n_plants, 
      family="binomial", 
      data=.) -> ba_2016

summary(ba_2016)
car::Anova(ba_2016)

cm5 <- rbind(
  'Check vs Arbor'   = c(0, 0, 1, 0, 1, 0),
  'Check vs Trellis' = c(0, 0, 0, 1, 0, 1), 
  'Arbor vs Trellis' = c(0, 0, 1, -1, 1, -1))
estimable(ba_2016, cm5)

plot_model(ba_2016, type = "pred", terms = c("days", "type"))
logistic.display(ba_2016, decimal = 2, simplified=TRUE)

# dat %>% 
#   filter(exp==5) %>% 
#   mutate(type = fct_relevel(type, "Arbor", "Trellis","Check")) %>%
#   glm(inf_plants/n_plants ~ days*type,
#       weights=n_plants, 
#       family="binomial", 
#       data=.) -> ba_2016b
# summary(ba_2016b)
# 
# cm6 <- rbind(
#   'Arbor vs Trellis'= c(0, 0, 1, 0, 1, 0),
#   'Arbor vs Check' = c(0, 0, 0, 1, 0, 1))
# estimable(ba_2016b, cm6)

save(sp_2013, sp_2016, ba_2013, ba_2014, ba_2016, file="roug_models.Rdata")
