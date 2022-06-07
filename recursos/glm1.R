library(lme4) 
library(RVAideMemoire)
Data$obs <- factor(formatC(1:nrow(Data), flag="0", width = 3))
model.glmm <- glmer(cbind(number_pres,number_abs) ~ Var1+Var2+Var3+Var4...+
                      (1|obs),family = binomial (link = logit),data = Data) 
overdisp.glmer(model.glmm) #Overdispersion for GLMM