https://www.youtube.com/watch?v=DDP62EUMRFs

#####  Case Study 1:  See SAS file for details on the data,experimental design, and
#####  the different models. Just some key results in R are shown. 
#####  Some code is borrowed from Walter Stroup (Univ. Nebraska).
#####
#####  Program is provided for educational purposes, and supplied without warranty.
#####  (Larry Madden [Ohio State] and Alissa Kriss [Syngenta]  July 2016)


## Packages of relevance for the following analyses. One may need to first install 
## these from CRAN. Not all are used in the example runs.
library(car)
library(lme4)
library(effects)
library(emmeans)
library(msm)
library(pbkrtest)
library(MASS)
library(gee)


## set working directory and read in table of data from txt file
M<-read.table("phomopsisp.txt")

## name the columns with data, mostly agreeing with SAS code
colnames(M)<- c("treat", "block", "Y", "n")

## print table to check that one has the right data
M

## identify block and treat as FACTORS (class variables) for analysis
M$block <- factor(M$block)
M$treat <- factor(M$treat)


## Now fit some mixed models. 
## Start with a LMM (proportion as normal), block as random
## One needs to use three functions: lmer, anova for type 3 tests, and lsmeans for means

model1r <-lmer(Y/n ~ treat + (1|block), data=M)
model1r  # print results
Anova(model1r,type="III") # anova function to get type 3 hypothesis test for treatment,...
emmeans(model1r, pairwise~treat, adjust="none")  # get means, differences, and SEs


## GLMM for conditional binomial data (naive GLMM, borrowing from LMM ideas)
## No unit-level random term for plot (i.e., block:treat interaction).
## One needs to use cbind(,) here in model to identify Y and n-Y 
## Note that one gets chi-squared statistics for Type 3 hypothesis tests, not F (or t) tests
## In SAS/GLIMMIX, add the " / chisq" option to model statement to get chi-squares.
## logit link (not listed) is default for binomial
## Default estimation with glmer (displayed in output) is Laplace for MLE estimation
## With SAS/GLIMMIX, one needs method=laplace for this estimation method
##
## In output for glmer, the square-root of random effect variances 
## (standard deviations) are displayed. Square these values to see estimated variances.

resp <- with(M, cbind(Y, n-Y)) 

model9 <- glm(resp ~ treat + block, family="binomial", data=M)
library(hnp)
hnp(model9)

model9<- glmer(resp ~ treat + (1|block), family="binomial", data=M)
hnp(model9)

model9  # print model-fit results for GLMM
Anova(model9,type="III")  # do type 3 testing of treatment
emmeans(model9, pairwise~treat, adjust="none")  # means and differences (all logiyts)

model9_2 <- glmer(Y/n ~ treat + (1|block), 
                 family="binomial", 
                 weights = n,
                 data=M)
hnp(model9_2)

model9_2  # print model-fit results for GLMM

## Fit GLMM with unit-level (block:treat interaction) random effect, plus block main effect
## See above comments about the three functions
## Harder to fit this model, and estimated block variance (.001646^2 = .00000271)
## is essentially 0. Should remove block effect term when using Laplace.

model12<- glmer(cbind(Y,n-Y)~treat+(1|block) + (1|block:treat),family="binomial",data=M)
model12
hnp(model12)

Anova(model12,type="III")
lsmeans(model12,pairwise~treat,adjust="none")

## Fit GLMM with unit-level (block:treat interaction) random effect, taking out block main effect
## This is based on the previous run, where block main effect was found to be virtually 0
## (.0000027). Because it is not quite exactly 0 (unlike with GLIMMIX), one might 
## keep it in. However, Laplace is suspect with variance estimate essentially 0.
## This one is a very appropriate choice for a RCBD and conditional binomial. 

model12i<- glmer(cbind(Y,n-Y)~treat + (1|block:treat),family="binomial",data=M)
model12i
Anova(model12i,type="III")
lsmeans(model12i,pairwise~treat,adjust="none")


## Now fit same GLMM model using quasi-likelood, which requires different
## package and function. The syntax is similar, but DIFFERENT, from glmer (tricky).
## CAUTION: with glmmPQL, one ALWAYS is getting an estimate of the phi overdispersion
## parameter. Thus, for the RCBD/binomial example, one can have a random block
## effect and the "residual" scale (phi) term. If one added a unit-level
## random effect (block:treat interaction), one would get meaningless results!!
## Thus, one cannot get a true conditional GLMM. Be careful when using glmmPQL.
##
## One cannot get pseudo-likelihood estimation with R without always getting an
## estimate of non-unity phi. Thus, one is always getting quasi-likelihood.
##
## The displayed "residual" is the square-root of phi. One gets chi-squared test
## statistics, not F or t statistics.
##
## As with glmer (above), the square-root of variances (=standard deviations) are displayed

model13<- glmmPQL(cbind(Y,n-Y)~treat, 
                  random= ~1|block, 
                  family="binomial",data=M)
model13
Anova(model13,type="III")
lsmeans(model13,pairwise~treat)

## Model fitting for GLMMs is always iterative (singly or doubly). The numerical results
## in R may not be exactly the same as the equivalent in SAS/GLIMMIX. This has to do
## with the specifics of the algorithm used. In general, the biggest difference will occur when 
## the parameter estimates approaches its boundary. For instance, with a variance, the
## usual lower boundary is 0. So, algorithms differ in how they proceed as the current
## estimate gets very close to 0. 
##
## Some results for pseudo(quasi)-likelihood (glmmPQL) with random block main effect may
## appear to be fairly different from the GLIMMIX output. The lsmeans and SEs are the
## same, but the chi-squared test statistic (if one requested this in GLIMMIX with the
## chisq option on the model statement) and the "residual" scale phi parameter estimate
## are fairly different for the two programs. This is because glmmPQL in R uses so-called 
## maximum quasi-likelihood estimation, but SAS/GLIMMIX uses so-called RESTRICTED pseudo- (or quasi-) 
## likelihood estimation when there is a phi parameter. If one added the option METHOD=MSPL to  
## the GLIMMIX statement (and the chisq option on the model statement), one gets identical results
## in R and GLIMMIX. With small to moderate sample sizes (as with the example), the default in
## GLIMMIX is preferred. 


## Next is a deliberately INCORRECT analysis, where random block AND block*treat interaction
## are specified. It is incorrect because with glmmPQL, one is also always getting an estimated
## phi "residual" or overdispersion parameter (one should NOT have phi and unit-level random term)
model14<- glmmPQL(cbind(Y,n-Y)~treat, random= (~1|block + treat|block), family="binomial",data=M)
model14
Anova(model14,type="III") 
lsmeans(model14,pairwise~treat)

library(performance)
model_performance( model12) 

, model12i, model13) 
