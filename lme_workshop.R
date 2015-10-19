# Clay Ford (jcf2d)
# StatLab@UVa
# Linear Mixed-Effect Modeling with R
# Fall 2015

# pkgs <- c("lme4","ggplot2")
# install.packages(pkgs)

library(lme4)
library(ggplot2)

# Fitting Models using lmer() ---------------------------------------------

# EXAMPLE 1

URL <- "http://people.virginia.edu/~jcf2d/workshops/LMEinR/ratdrink.csv"
ratdrink <- read.csv(URL)

# if URL not available and ratdrink.csv in working directory:
# ratdrink <- read.csv("ratdrink.csv")

# The data consist of 5 weekly measurements of body weight for 27 rats. The 
# first 10 rats are on a control treatment while 7 rats have thyroxine added to 
# their drinking water. 10 rats have thiouracil added to their water. We're 
# interested in how the treatments affect the weight of the rats. Source:
# faraway package (Faraway, 2006)

str(ratdrink)
# explore data
summary(ratdrink)# unbalanced data
with(ratdrink, table(subject, treat))
with(ratdrink, table(subject, weeks))

# summary stats
aggregate(wt ~ weeks + treat, data=ratdrink, mean)

# exploratory plots
# scatterplot
ggplot(ratdrink, aes(x=weeks, y=wt)) + geom_point()
# wt versus weeks with dots colored by treat and with grouping
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat, group=subject)) + 
  geom_point() + geom_line()

# with faceting
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject)) + 
  geom_point() + geom_line() + facet_wrap(~ treat)
# boxplots by week
ggplot(ratdrink, aes(x=treat,y=wt)) + geom_boxplot() + facet_wrap(~weeks)


# interaction plot
with(ratdrink, 
     interaction.plot(x.factor = weeks, 
                      trace.factor = treat, 
                      response = wt))

# linear mixed-effect model #1
# no interaction between treat and week
# random intercept
lmm1 <- lmer(wt ~ treat + weeks + (1 | subject), data=ratdrink)
lmm1
summary(lmm1) # notice: no p-values!
summary(lmm1, corr=FALSE) # supress "Correlation of Fixed Effects"

# What are Correlation of Fixed Effects?
# see explanation from lme4 author:
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2009q1/001941.html

fixef(lmm1) 

# Control is baseline. "treatthiouracil -13.9600" means rats on thiouracil are
# about 13 grams lighter than rats on control.

ranef(lmm1) # aka Best Linear Unbiased Predictions (BLUPs)
coef(lmm1) # fitted model per group

# Notice the (Intercept) column is the sum of the fixed intercept and random
# intercept:
fixef(lmm1)[1] # fixed effect estimate of Intercept
ranef(lmm1)$subject[1,] # predicted random effect for subject 1 Intercept

# add them to get the intercept column in coef(lmm1)
fixef(lmm1)[1] + ranef(lmm1)$subject[1,]
coef(lmm1)$subject[1,] # coefficients for subject 1

# estimates of variance parameters
VarCorr(lmm1)


# fit random intercept and random slope for weeks;
lmm2 <- lmer(wt ~ treat + weeks + (weeks | subject), data=ratdrink)
summary(lmm2, corr=F)
fixef(lmm2)
ranef(lmm2)
coef(lmm2) # fitted model per group


# Again notice the intercepts and slopes are the sum of the random effects and
# the fixed Intercept and slope estimates:
fixef(lmm2)[c(1,4)] # fixed effect estimates
ranef(lmm2)$subject[1,] # predicted random effects for subject 1

# add them to get the intercept and slope columns in coef(lmm2)
fixef(lmm2)[c(1,4)] + ranef(lmm2)$subject[1,]
coef(lmm2)$subject[1,] # coefficients for subject 1

# estimates of variance parameters
VarCorr(lmm2)

# Corr estimated to be -0.326. Suggests the slope and intercept random effects
# may not be independent. Perhaps a higher intercept means a lower trajectory?

# fit model with uncorrelated random intercept and slope:
lmm3 <- lmer(wt ~ treat + weeks + (weeks || subject), data=ratdrink)
summary(lmm3, corr=F)

# estimates of variance parameters
VarCorr(lmm3)


# fit model with interaction;
# implies different treatments lead to different slopes
# include random slopes and intercept
lmm4 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks | subject), 
             data=ratdrink)
# or lmm4 <- lmer(wt ~ treat * weeks + (weeks | subject), data=ratdrink)
summary(lmm4, corr=F)
fixef(lmm4)
ranef(lmm4)
coef(lmm4)

VarCorr(lmm4)

# Interpreting interaction:
# The intercept and weeks coefficients are the fitted line for the control group.

# Intercept + treatthiouracil is the intercept for the thiouracil group.
# weeks + treatthiouracil:weeks is the slope for the thiouracil group.

# treatthiouracil:weeks = -9.37 means the trajectory for thiouracil is lower
# than the control group.

# fit model with interaction and uncorrelated random slopes and intercept
lmm5 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks || subject), 
             data=ratdrink)
summary(lmm5, corr=FALSE)

# back to presentation

# Assessing Significance --------------------------------------------------

# confidence intervals
# Let's look at lmm1
formula(lmm1)

# profile method for all parameters
confint(lmm1)
# oldNames = FALSE changes the labeling
confint(lmm1, oldNames = FALSE)

# specify weeks parameters
confint(lmm1, parm = "weeks")
# only fixed effects
confint(lmm1, parm = "beta_")
# only variance parameters
confint(lmm1, parm = "theta_", oldNames = FALSE)

# bootstrap method with a progress bar (nsim = 500)
confint(lmm1, method = "boot")

# bootstrap method with a progress bar
confint(lmm1, method = "boot", nsim = 200,
        .progress="txt",oldNames = FALSE)
# add a percent completion indicator
confint(lmm1, method = "boot", nsim = 200,
        .progress="txt", PBargs=list(style=3),
        oldNames = FALSE)

# generate approximate p-values
# install.packages("lmerTest")
library(lmerTest)
# Notice it implements a different version of lmer!
# refit lmm1; call it lmm1a
lmm1a <- lmer(formula(lmm1), data=ratdrink)
summary(lmm1a)

# Let's unload the lmerTest package and unmask the original lmer function.
detach("package:lmerTest", unload=TRUE)
rm(lmm1a)


# Diagnostics -------------------------------------------------------------

# Two basic assumptions need to be checked:
# 1. within-group errors are normal, centered at 0, have constant variance
# 2. random effects are normal, centered at 0, have constant variance

# check constant variance assumption
# residual vs. fitted values
plot(lmm5)

# plots of residuals by weeks
plot(lmm5, form = resid(.) ~ weeks)
# by weeks and treatment
plot(lmm5, form = resid(.) ~ weeks | treat)
# not doing very well with thyroxine in weeks 0 and 1

# residuals by subjects
plot(lmm5, subject ~ resid(.)) # oops...need to declare subject a factor
plot(lmm5, factor(subject) ~ resid(.))
# hopefully we're not systematically under- or over-predicting for subjects

# check normality of residuals
qqnorm(resid(lmm5))

# check constant variance of random effects
plot(ranef(lmm5))

# check normality of random effects
# library(lattice)
lattice::qqmath(ranef(lmm5))
# These hopefully follow a positive 45 degree line

# Catepillar plot

# plot predicted random effects for each level of a grouping factor; allows you 
# to see if there are levels of a grouping factor with extremely large or small
# predicted random effects.
lattice::dotplot(ranef(lmm5))

# check model fit
plot(lmm5, wt ~ fitted(.) | subject, abline = c(0,1))



# Model Predictions -------------------------------------------------------

# predicted values, including random effects
# same as fitted(lmm5)
predict(lmm5)

# compare to original values for subjects 1 & 2
cbind(ratdrink$wt[1:10], predict(lmm5)[1:10])

# predicted values, NOT including random effects;
# also known as marginal predictions or population fitted values
predict(lmm5, re.form=NA)

# compare to original values for subjects 1 & 2
cbind(ratdrink$wt[1:10], predict(lmm5, re.form=NA)[1:10])
# NOTE: same for both subjects (both have same treatment: control)

# make predictions for new data
# new data needs to be in a data frame with same names as original data

# predict weight at 2.5 weeks for all treatments:
nd <- data.frame(treat=levels(ratdrink$treat), weeks=2.5)
nd
predict(lmm5, newdata=nd, re.form=NA)

# predict weight at weeks 3, 4, and 5 for thiouracil
nd <- data.frame(treat="thiouracil", weeks=c(3,4,5))
nd
predict(lmm5, newdata=nd, re.form=NA)

# back to presentation

# Model Comparison --------------------------------------------------------

# let's compare lmm2 with lmm4:
formula(lmm2) # no interaction
formula(lmm4) # with interaction

anova(lmm2, lmm4)
# interaction appears significant

# can also use extractAIC to compare models (lower is better)
extractAIC(lmm2)
extractAIC(lmm4)

# AIC also works; the calculations are slightly different
AIC(lmm2, lmm4)

# let's compare lmm5 with lmm4:
formula(lmm5) # with interaction; no correlation between random effects
formula(lmm4) # with interaction; correlation between random effects

VarCorr(lmm4) # with correlated random effects
VarCorr(lmm5) # without correlated random effects

# let's test if the correlation (or covariance) between the two random effects
# is 0
anova(lmm4, lmm5)
# notice models are re-fit with ML
# the p-value is approximate

# suppress since fixed effects are the same in each model:
anova(lmm4, lmm5, refit = FALSE)

# Let's compute a corrected p-value. 
# Save the output so we can access Chi-square test statistic
str(anova(lmm4, lmm5, refit = FALSE))
aout <- na.omit(anova(lmm4, lmm5, refit = FALSE)) # drop NAs
str(aout)
aout$Chisq

# In the test we see that it's on 1 degree of freedom, but recall the null
# distribution is not on 1 degree of freedom but rather a mixture of
# distributions.
0.5*pchisq(aout$Chisq, 1, lower.tail = FALSE) + 0.5*pchisq(aout$Chisq, 0, lower.tail = FALSE)
# or this:
pchisq(aout$Chisq, 1, lower.tail = FALSE)/2

# we can write a function to automate this:
pvalMix <- function(stat,df){
  0.5*pchisq(stat, df, lower.tail = FALSE) + 
    0.5*pchisq(stat, df-1, lower.tail = FALSE)
}
pvalMix(stat=aout$Chisq, df=aout$`Chi Df`)

# Result: fail to reject; appears safe to assume the random effects are independent



# time permitting example 2 (with nested random effects) ------------------

# multilevel model

URL <- "http://people.virginia.edu/~jcf2d/workshops/LMEinR/jspr.csv"
jspr <- read.csv(URL)

# if URL not available and jspr.csv in working directory:
# jspr <- read.csv("jspr.csv")

str(jspr)
# Junior School Project
# data from primary schools in inner London. Source: Mortimore, P., P. Sammons, 
# L. Stoll, D. Lewis, and R. Ecob (1988). School Matters. Wells, UK: Open Books.

# We have measures of students within classes, which are within schools.
# Up to 4 classes within a school;
# response variable is english: an english test score.
# raven = score on Raven's test, designed to measure reasoning ability

# social is class of the father (ordinal scale): 
# Nonmanual: 1, 2, 3
# Manual: 4, 5, 6
# Long-term unemployed=7; Not currently employed=8; Father absent=9

# want to model English score as a function of gender, social class and Raven's
# test score. Data is grouped by school, and class within school

# set school, class and social as factors:
jspr$school <- factor(jspr$school)
jspr$class <- factor(jspr$class)
jspr$social <- factor(jspr$social)

# explore the data

# how many schools?
length(unique(jspr$school))

# break down of boy/girl
table(jspr$gender)

# break down of social
table(jspr$social)
barplot(table(jspr$social)) # most kids from working class homes

# break down of gender and social
with(jspr, table(gender, social))

# how many students in each class in each school
with(jspr, table(class, school))

# english scores by gender and social
aggregate(english ~ gender, data=jspr, mean)
aggregate(english ~ social, data=jspr, mean)
aggregate(english ~ gender + social, data=jspr, mean)


# some visual exploration


# visualize variability of mean english score between schools
# also notice variability within schools
ggplot(jspr, aes(x=school,y=english)) + geom_point(alpha=1/3) + 
  stat_summary(fun.y="mean", geom="point", color="red", size=4) +
  geom_hline(yintercept=mean(jspr$english))

# visualize variability of mean english score between classes within schools
ggplot(jspr, aes(x=class,y=english)) + geom_point(alpha=1/3) + 
  stat_summary(fun.y="mean", geom="point", color="red", size=4) +
  facet_wrap(~ school)


ggplot(jspr, aes(x=raven, y=english)) + geom_point()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter())
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~gender)

ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~social) +
  labs(title="English score vs Raven assessment by Social Class")


# with grouping
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + facet_wrap(~school)
ggplot(jspr, aes(x=raven, y=english, color=gender)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~school)
ggplot(jspr, aes(x=raven, y=english, color=gender)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~social)

# random slope for raven?
# gender and raven interact?
# social and raven interact?

# fit models

# random intercept with raven + gender + social
lmeEng1 <- lmer(english ~ raven + gender + social + (1 | school/class), data=jspr)
summary(lmeEng1, corr=FALSE)
VarCorr(lmeEng1)
fixef(lmeEng1)
ranef(lmeEng1)

# models for schools, and classes within schools
coef(lmeEng1)

# add interaction for gender and raven
lmeEng2 <- lmer(english ~ raven*gender + social + (1 | school/class), data=jspr)
summary(lmeEng2, corr=FALSE)
VarCorr(lmeEng2)

# compare models
anova(lmeEng1, lmeEng2)
# appears interaction is not warranted

# does social help explain variability in english score
lmeEng3 <- lmer(english ~ raven + gender + (1 | school/class), data=jspr)
summary(lmeEng3, corr=FALSE)

# compare models
anova(lmeEng1, lmeEng3)
# it seems we should keep social

# random slope for raven?
lmeEng4 <- lmer(english ~ raven + gender + social + (raven | school/class), data=jspr)
summary(lmeEng4, corr=FALSE)
VarCorr(lmeEng4)
# perfect correlation?

# try fitting random slope for raven just at school level
lmeEng5 <- lmer(english ~ raven + gender + social + (raven | school) + (1 | school:class), 
                data=jspr)
summary(lmeEng5, corr=FALSE)
VarCorr(lmeEng5)


anova(lmeEng4, lmeEng5, refit=FALSE)
# with correction
aout <- na.omit(anova(lmeEng4, lmeEng5, refit=FALSE))
pvalMix(aout$Chisq, df=aout$`Chi Df`)

# do we even need a random intercept for raven?
aout <- na.omit(anova(lmeEng1,lmeEng4, refit=FALSE))
pvalMix(aout$Chisq, df=aout$`Chi Df`)
# probably OK to do without


# check model fit
plot(lmeEng1, english ~ fitted(.) | school, abline = c(0,1))
# not great

# some diagnostics
plot(lmeEng1)
plot(lmeEng1, form = school ~ resid(.))

# normality check
qqnorm(resid(lmeEng1)) # residuals
plot(ranef(lmeEng1)) # random effects

# are there levels of a grouping factor with extremely large or small predicted
# random effects?
lattice::dotplot(ranef(lmeEng1))

