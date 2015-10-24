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

# summary stats
aggregate(wt ~ weeks + treat, data=ratdrink, mean)

# interaction plot
with(ratdrink, 
     interaction.plot(x.factor = weeks, 
                      trace.factor = treat, 
                      response = wt))

# exploratory plots
# scatterplot
ggplot(ratdrink, aes(x=weeks, y=wt)) + geom_point()
# wt versus weeks with dots colored by treat and with grouping
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat, group=subject)) + 
  geom_point() + geom_line()

# boxplots by week
ggplot(ratdrink, aes(x=treat,y=wt)) + geom_boxplot() + facet_wrap(~weeks)



# model #1
# no interaction between treat and week
# random intercept
lmm1 <- lmer(wt ~ treat + weeks + (1 | subject), data=ratdrink)
lmm1
summary(lmm1, corr=FALSE) # supress "Correlation of Fixed Effects"

# What are Correlation of Fixed Effects?
# see explanation from lme4 author:
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2009q1/001941.html

# Control is baseline. "treatthiouracil -13.9600" means rats on thiouracil are
# about 14 grams lighter than rats on control.

fixef(lmm1) 

coef(lmm1) # fitted model per group
# notice the intercepts vary (random intercept model)

ranef(lmm1) # aka Best Linear Unbiased Predictions (BLUPs)

# Notice the (Intercept) column in the coef(lmm1) output is the sum of the fixed
# intercept and random intercept:

coef(lmm1)$subject[1,] # coefficients for subject 1
fixef(lmm1)[1] # fixed effect estimate of Intercept
ranef(lmm1)$subject[1,] # predicted random effect for subject 1 Intercept

# add them to get the intercept column in coef(lmm1)
fixef(lmm1)[1] + ranef(lmm1)$subject[1,]
coef(lmm1)$subject[1,] # coefficients for subject 1

# estimates of variance parameters
VarCorr(lmm1)

# model #2
# fit random intercept and random slope for weeks;
lmm2 <- lmer(wt ~ treat + weeks + (weeks | subject), data=ratdrink)
summary(lmm2, corr=F)

# Corr estimated to be -0.326. Suggests the slope and intercept random effects
# may not be independent. Perhaps a higher intercept means a lower trajectory?

fixef(lmm2)
ranef(lmm2)
coef(lmm2) # fitted model per group
# Notice both the intercept and weeks coefficients vary

# Again notice the intercepts and slopes are the sum of the random effects and
# the fixed Intercept and slope estimates:
fixef(lmm2)[c(1,4)] # fixed effect estimates
ranef(lmm2)$subject[1,] # predicted random effects for subject 1

# add them to get the intercept and slope columns in coef(lmm2)
fixef(lmm2)[c(1,4)] + ranef(lmm2)$subject[1,]
coef(lmm2)$subject[1,] # coefficients for subject 1

# estimates of variance parameters
VarCorr(lmm2)

# model #3
# fit model with uncorrelated random intercept and slope:
lmm3 <- lmer(wt ~ treat + weeks + (weeks || subject), data=ratdrink)
summary(lmm3, corr=F)

# estimates of variance parameters
VarCorr(lmm3)

# model #4
# fit model with interaction;
# implies different treatments lead to different slopes
# include random slopes and intercept
lmm4 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks | subject), 
             data=ratdrink)
# or lmm4 <- lmer(wt ~ treat * weeks + (weeks | subject), data=ratdrink)
summary(lmm4, corr=F)

# Interpreting interaction:
# The intercept and weeks coefficients are the fitted line for the control group.

# Intercept + treatthiouracil is the intercept for the thiouracil group.
# weeks + treatthiouracil:weeks is the slope for the thiouracil group.

# treatthiouracil:weeks = -9.37 means the trajectory for thiouracil is lower
# than the control group.

# model #5
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

# can specify specific parameters
confint(lmm1, parm = "weeks")
# only fixed effects: use "beta_"
confint(lmm1, parm = "beta_")
# only variance parameters: "theta_"
confint(lmm1, parm = "theta_", oldNames = FALSE)

# bootstrap method 
confint(lmm1, method = "boot")

# bootstrap method with a progress bar and fewer simulations:
confint(lmm1, method = "boot", nsim = 200,
        .progress="txt",oldNames = FALSE)

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

# Back to presentation.

# Diagnostics -------------------------------------------------------------

# Two basic assumptions need to be checked:
# 1. within-group errors are normal, centered at 0, have constant variance
# 2. random effects are normal, centered at 0, have constant variance

# Let's look at model #5
formula(lmm5) # with interaction, uncorrelated random effects

# check constant variance assumption
# residual vs. fitted values
plot(lmm5)

# plots of residuals by weeks
plot(lmm5, form = resid(.) ~ weeks)
# by weeks and treatment
plot(lmm5, form = resid(.) ~ weeks | treat)
# not doing very well with thyroxine in weeks 0 and 1

# residuals by subjects (boxplots)
plot(lmm5, subject ~ resid(.)) # oops...need to declare subject a factor
plot(lmm5, factor(subject) ~ resid(.))
# hopefully we're not systematically under- or over-predicting for subjects;
# this type of plot not feasible with huge number of subjects

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
# again this plot not feasible with large numbers of subjects

# Back to presentation

# Model Predictions -------------------------------------------------------

# predicted values, including random effects
# same as fitted(lmm5)
predict(lmm5)

# compare to original values for subjects 1 & 2
cbind(observed = ratdrink$wt[1:10], predicted = predict(lmm5)[1:10])

# predicted values, NOT including random effects;
# also known as marginal predictions or population fitted values
predict(lmm5, re.form=NA)

# compare to original values for subjects 1 & 2
cbind(observed = ratdrink$wt[1:10], predicted = predict(lmm5, re.form=NA)[1:10])
# NOTE: same for both subjects (both have same treatment: control)

# make predictions for new data
# new data needs to be in a data frame with same names as original data

# predict marginal weight at 2.5 weeks for all treatments:
nd <- data.frame(treat=levels(ratdrink$treat), weeks=2.5)
nd
predict(lmm5, newdata=nd, re.form=NA)

# predict marginal weight at weeks 3, 4, and 5 for thiouracil
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

# The p-value is conservatice.
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

# sometimes called a multilevel model

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

# set school, class and social as factors (ie, categorical variables):
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
  stat_summary(fun.y="mean", geom="point", color="red", size=3) +
  facet_wrap(~ school)

# scatterplots
ggplot(jspr, aes(x=raven, y=english)) + geom_point()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter())
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~gender)

ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~social) +
  labs(title="English score vs Raven assessment by Social Class")


# scatterplots with grouping by school
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + facet_wrap(~school)
# scatterplots with grouping by school
ggplot(jspr, aes(x=raven, y=english, color=gender)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~school)
# scatterplots with grouping by social
ggplot(jspr, aes(x=raven, y=english, color=gender)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~social)

# random slope for raven?
# gender and raven interact?
# social and raven interact?
# treat social as categorical or a scale?

# fit models

# random intercept with raven + gender + social
lmeEng1 <- lmer(english ~ raven + gender + social + (1 | school/class), data=jspr)
summary(lmeEng1, corr=FALSE)
# models for schools, and classes within schools
coef(lmeEng1)

# Interpretation of fixed effect coefficients: 

# raven: every 1 point increase in Raven test score leads to about a 1.6
# increase in english score.

# gendergirl: girls score about 6 points higher on english test.

# social2-9: difference from social1 ("highest" social class)

# Intercept: average English score for boys in social class 1 and a Raven test
# score of 0. (nonsense!)

# To make intercept interpretable, let's center the Raven score and use it in
# the model instead of raven:
jspr$craven <- jspr$raven - mean(jspr$raven)

# new lmeEng1
lmeEng1 <- lmer(english ~ craven + gender + social + (1 | school/class), data=jspr)
summary(lmeEng1, corr=FALSE)

# Now Intercept is the average English score for boys in social class 1 with the
# mean Raven score, which is about 25.

# add interaction for gender and raven
lmeEng2 <- lmer(english ~ craven*gender + social + (1 | school/class), data=jspr)
summary(lmeEng2, corr=FALSE)

# The interaction doesn't look significant. Assuming it is, the interpretation
# of the craven and gender fixed effect parameters is as follows:

# craven: every 1 point increase in Raven test score leads to about a 1.5
# increase in english score FOR BOYS.

# craven:gendergirl: every 1 point increase in Raven test score leads to about a
# 1.5 + 0.2 = 1.7 increase in english score FOR GIRLS.


# compare models to see if we should keep interaction:
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

# let's look at data again
# raven vs english, by school, color coded by class, with trend lines
ggplot(jspr, aes(x=raven, y=english, color=class)) + geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~school)
# trying to account for raven slope variabilty between classes within schools
# when many schools only have one class is problematic.

# probably better to just accont for raven variation between schools:
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + 
  geom_smooth(method="lm", se=F) +
  facet_wrap(~school)

# try fitting random slope for raven just at school level
lmeEng5 <- lmer(english ~ raven + gender + social + 
                  (raven | school) + (1 | school:class), 
                data=jspr)
summary(lmeEng5, corr=FALSE)
VarCorr(lmeEng5)

# Compare models 4 and 5
anova(lmeEng4, lmeEng5, refit=FALSE)

# Recall the p-value is conservative

# with correction
aout <- na.omit(anova(lmeEng4, lmeEng5, refit=FALSE))
pvalMix(aout$Chisq, df=aout$`Chi Df`)
# model 4 appears preferable to model 5

# do we even need a random slope for raven?
# compare model 4 to model 1
anova(lmeEng1,lmeEng4, refit=FALSE)
# Notice AIC is identical
# perform correction
aout <- na.omit(anova(lmeEng1,lmeEng4, refit=FALSE))
pvalMix(aout$Chisq, df=aout$`Chi Df`)
# probably OK to do without random slope for raven

# perhaps try model with social as a scale instead of a categorical variable.
jspr$socials <- unclass(jspr$social)

lmeEng6 <- lmer(english ~ craven + gender + socials + (1 | school/class), data=jspr)
summary(lmeEng6, corr=FALSE)
# as social score increases, english scores tend to go down. Seems significant.

# Is the new model better? Can't compare with hypothesis test.
AIC(lmeEng1, lmeEng6)
# Looks like we still prefer our first model if we go by AIC.
# But perhaps you prefer model 6 for the easier interpretation...

# check model fit
plot(lmeEng1, english ~ fitted(.) | school, abline = c(0,1))
# not great. However it's worth noting that's probably not what we would use our
# model for. Probably just want to understand the relationship (if any) between 
# our predictors and response. It seems social class and Raven score are
# positively related with higher English scores.

# some diagnostics
plot(lmeEng1)
plot(lmeEng1, form = school ~ resid(.))

# normality check
qqnorm(resid(lmeEng1)) # residuals
plot(ranef(lmeEng1)) # random effects: TWO plots

# are there levels of a grouping factor with extremely large or small predicted
# random effects?
lattice::dotplot(ranef(lmeEng1)) # TWO plots

# look at school 29 and school 31
subset(jspr, school==29)
subset(jspr, school==31)
aggregate(english ~ school, data=jspr, mean, subset = school %in% c(29,31))
mean(jspr$english)

