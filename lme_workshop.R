# Clay Ford (jcf2d)
# StatLab@UVa
# Linear Mixed-Effect Modeling with R
# Fall 2015

# pkgs <- c("lme4","ggplot2", "faraway","effects")
# install.packages(pkgs)

library(lme4)
library(ggplot2)
library(effects)
library(faraway) # for the data sets


# Fitting Models using lmer() ---------------------------------------------

data("ratdrink")

# The data consist of 5 weekly measurements of body weight for 27 rats. The
# first 10 rats are on a control treatment while 7 rats have thyroxine added to
# their drinking water. 10 rats have thiouracil added to their water.
# Source: faraway package (Faraway, 2006)

str(ratdrink)
# explore data
summary(ratdrink)
table(ratdrink$subject)
with(ratdrink, table(subject,weeks))
# summary stats
aggregate(wt ~ treat, data=ratdrink, mean)
aggregate(wt ~ treat + weeks, data=ratdrink, mean)
aggregate(wt ~ treat, data=ratdrink, sd)

# exploratory plots
# wt versus weeks with dots colored by treat
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat)) + geom_point()
# with groups
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat, group=subject)) + 
  geom_point() + geom_line()
# with faceting
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject)) + 
  geom_point() + geom_line() + facet_wrap(~ treat)

# linear mixed-effect model
# random intercept
lme1 <- lmer(wt ~ treat + weeks + (1 | subject), data=ratdrink)
lme1
summary(lme1) # notice: no p-values!
fixef(lme1)
ranef(lme1)
coef(lme1) # fitted model per group

# just the intercepts
coef(lme1)$subject[,1]

# notice the intercepts are the sum of the random effects and the fixed
# Intercept estimate:
fixef(lme1)[1] # fixed effect estimate of Intercept
ranef(lme1)$subject # predicted random effects for Intercept
fixef(lme1)[1] + ranef(lme1)$subject == coef(lme1)$subject[,1]

# estimates of variance parameters
VarCorr(lme1)


# fit random intercept and random slope
lme2 <- lmer(wt ~ treat + weeks + (weeks | subject), data=ratdrink)
summary(lme2)
fixef(lme2)
ranef(lme2)
coef(lme2) # fitted model per group


# Again notice the intercepts and slopes are the sum of the random effects and
# the fixed Intercept and slope estimates:
fixef(lme2)[c(1,4)] # fixed effect estimates
ranef(lme2)$subject # predicted random effects
fixef(lme2)[1] + ranef(lme2)$subject[,1] == coef(lme2)$subject[,1]
fixef(lme2)[4] + ranef(lme2)$subject[,2] == coef(lme2)$subject[,4]


# fit model with interaction and random slopes and intercept
lme3 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks | subject), 
             data=ratdrink)
# or lme3 <- lmer(wt ~ treat * weeks + (weeks | subject), data=ratdrink)
summary(lme3)
fixef(lme3)
ranef(lme3)

VarCorr(lme3)


# Effect Plots ------------------------------------------------------------

# plot fitted model for lme3

# a rather complicated way using ggplot2
fe <- fixef(lme3)
cols <- scales::hue_pal()(3) # get the colors that ggplot generated
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat)) + geom_point() +
  geom_abline(intercept=fe[1], slope=fe[4], color=cols[1]) +
  geom_abline(intercept=fe[1] + fe[2], slope=fe[4] + fe[5], color=cols[2]) +
  geom_abline(intercept=fe[1] + fe[3], slope=fe[4] + fe[6], color=cols[3])

# an easier way using the effects package
library(effects)
plot(allEffects(lme3))
# combined into one plot
plot(allEffects(lme3), multiline=TRUE)
# combined into one plot with confidence bands
plot(allEffects(lme3), multiline=TRUE, ci.style = "bands")
# see ?allEffects and ?plot.eff for more options and examples


# back to presentation


# Confidence Intervals ----------------------------------------------------

# profile method
confint(lme3)
# oldNames = FALSE changes the labeling
confint(lme3, oldNames = FALSE)

# bootstrap method with a progress bar (nsim = 500)
confint(lme3, method = "boot", .progress="txt")
# add a percent completion indicator
confint(lme3, method = "boot", nsim = 200,
        .progress="txt", PBargs=list(style=3))



# Diagnostics -------------------------------------------------------------

# Check assumptions of constant variance and normality

# check constant variance assumption
# residual vs. fitted values
plot(lme3)
# same as this:
plot(resid(lme3) ~ fitted(lme3))
abline(h=0)


# plots of residuals by weeks
plot(lme3, form = resid(.) ~ weeks)
# by weeks and treatment
plot(lme3, form = resid(.) ~ weeks | treat)

# residuals by subjects
plot(lme3, subject ~ resid(.))

# residuals by treat
plot(lme3, treat ~ resid(.))

# check normality of residuals
qqnorm(resid(lme3))

# check constant variance of random effects
plot(ranef(lme3))

# check normality of random effects
qqnorm(ranef(lme3)[[1]]$"(Intercept)") # intercept
qqnorm(ranef(lme3)[[1]]$weeks) # slope

# plot predicted random effects for each level of a grouping factor; allows you 
# to see if there are levels of a grouping factor with extremely large or small
# predicted random effects.
library(lattice)
dotplot(ranef(lme3))

# Model Comparison --------------------------------------------------------


# compare models 
# comparisons are sequential (1 vs. 2, 2 vs. 3)
anova(lme1, lme2, lme3)
# lme3 appears to be the "best" model

# notice models are re-fit with ML


# using simulation to test for random intercepts
library(RLRsim)
# Testing in models with only a single variance component require only the first
# argument m:
exactRLRT(m=lme1)


# For testing in models with multiple variance components, the fitted model m
# must contain only the random effect set to zero under the null hypothesis,
# while mA and m0 are the models under the alternative and the null,
# respectively



# m -- The fitted model under the alternative or, for testing in models with
# multiple variance components, the reduced model containing only the random
# effect to be tested (see Details), an lme, lmerMod or spm object 

# mA -- The full model under the alternative for testing in models with
# multiple variance components
# 
# m0 -- The model under the null for testing in models with multiple variance
# components

mA <- lme3
m0 <- update(mA, . ~ . - (weeks | subject) + (1 | subject) + (0 + weeks | subject))
m.slope  <- update(mA, . ~ . - (weeks | subject) + (0 + weeks | subject))
#test for subject specific slopes:
exactRLRT(m.slope, mA, m0)


ndvs <- simulate(lme3, nsim = 1)


# multilevel model
jspr <- read.csv("jspr.csv")
str(jspr)
# data from primary schools in inner London. Source: Mortimore, P., P. Sammons, 
# L. Stoll, D. Lewis, and R. Ecob (1988). School Matters. Wells, UK: Open Books.

# We have measures of students within classes within schools.
# Up to 4 classes with a school
# response variable is english, an english test score.

# social is class of the father: I=1; II=2; III nonmanual=3; III manual=4; IV=5;
# V=6; Long-term unemployed=7; Not currently employed=8; Father absent=9

# want to model English as a function of gender, social class and Raven's test
# score from first year.

# set school, class and social as factors:
jspr$school <- factor(jspr$school)
jspr$class <- factor(jspr$class)
jspr$social <- factor(jspr$social)

# explore the data

# how manu schools?
length(unique(jspr$school))

# break down of boy/girl
table(jspr$gender)

# break down of social
table(jspr$social)

# break down of gender and social
with(jspr, table(gender, social))

# how many students in each class in each school
with(jspr, table(class, school))

# mean english score per school
aggregate(english ~ school, data=jsp, mean)
# variability between schools

# mean english score per class per school
aggregate(english ~ school + class, data=jsp, mean)
mData <- aggregate(english ~ school + class, data=jsp, mean)
mData[order(mData$school),]
# variability between classes within schools

aggregate(english ~ gender, data=jspr, mean)
aggregate(english ~ gender + social, data=jspr, mean)

# some visual exploration
ggplot(jspr, aes(x=gender, y=english)) + geom_boxplot()
ggplot(jspr, aes(x=gender, y=english)) + geom_boxplot() + facet_wrap(~school)

ggplot(jspr, aes(x=social, y=english)) + geom_boxplot()
ggplot(jspr, aes(x=social, y=english)) + geom_boxplot() + facet_wrap(~gender)

ggplot(jspr, aes(x=raven, y=english)) + geom_point()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter())
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth()
ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~gender)

ggplot(jspr, aes(x=raven, y=english)) + geom_point(position=position_jitter()) +
  geom_smooth() + facet_wrap(~social)

# with grouping
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + facet_wrap(~school)
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~school)
# random slope for raven?

# interaction plot
with(jspr, interaction.plot(x.factor = social,trace.factor = gender,response = english))

lmeEng1 <- lmer(english ~ raven + gender*social + (1 | school/class), data=jspr)
summary(lmeEng1)
fixef(lmeEng1)
ranef(lmeEng1)
coef(lmeEng1)
anova(lmeEng1)

lmeEng2 <- lmer(english ~ raven + gender*social + (raven | school/class), data=jspr)
anova(lmeEng1, lmeEng2, refit=FALSE)
# random slope for raven doesn't appear to be necessary;
# but recall, the test is conservative.

# observed value of LRT test statsitcs
oLRT <- -2*(logLik(lmeEng1) - logLik(lmeEng2))

simulate(lmeEng2, re.form=~(raven | school/class))

predict(lmeEng1, re.form=~raven + gender * social + (1 | school/class))
# simulate likelihood ratio test statistics
eval <- logical(100)
for(i in 1:100){
  yN <- simulate(lmeEng1)
  ll1 <- as.numeric(logLik(refit(lmeEng1, newresp = yN)))
  ll2 <- as.numeric(logLik(refit(lmeEng2, newresp = yN)))
  eval[i] <- (-2*(ll1 - ll2)) >= oLRT
}

