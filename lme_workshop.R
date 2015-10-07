# Clay Ford (jcf2d)
# StatLab@UVa
# Linear Mixed-Effect Modeling with R
# Fall 2015

# pkgs <- c("lme4","ggplot2", "effects", "car")
# install.packages(pkgs)

library(lme4)
library(ggplot2)
library(effects)
library(car)

# Fitting Models using lmer() ---------------------------------------------

# EXAMPLE 1

ratdrink <- read.csv("http://people.virginia.edu/~jcf2d/workshops/LMEinR/ratdrink.csv")

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
aggregate(wt ~ treat, data=ratdrink, mean)
aggregate(wt ~ weeks + treat, data=ratdrink, mean)
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

# interaction plot
with(ratdrink, 
     interaction.plot(x.factor = weeks, 
                      trace.factor = treat, 
                      response = wt))

# linear mixed-effect model, no interaction between treat and week
# random intercept
lmm1 <- lmer(wt ~ treat + weeks + (1 | subject), data=ratdrink)
lmm1
summary(lmm1) # notice: no p-values!
fixef(lmm1)
ranef(lmm1)
coef(lmm1) # fitted model per group

# Notice the (Intercept) column is the sum of the fixed intercept and random
# intercept:

fixef(lmm1)[1] # fixed effect estimate of Intercept
ranef(lmm1)$subject # predicted random effects for Intercept

# add them to get the intercept column in coef(lmm1)
fixef(lmm1)[1] + ranef(lmm1)$subject
# check they're the same
cbind(coef(lmm1)$subject[,1], fixef(lmm1)[1] + ranef(lmm1)$subject)
fixef(lmm1)[1] + ranef(lmm1)$subject == coef(lmm1)$subject[,1]

# estimates of variance parameters
VarCorr(lmm1)


# fit random intercept and random slope for weeks;
lmm2 <- lmer(wt ~ treat + weeks + (weeks | subject), data=ratdrink)
summary(lmm2)
fixef(lmm2)
ranef(lmm2)
coef(lmm2) # fitted model per group


# Again notice the intercepts and slopes are the sum of the random effects and
# the fixed Intercept and slope estimates:
fixef(lmm2)[c(1,4)] # fixed effect estimates
ranef(lmm2)$subject # predicted random effects
fixef(lmm2)[1] + ranef(lmm2)$subject[,1] == coef(lmm2)$subject[,1]
fixef(lmm2)[4] + ranef(lmm2)$subject[,2] == coef(lmm2)$subject[,4]

# estimates of variance parameters
VarCorr(lmm2)

# fit model with uncorrelated random intercept and slope:
lmm3 <- lmer(wt ~ treat + weeks + (weeks || subject), data=ratdrink)
summary(lmm3)
fixef(lmm3)
ranef(lmm3)
coef(lmm3) # fitted model per group

# estimates of variance parameters
VarCorr(lmm3)


# fit model with interaction and random slopes and intercept
lmm4 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks | subject), 
             data=ratdrink)
# or lmm4 <- lmer(wt ~ treat * weeks + (weeks | subject), data=ratdrink)
summary(lmm4)
fixef(lmm4)
ranef(lmm4)
coef(lmm4)

VarCorr(lmm4)

# fit model with interaction and uncorrelated random slopes and intercept
lmm5 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks || subject), 
             data=ratdrink)
summary(lmm5, correlation=FALSE)

# correlation=FALSE suppresses the Correlation of Fixed Effects output.

# Effect Plots ------------------------------------------------------------

# plot fitted model for lmm4: correlated random intercept and slope with
# interaction betweem weeks and treat:

# a rather complicated way using ggplot2
fe <- fixef(lmm4)
cols <- scales::hue_pal()(3) # get the colors that ggplot generates
ggplot(ratdrink, aes(x=weeks, y=wt, color=treat)) + geom_point() +
  geom_abline(intercept=fe[1], slope=fe[4], color=cols[1]) +
  geom_abline(intercept=fe[1] + fe[2], slope=fe[4] + fe[5], color=cols[2]) +
  geom_abline(intercept=fe[1] + fe[3], slope=fe[4] + fe[6], color=cols[3])

# an easier way using the effects package
# library(effects)
plot(allEffects(lmm4))
# combined into one plot
plot(allEffects(lmm4), multiline=TRUE)
# combined into one plot with confidence bands
plot(allEffects(lmm4), multiline=TRUE, ci.style = "bands")
# see ?allEffects and ?plot.eff for more options and examples


# back to presentation



# Assessing Significance --------------------------------------------------

# confidence intervals
# Let's look at lmm5
formula(lmm5)

# profile method
confint(lmm5)
# oldNames = FALSE changes the labeling
confint(lmm5, oldNames = FALSE)

# bootstrap method with a progress bar (nsim = 500)
confint(lmm5, method = "boot", .progress="txt", , oldNames = FALSE)
# add a percent completion indicator
confint(lmm5, method = "boot", nsim = 200,
        .progress="txt", PBargs=list(style=3),
        oldNames = FALSE)

# generate approximate p-values
# install.packages("lmerTest")
library(lmerTest)
# Notice it implements a different version of lmer!
# refit lmm4; call it lmm4a
lmm5a <- lmer(formula(lmm5), data=ratdrink)
summary(lmm5a)

# Let's unload the lmerTest package and unmask the original lmer function.
detach("package:lmerTest", unload=TRUE)

# assessing fixed-effect factors. In this case "treat". It has three levels.

# sequential test (Type I), no p-values
anova(lmm5)

# test each term after all others (Type II), approx p-values
# library(car)
Anova(lmm5)

# treat not significant, but interaction is

# Diagnostics -------------------------------------------------------------

# Two basic assumptions need to be checked:
# 1. within-group errors are normal, centered at 0, have constant variance
# 2. random effects are normal, centered at 0, have constant variance

# check constant variance assumption
# residual vs. fitted values
plot(lmm5)
# same as this:
plot(resid(lmm5) ~ fitted(lmm5))
abline(h=0)


# plots of residuals by weeks
plot(lmm5, form = resid(.) ~ weeks)
# by weeks and treatment
plot(lmm5, form = resid(.) ~ weeks | treat)
# not doing very well with thyroxine in weeks 0 and 1

# residuals by subjects
plot(lmm5, subject ~ resid(.))
plot(lmm5, factor(subject) ~ resid(.))
# hopefully we're not systematically under- or over-predicting for subjects

# residuals by treat
plot(lmm5, treat ~ resid(.))

# check normality of residuals
qqnorm(resid(lmm5))

# check constant variance of random effects
plot(ranef(lmm5))

# check normality of random effects
library(lattice)
qqmath(ranef(lmm5))

# another way without lattice
qqnorm(ranef(lmm5)[[1]]$"(Intercept)") # intercept
qqnorm(ranef(lmm5)[[1]]$weeks) # slope

# plot predicted random effects for each level of a grouping factor; allows you 
# to see if there are levels of a grouping factor with extremely large or small
# predicted random effects.
dotplot(ranef(lmm5))

# check model fit
plot(lmm5, wt ~ fitted(.) | subject, abline = c(0,1))


# back to presentation

# Model Comparison --------------------------------------------------------

# let's compare lmm5 with lmm3:
formula(lmm3) # no interaction; no correlation between random effects
formula(lmm5) # with interaction; no correlation between random effects

anova(lmm3, lmm5)
# interaction appears significant

# can also use extractAIC to compare models (lower is better)
extractAIC(lmm3)
extractAIC(lmm5)

# AIC also works; the calculations are slightly different
AIC(lmm3, lmm5)

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


# do we need a random effect for weeks (ie, a random slope)?
anova(lmm1, lmm2)
# notice models are re-fit with ML

# suppress since fixed effects are the same in each model:
anova(lmm1, lmm2, refit = FALSE)

# p-value is tiny, but let's compute a corrected p-value anyway. 
# let's save the output so we can access Chi-square test statistic
aout <- na.omit(anova(lmm1, lmm2, refit = FALSE)) # drop NAs
aout$Chisq
pvalMix(stat=aout$Chisq, df=aout$`Chi Df`)


# compare model with correlated random effects to model without correlated
# random effects:
anova(lmm2, lmm3)
# let's compute corrected p-value
pvalMix(0.9721, 1)
# still not significant; prefer lmm3, simpler model with no correlation between 
# random intercept and slope.



# EXAMPLE 2

# multilevel model
jspr <- read.csv("http://people.virginia.edu/~jcf2d/workshops/LMEinR/jspr.csv")
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
aggregate(english ~ school, data=jspr, mean)
# variability between schools

# mean english score per class per school
mData <- aggregate(english ~ school + class, data=jspr, mean)
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
  geom_smooth() + facet_wrap(~social) +
  labs(title="English score vs Raven assessment by Social Class")


# with grouping
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + facet_wrap(~school)
ggplot(jspr, aes(x=raven, y=english)) + geom_point() + geom_smooth(method="lm", se=F) +
  facet_wrap(~school)
# random slope for raven?

# interaction plot
with(jspr, interaction.plot(x.factor = social,trace.factor = gender,response = english))


lmeEng0 <- lmer(english ~ 1 + (1 | school), data=jspr)
summary(lmeEng0)
lmeEng0a <- lmer(english ~ 1 + (1 | school/class), data=jspr)
summary(lmeEng0a)

anova(lmeEng0, lmeEng0a, refit=FALSE)
# looks like we should keep the class within school random effect

lmeEng1 <- lmer(english ~ raven + gender*social + (1 | school/class), data=jspr)
summary(lmeEng1)
print(summary(lmeEng1),corr=FALSE) # without the correlation matrix of fixed-effect coefficients.
VarCorr(lmeEng1)
fixef(lmeEng1)
ranef(lmeEng1)
coef(lmeEng1)
anova(lmeEng1) # sequential F tests

# If you want p-values
# library(car)
Anova(lmeEng1) # a Type-II test
# Type-II tests are calculated according to the principle of marginality,
# testing each term after all others, except ignoring the term's higher-order
# relatives;


# remove interaction for gender and social
lmeEng2 <- lmer(english ~ raven + gender + social + (1 | school/class), data=jspr)
print(summary(lmeEng2), corr=FALSE)
VarCorr(lmeEng2)
fixef(lmeEng2)
ranef(lmeEng2)
coef(lmeEng2)
anova(lmeEng2)
Anova(lmeEng2)

# check model fit
plot(lmeEng2, english ~ fitted(.) | class, abline = c(0,1))

# visualoze model fit for population
plot(allEffects(lmeEng2))


lmeEng3 <- lmer(english ~ raven + gender + social + (raven | school/class), data=jspr)
anova(lmeEng2, lmeEng3, refit=FALSE)
# corrected p-value
pvalMix(8.0432, df=4)
# looks like we could safely do without a random effect for raven



