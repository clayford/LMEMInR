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

ratdrink <- read.csv("https://github.com/clayford/LMEMInR/raw/master/ratdrink.csv")

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
with(ratdrink, interaction.plot(x.factor = weeks, trace.factor = treat, response = wt))

# linear mixed-effect model, no interaction between treat and week
# random intercept
lmm1 <- lmer(wt ~ weeks + treat + (1 | subject), data=ratdrink)
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


# fit model with uncorrelated random intercept and slope:
lmm3 <- lmer(wt ~ treat + weeks + (weeks || subject), data=ratdrink)
summary(lmm3)
fixef(lmm3)
ranef(lmm3)
coef(lmm3) # fitted model per group


# fit model with interaction and random slopes and intercept
lmm4 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks | subject), 
             data=ratdrink)
# or lmm4 <- lmer(wt ~ treat * weeks + (weeks | subject), data=ratdrink)
summary(lmm4)
fixef(lmm4)
ranef(lmm4)

VarCorr(lmm4)

# fit model with interaction and uncorrelated random slopes and intercept
lmm5 <- lmer(wt ~ treat + weeks + treat:weeks + (weeks || subject), 
             data=ratdrink)
summary(lmm5)


# Effect Plots ------------------------------------------------------------

# plot fitted model for lmm4: correlated random intercept and slope with
# interaction betweem weeks and treat:

# a rather complicated way using ggplot2
fe <- fixef(lmm4)
cols <- scales::hue_pal()(3) # get the colors that ggplot generated
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

# profile method
confint(lmm4)
# oldNames = FALSE changes the labeling
confint(lmm4, oldNames = FALSE)

# bootstrap method with a progress bar (nsim = 500)
confint(lmm4, method = "boot", .progress="txt")
# add a percent completion indicator
confint(lmm4, method = "boot", nsim = 200,
        .progress="txt", PBargs=list(style=3))

# assessing fixed-effect factors. In this case "treat". It has three levels.
levels(ratdrink$treat)

# sequential test (Type I), no p-values
anova(lmm4)

# test each term after all others (Type II), approx p-values
# library(car)
Anova(lmm4)


# Diagnostics -------------------------------------------------------------

# Two basic assumptions need to be checked:
# 1. within-group errors are normal, centered at 0, have constant variance
# 2. random effects are normal, centered at 0, have constant variance

# check constant variance assumption
# residual vs. fitted values
plot(lmm4)
# same as this:
plot(resid(lmm4) ~ fitted(lmm4))
abline(h=0)


# plots of residuals by weeks
plot(lmm4, form = resid(.) ~ weeks)
# by weeks and treatment
plot(lmm4, form = resid(.) ~ weeks | treat)

# residuals by subjects
plot(lmm4, subject ~ resid(.))
plot(lmm4, factor(subject) ~ resid(.))

# residuals by treat
plot(lmm4, treat ~ resid(.))

# check normality of residuals
qqnorm(resid(lmm4))

# check constant variance of random effects
plot(ranef(lmm4))

# check normality of random effects
library(lattice)
qqmath(ranef(lmm4))

# another way without lattice
qqnorm(ranef(lmm4)[[1]]$"(Intercept)") # intercept
qqnorm(ranef(lmm4)[[1]]$weeks) # slope

# plot predicted random effects for each level of a grouping factor; allows you 
# to see if there are levels of a grouping factor with extremely large or small
# predicted random effects.
dotplot(ranef(lmm4))

# check model fit
plot(lmm4, wt ~ fitted(.) | subject, abline = c(0,1))


# back to presentation

# Model Comparison --------------------------------------------------------

# do we need a random effect for weeks (ie, a random slope)?
anova(lmm1, lmm2)
# notice models are re-fit with ML

# suppress since fixed effects are the same in each model:
anova(lmm1, lmm2, refit = FALSE)

# p-value is tiny, but let's compute a corrected p-value anyway. 
# let's save the output so we can access Chi-square test statistic
str(anova(lmm1, lmm2, refit = FALSE))
aout <- na.omit(anova(lmm1, lmm2, refit = FALSE)) # drop NAs
aout$Chisq

# In the test we see that it's on 2 degrees of freedom, but recall the null
# distribution is not on 2 degrees of freedom but rather a mixture of
# distributions.
0.5*pchisq(aout$Chisq, 2, lower.tail = FALSE) + 0.5*pchisq(aout$Chisq, 1, lower.tail = FALSE)

# even smaller!

# we can write a function to automate this:
pvalMix <- function(stat,df){
  0.5*pchisq(stat, df, lower.tail = FALSE) + 
     0.5*pchisq(stat, df-1, lower.tail = FALSE)
}
pvalMix(aout$Chisq, aout$`Chi Df`)


# compare model with correlated random effects to model without correlated
# random effects:
anova(lmm2, lmm3)
# let's compute corrected p-value
pvalMix(0.9721, 1)
# still not significant; prefer lmm3, simpler model with no correlation between 
# random intercept and slope.


# let's compare lmm5 with lmm3:
formula(lmm3)
formula(lmm5)

anova(lmm3, lmm5)
# interaction appears significant

plot(lmm5)
plot(lmm5, treat ~ resid(.))
plot(ranef(lmm5))
qqmath(ranef(lmm5))

plot(lmm5, resid(.) ~ weeks | treat)
# thyroxine residuals exhibits a pattern...
# let's look at data again
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject)) + 
  geom_point() + geom_line() + facet_wrap(~ treat)

# visualize; no difference in the fixed effects
plot(allEffects(lmm5), multiline=TRUE, main="lmm5")
plot(allEffects(lmm4), multiline=TRUE, main="lmm4")

VarCorr(lmm4)
VarCorr(lmm5)



# EXAMPLE 2

# multilevel model
jspr <- read.csv("https://github.com/clayford/LMEMInR/raw/master/jspr.csv")
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



