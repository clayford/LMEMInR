# Clay Ford (jcf2d)
# StatLab@UVa
# Linear Mixed-Effect Modeling with R
# Fall 2015

# pkgs <- c("lme4","ggplot2", "faraway","effects")
# install.packages(pkgs)

library(lme4)
library(ggplot2)
library(effects)
library(faraway)


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
pairs(ranef(lme3)[[1]])

# check normality of random effects
qqnorm(ranef(lme3)[[1]]$"(Intercept)") # intercept
qqnorm(ranef(lme3)$subject$weeks) # slope




# Model Comparison --------------------------------------------------------


# compare models 
# comparisons are sequential (1 vs. 2, 2 vs. 3)
anova(lme1, lme2, lme3)
# lme3 appears to be the "best" model

# notice models are re-fit with ML


