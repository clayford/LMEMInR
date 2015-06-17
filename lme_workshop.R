# Clay Ford (jcf2d)
# StatLab@UVa
# Linear Mixed-Effect Modeling with R
# Fall 2015

# pkgs <- c("lme4","ggplot2", "faraway")
# install.packages(pkgs)

library(lme4)
library(ggplot2)
library(faraway)

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
# wt versus weeks
ggplot(ratdrink, aes(x=weeks, y=wt)) + geom_point()
# with groups
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject)) + geom_point() + geom_line()
# with treat
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject, color=treat)) + 
  geom_point() + geom_line()
# with faceting
ggplot(ratdrink, aes(x=weeks, y=wt, group=subject)) + 
  geom_point() + geom_line() + facet_wrap(~ treat)

# linear mixed-effect model
# random intercept
lme1 <- lmer(wt ~ treat + weeks + (1 | subject), data=ratdrink)
summary(lme1)
# random intercept and random slope
lme2 <- lmer(wt ~ treat + weeks + (weeks | subject), data=ratdrink)
summary(lme2)
anova(lme1, lme2)