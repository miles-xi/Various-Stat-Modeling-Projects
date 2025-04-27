library(lme4)  # the most common and best choice of package
data('sleepstudy')
head(sleepstudy)
#https://cambiotraining.github.io/stats-mixed-effects-models/materials/05-fitting-mixed-models.html

# visualize the data using 
library(ggplot2)
ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point() +
  geom_smooth(method = "lm")

#### 5.3 Adding random intercepts ####
# linear mixed effects model. treat Subject as a random effect
# Y_{ij} = beta0 + beta1 * x_{ij} + gamma_i + epsilon_{ij}
# <=> Y_{ij} = (beta0 + gamma_i) + beta1 * x_{ij} + epsilon_{ij}
# where i refers to group i, and j observation j
lme_sleep1 = lmer(Reaction ~ Days + (1|Subject), data=sleepstudy)
summary(lme_sleep1)

# visualize the fitted model alongside the standard linear model
# can extract estimated random effects using ranef(lme_sleep1)
lm_sleep = lm(Reaction ~ Days, data=sleepstudy)  #create a linear model

library(broom)
library(broom.mixed)
ggplot(sleepstudy, aes(x=Days, y=Reaction)) + 
  facet_wrap(facets=vars(Subject), nrow=3) +   #create separate plots for each subject in the sample
  geom_point() +
  geom_line(data=augment(lm_sleep), aes(y=.fitted)) +   # without random effect
  geom_line(data=augment(lme_sleep1), aes(y=.fitted), color='blue')    # with random effect

head(ranef(lme_sleep1))  #these are the vertical distances 

# another visualization, show all fitted lines together
ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  geom_point() + 
  geom_line(data=augment(lm_sleep), aes(y=.fitted)) +
  geom_line(data=augment(lme_sleep1), aes(y=.fitted, group=Subject), color='lightblue')
# the same slope means people get worse at the same rate
# intercepts being different means people's baseline Reaction is different


#### 5.4 add random slopes ####
# Y_{ij} = beta0 + beta1 * x_{ij} + gamma0_i + gamma1_i * x_{ij} + epsilon_{ij}
# Y_{ij} = (beta0 + gamma0_i) + (beta1 + gamma1_i) * x_{ij} + epsilon_{ij}
lme_sleep2 = lmer(Reaction ~ Days + (1 + Days | Subject), data=sleepstudy)

# visualize
ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  facet_wrap(facets = vars(Subject), nrow=3) +
  geom_point() +
  geom_line(data=augment(lm_sleep), aes(y=.fitted)) +   #global line
  geom_line(data=augment(lme_sleep1), aes(y=.fitted), color='blue') +   #random intercept only
  geom_line(data=augment(lme_sleep2), aes(y=.fitted), color='red')  #random intercept and slope

# extract random effect adjustments, predicted random effect values
ranef = ranef(lme_sleep2)$Subject
c(sd(ranef$Days), sd(ranef[['(Intercept)']]))



#### 5.5.2 Sharing information ####
# use geom_smooth to add lines of best fit 
# if we fit each subject with their own individual regression
ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  facet_wrap(facets = vars(Subject), nrow = 3) +  
  geom_point() +
  geom_line(data=augment(lm_sleep), aes(y=.fitted)) +  #global line
  geom_line(data=augment(lme_sleep1), aes(y=.fitted), color='blue') +  # random intercept only
  geom_line(data=augment(lme_sleep2), aes(y=.fitted), color='red') +  #random intercept & slope
  geom_smooth(method='lm', se=FALSE, linewidth=0.5, color='green')   #individual regression

# black line results from ignoring Subject variable entirely -- complete pooling
# green lines result from splitting the dataset into separate groups by Subject and fitting 
#  individual regressions -- no pooling
# red and blue lines represent our mixed effects models -- partial pooling

# green and red lines look similar, but
# red lines are all closer to the black line than the green line is -- shrink towards the global line
# This is b/c when random effects are estimated, information is shared b/t subjects/groups, 
#  we take into account the global average

# decide whether to treat a variable as a random effect:
# whether to take into account the global average/share information b/t groups? 

#### a question ####
# are the following equivalent?
# 1) fit a regression model for each subject 
# 2) use all data to fit a single model 
#   but treat the Subject as a categorical variable (fixed effect)
#   the slope and an intercept depend on the Subject
# answer: yes, as both models solve the same minimization problem

# 2) model: Y_i = beta0 + beta1 * x_i1 + beta2 * x_i2 + beta3 * x_i1 * x_i2 + epsilon_i
# where x_i2 is a vector of dummy variables
# Y_i = (beta0 + beta2 * x_i2) + (beta1 + beta3 * x_i2) * x_i1 + epsilon_i
lm_sleep2 = lm(Reaction ~ Days*Subject, data=sleepstudy)
summary(lm_sleep2)

ggplot(sleepstudy, aes(x=Days, y=Reaction)) +
  facet_wrap(facets = vars(Subject), nrow = 3) +  
  geom_point() +
  geom_line(data=augment(lm_sleep2), aes(y=.fitted), color='blue') +  # 2)
  geom_smooth(method='lm', se=FALSE, linewidth=0.5, color='lightgreen')   # 1)


#### an exercise ####
rm(list = ls())
dragons = read.csv('dragons.csv')
dragons$scales = as.factor(dragons$scales)
dragons$mountain = as.factor(dragons$mountain)

# vis 1
par(mfrow=c(1,3))
plot(dragons$wingspan, dragons$intelligence, xlab = "wingspan", ylab = "intelligence")
abline(lm(intelligence ~ wingspan, data = dragons))
plot(dragons$scales, dragons$intelligence, xlab = "scales", ylab = "")
plot(dragons$mountain, dragons$intelligence, xlab = "mountain", ylab = "")

# vis 2, a scatter plot, where data points are distinguished by the scales
ggplot(dragons, aes(x=wingspan, y=intelligence, color=scales)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE)
# comment: as wingspan increases, so does intelligence; 
# intelligence is higher on average in metallic dragons than in chromatic dragons
# there is no interaction b/t wingspan and scales as the slopes look the same

# vis 3, scatter plot split by mountain
ggplot(dragons, aes(x=wingspan, y=intelligence, color=scales)) +
  facet_wrap(vars(mountain)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE)
# the broad impression remains the same, but the strength of the relationship
# b/t wingspan and intelligence seems to vary across different facets/mountains

ggplot(dragons, aes(x=scales, y=intelligence)) +
  facet_wrap(vars(mountain)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE)
# it's hard to tell if the relationship b/t scales and intelligence varies across facets/mountains

# fit the random effects model
# Y_ij = beta0 + beta1 * wingspan_ij + beta2*scales * wingspan_ij + gamma0_i + gamma1_i * wingspan_ij + epsilon_ij
# <=> Y_ij = (beta0 + gamma0_i) + (beta1 + beta2*scales + gamma1_i) * wingspan_ij + epsilon_ij
# where i is group/mountain i, j is j-th obs. in group i
library(lme4)
lme = lmer(intelligence ~ wingspan*scales + (1+wingspan|mountain), data = dragons)
summary(lme)

# visualize the model, 1
ggplot(data=augment(lme), aes(x=wingspan, y=intelligence, color=scales)) +
  facet_wrap(facets = vars(mountain)) +  
  geom_point() +
  geom_line(aes(y=.fitted)) 

# visualize the model, 2
ggplot(augment(lme), aes(x=wingspan, y=intelligence, color=mountain)) +
  geom_point() + 
  geom_line(aes(y=.fitted, linetype=scales))


#### 6 significance & model comparison ####
library(lmerTest)
library(pbkrtest)

# p-value 
# - needs the distribution of the test statistic under H0 
# - not easy to determine due to d.f. not easy to determine

#### 6.3 overall model significance ####
data('sleepstudy')
lm_null = lm(Reaction ~ 1, data=sleepstudy)

# Reaction_ij = (beta0 + gamma0_i) + (beta1 + gamma1_i) * Days_ij + epsilon_ij
lme = lmer(Reaction~ Days + (1+Days | Subject), data=sleepstudy, REML=FALSE)
anova(lme, lm_null)  # drop-in-deviance test, uses LRT statistic, don't require knowing the d.f.s?


#### 6.4 fixed effects ####
# method 1, LRT
# Reaction_ij = (beta0 + gamma0_i) + (gamma1_i) * Days_ij + epsilon_ij
lme_random = lmer(Reaction ~ 1 + (1+Days | Subject), data=sleepstudy, REML=FALSE)
anova(lme, lme_random)  #test the significance of Days


# method 2, approximate df
library(lmerTest)   # a companion package to lme4
# lmerTest overwrites lmer(), 
# which now uses the Satterthwaite approximation of df and thus provides p-values
# for using the unmodified version of lmer(): lme4::lmer()

#refit the model using the modified lmer()
# S. approximation is appropriate for mixed models fitted using either MLE or ReML
lme = lmer(Reaction~ Days + (1+Days | Subject), data=sleepstudy) 
summary(lme)  # now we can see t statistics and p-vals

# lmerTest also modifies anova()
anova(lme)  # F test of Days

# another option is Kenward-Roger approximation
# K-R approximation can only be applied to models fitted with ReML, so less popular


# method 3, t-to-z approximation
# suppose sample is large enough and so
# t values approximate z scores
# z distribution doesn't rely on d.f.
# so we can derive p-value based on z distribution
# use 2*pnorm(q, lower.tail=FALSE)


# method 4, parametric bootstrapping via PBmodComp() from the pbkrtest package
# can also do it manually

# summary: most use method 1 or 2; 
# 4 for more theoretical and more coding
# can try them all which helps you determine how robust your conclusions are


#### 6.5 random effects ####
# method 1, use LRT, with caveat
# the approach is the same: construct two nested models
lme_without_ranef = lmer(Reaction ~ Days + (1 | Subject), data=sleepstudy)
lme = lmer(Reaction ~ Days + (1 + Days| Subject), data=sleepstudy)
anova(lme, lme_without_ranef)

# caveat: the H0: var of random effect is 0
# which is "on the boundary," i.e., 0 is the lowest possible value for the var
# which makes the approximation to distribution of an LRT (?) break down
# p-value becomes large


# method 2, AIC/BIC, with caveat
# compare AIC/BIC values shown in anova() output
# same caveat: underestimate the importance of random effects

# method 3, bootstrap

# method 4, not testing at all
# If the random effect is important in representing the design and structure of your dataset, 
# then your model is better served by containing it, regardless of the p-value

# also, should care more about the uncertainty of the random effect (CI for the variance of the random effect)
# than about the significance

#### the exercise continued ####
lme = lmer(intelligence ~ wingspan*scales + (1+wingspan|mountain), data = dragons)

# lme vs. the null model
lm_null = lm(intelligence ~ 1, data = dragons)
anova(lme, lm_null)

# test the significance of fixed effects
#   test the interaction term
lme_drop_interact = lmer(intelligence ~ wingspan + scales + (1+wingspan|mountain), data = dragons)
anova(lme, lme_drop_interact)  #interaction term is not significant

# can further compare lme_drop_interact with models that drop wingspan or scales
# they will turn out to be significant

# visualize the final model
# intel_ij = (beta0 + gamma0_i + beta2 * scales_ij) + (beta1 + gamma1_i) * wingspan_ij + epsilon_ij
# where i is the group/mountain i; j is the j-th obs. in group i
ggplot(augment(lme_drop_interact), aes(x=wingspan, y=intelligence, color=scales)) +
  facet_wrap(vars(mountain)) +
  geom_point() +
  geom_line(aes(y=.fitted))









