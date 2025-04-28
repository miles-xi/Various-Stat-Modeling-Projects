# https://cambiotraining.github.io/stats-mixed-effects-models/materials/07-checking-assumptions.html

# 7.2 assumptions:
# L: Linearity
# I: independence (beyond the non-independence that we have accounted for with our random effects) - more of experimental design
# N: errors are normally distributed
# E: equal variance

# Continuous response variable - more of experimental design
# not a formal assumption: no overly influential data points

# additional assumptions due to random effects:
# random effects (coefficients) ~ Normal
# random effects are not influence by any of the other predictors


#### 7.3 testing these assumptions ####
library(performance)  # to visually check assumptions
library(lme4)
data('sleepstudy')
lme_sleep = lmer(Reaction ~ Days + (1 + Days|Subject), data=sleepstudy)

#### 7.3.1 The usual suspects #### 
check_model(lme_sleep, check=c('linearity', 'homogeneity', 'qq', 'outliers'))

# note, the second plot for testing homogeneity uses std. residuals, b/c
# variance of the residuals is sigma2 * sqrt(1-h_ii). 
# Thus unequal variance could arise from the leverages
# we remove this effect by standardizing
# standardization also divides the raw residuals by sigma; this is mainly for scaling purposes

# the third plot is "influential observations"
# which should have a large leverage and is an outlier (large std. residual)
# contour lines are not drawn probably b/c leverages are all very small

library(broom.mixed)
data = augment(lme_sleep)
data$semi_std_res = data$.resid / sqrt(1 - data$.hat)
data$std_res = data$semi_std_res/ sigma(lme_sleep)

# homogeneity
par(mfrow=c(1,3))
plot(.resid ~ .fitted, data=data)
plot(semi_std_res ~ .fitted, data=data)
plot(std_res ~ .fitted, data=data)

# normality
library(ggplot2)
qq_raw = ggplot(data, aes(sample=.resid)) +
  stat_qq() + 
  stat_qq_line(color='red') +
  labs(title='QQ plot of raw residuals')

qq_semistd = ggplot(data, aes(sample=semi_std_res)) + 
  stat_qq() +
  stat_qq_line(color='blue') + 
  labs(title='QQ plot of semi std residuals')

qq_std = ggplot(data, aes(sample=std_res)) + 
  stat_qq() +
  stat_qq_line(color='lightblue') + 
  labs(title='QQ plot of std residuals')

library(patchwork)
qq_raw + qq_semistd + qq_std


#### 7.3.2 Normality of random effects #### 
check_model(lme_sleep, check='reqq')

# there're 18 points in each plot which correspond to 18 subjects/groups

#### 7.3.3 posterior predictive check #### 

#plot(density(sleepstudy$Reaction), xlab='Reaction', main='Density of Reaction')

ggplot(sleepstudy ,aes(x=Reaction)) + 
  geom_density(fill='lightblue') +
  labs(x='Reaction', title='Density')

check_model(lme_sleep, check='pp_check', colors=c("green", "blue"))

#### exercise #### 
dragons = read.csv('GLM_content/dragons.csv')

# Y_ij = (beta0 + beta1 * scales_ij + gamma0_i) + (beta2 + gamma1_i) * wingspan_ij + epsilon_ij
lme_dropx = lmer(intelligence ~ wingspan + scales + (1+wingspan|mountain), data = dragons)

# Y_ij = (beta0 + beta1 * scales_ij + gamma0_i) + (beta2 + beta3 * scale_ij + gamma1_i) * wingspan_ij + epsilon_ij
lme_full = lmer(intelligence ~ wingspan*scales + (1 + wingspan|mountain), data=dragons)

check_model(lme_dropx, check=c('linearity', 'homogeneity', 'qq', 'outliers'))
check_model(lme_full, check=c('linearity', 'homogeneity', 'qq', 'outliers'))

check_model(lme_dropx, check=c('reqq', 'pp_check'))
check_model(lme_full, check=c('reqq', 'pp_check'))

