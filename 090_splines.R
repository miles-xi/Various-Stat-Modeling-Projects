rm(list=ls())
library(MASS)
library(splines)
# references:
# ISL, ESL
# stats.stackexchange.com/questions/172217/

#### B-spline example ####
# B-spline basis (cf. truncated power basis)
# degree d=1, K=3 knots, hence K+d=4 (df in R) basis functions
# hence K+d+1=5 params to be estimated (df in ESL) in regression model
x = seq(0, 1, by=0.05)
bs(x, degree=1, df=4)   # K=3 knots are: 0.25, 0.50, 0.75
matplot(x, bs(x, degree=1, df=4))

# degree d=2, K=3 knots, K+d=5 (df in R) basis functions
bs(x, degree=2, df=5)
matplot(x, bs(x, degree=2, df=5))

# and so on...

#### fit to the data ####
# bookdown.org/ssjackson300/Machine-Learning-Lecture-Notes/splines.html#prac-reg-splines
# Chap 9.2 only
data("Boston")
y = Boston$medv
x = Boston$lstat
xlab='Lower status (%)'
ylab = 'Median property value'

plot(x, y, col='gray', xlab=xlab, ylab = ylab, main='', bty='l')

# find knots based on percentiles
summary(x)
cuts = c(6.95, 11.360, 16.955)
x_sorted = sort(x)

# fit a linear spline
sp1 = lm(y ~ bs(x, degree=1, knots=cuts))
summary(sp1)
pred1 = predict(sp1, newdata = list(x = x_sorted), se = TRUE)
se = cbind(pred1$fit + 2 * pred1$se.fit, pred1$fit - 2 * pred1$se.fit)

# plot the linear spline
par(mfrow=c(1, 3))
plot(x, y, col= 'gray', xlab=xlab, ylab=ylab, main='linear spline (df=1)', bty='l')
lines(x_sorted, pred1$fit, lwd=2.5, col='red')
matlines(x_sorted, se, lwd=1.5, col='red', lty=3)

# fit a quadratic and a cubic spline
sp2 = lm(y ~ bs(x, degree=2, knots=cuts))
pred2 = predict(sp2, newdata = list(x = x_sorted), se = TRUE)
se2 = cbind(pred2$fit + 2* pred2$se.fit, pred2$fit - 2* pred2$se.fit)

sp3 = lm(y ~ bs(x, degree=3, knots=cuts))
pred3 = predict(sp3, newdata = list(x = x_sorted), se = TRUE)
se3 = cbind(pred3$fit + 2* pred3$se.fit,pred3$fit - 2* pred3$se.fit)

# plot the quadratic and cubic splines
plot(x, y, col="grey",
     xlab = xlab, ylab = '', 
     main = "Quadrtic Spline, df=2", bty = 'l')
lines(x_sorted, pred2$fit, lwd = 2, col = "blue")
matlines(x_sorted, se2, lwd = 2, col = "blue", lty = 3)

plot(x, y, col="grey",
     xlab = xlab, ylab = '', 
     main = "Cubic Spline, df=3", bty = 'l')
lines(x_sorted, pred3$fit, lwd = 2, col = "coral")
matlines(x_sorted, se3, lwd = 2, col = "coral", lty = 3)

#### natural splines part 1 - "half" natural ####
# https://rpubs.com/enwuliu/1004385
rm(list=ls())
library(MultiKink)  #for data
library(ggplot2)
library(splines)
library(rms)  #regression modeling strategies

data('triceps')
data_plot = ggplot(triceps, aes(x=age, y=triceps)) +
  geom_point(alpha=0.3) + 
  theme_minimal()
data_plot

# a cubic spline regression model with five knots, represented by truncated power function
# y = b0 + b1*x + b2*x^2 + b3*x^3 + 
#     theta1* (x-t1)_+^3 + theta2* (x-t2)_+^3 + ... + theta5 * (x-t5)_+^3 + epsilon
# where the knots are c(5, 10, 20, 30, 40)
lm_unatural = lm(triceps ~ age + I(age^2) + I(age^3) + 
                   I((age-5)^3 * (age>=5)) + 
                   I((age-10)^3 * (age>=10)) + 
                   I((age-20)^3 * (age>=20)) + 
                   I((age-30)^3 * (age>=30)) +
                   I((age-40)^3 * (age>=40)), data = triceps)
coef(summary(lm_unatural))   #total 9 params, 9 = (d=3) + (K=5) + 1

# also use bs() for comparison
lm_unatural2 = lm(triceps ~ bs(age, knots = c(5, 10, 20, 30, 40), degree=3), data=triceps)
coef(summary(lm_unatural2))   #also total 9 params, 9 = (8 = #B-splines) + 1

# plot the models
# coefficient estimates are not equal, but predictions are the same
data_plot +
  geom_line(data=triceps, aes(y=predict(lm_unatural)), col='blue', linewidth=2.5) + 
  geom_line(data=triceps, aes(y=predict(lm_unatural2)), col='lightblue', linewidth=1)


# 'half-natural' cubic splines
# where we constrain the spline function to be linear at the left boundary (x<5) only
# by imposing f''(x) = 0 for any x<5 on the truncated power basis
# this leads to b2=0, b3=0
# so the model now is
# y = b0 + b1*x + 
#    theta1* (x-t1)_+^3 + theta2* (x-t2)_+^3 + ... + theta5 * (x-t5)_+^3 + epsilon
lm_half_natural = lm(triceps ~ age +
                   I((age-5)^3 * (age>=5)) + 
                   I((age-10)^3 * (age>=10)) + 
                   I((age-20)^3 * (age>=20)) + 
                   I((age-30)^3 * (age>=30)) +
                   I((age-40)^3 * (age>=40)), data = triceps)

# plot the 'half-natural' model to compare
# we can see the half-natural is linear before the first knot (t1=5)
data_plot +
  geom_line(data=triceps, aes(y=predict(lm_unatural)), col='blue', linewidth=3.5) + 
  geom_line(data=triceps, aes(y=predict(lm_unatural2)), col='lightblue', linewidth=3) +
  geom_line(data=triceps, aes(y=predict(lm_half_natural)), col='white', linewidth=1)

#### natural splines part 2 - maths & manual calculation ####

# natural cubic splines are obtained by 
# constraining the spline function to be linear at the both boundaries (x<5 & x>=50)
# To do so, impose f''(x) = 0 for any x<5 and f''(x) = 0 for any x>=50 on the truncated power basis
# This gives a new representation of the truncated power basis
# and now the model is
# y = b0 + b1*x + 
#   theta1 * s1(x) + theta2 * s2(x) + theta3 * s3(x) + epsilon

# Generally, the truncated power basis representation of a cubic spline: 
# y = b0 + b1*x + b2*x^2 + b3*x^3 + 
#   theta1 * (x-t1)_+^3 + theta2 * (x-t2)_+^3 + ... + thetaK * (x-tK)_+^3 + epsilon
# Natural cubic spline:
# y = b0 + b1*x + 
#   theta1 * s1(x) + theta2 * s2(x) + ... + theta_(K-2) * s_K-2_(x) + epsilon
# where sk(x) = (tK-tk) * [dk(x) - d_K-1_(x)], k=1, 2,..., K-2
# where dk(x) = ( (x-tk)_+^3 - (x-tK)_+^3 ) / (tK - tk)

get_dk = function(x, tk){
  numerator = (x-tk)^3 * (x>=tk) - (x-40)^3 * (x>=40)
  denominator = 40 - tk
  dk = numerator / denominator
  return(dk)
}
get_sk = function(x, tk){
  sk = (40-tk) * (get_dk(x, tk) - get_dk(x, 30))
  sk = sk / (40-5)^2  #scale for numerical reasons, this scales a predictor in OLS which is scaling invariant
  return(sk)
}

# new predictors s1, s2, s3
s1 = get_sk(triceps$age, 5)
s2 = get_sk(triceps$age, 10)
s3 = get_sk(triceps$age, 20)
triceps_new = cbind(triceps, s1, s2, s3)

ns_by_hand = lm(triceps ~ age + s1 + s2 + s3, data = triceps_new)
coef(summary(ns_by_hand))  # exactly the same as using rcs() of the rms library

# using rcs(), same results as above
# estimated coefficients are
# b0 = 8.56, b1 = -0.30, 
# theta1 = 7.34, theta2 = -13.05, theta3 = 7.71
lm_full_natural = ols(triceps ~ rcs(age, parms=c(5,10,20,30,40)),
                      data=triceps)
lm_full_natural


#### natural splines part 3 - compare with ns() ####
ns_using_splines = lm(triceps ~ ns(age, knots = c(5,10,20,30,40)),
                      data=triceps)

# outputs are different from hand calculation (ns_by_hand) 
# and from using rcs() (lm_full_natural)
coef(summary(ns_using_splines))  

# predictions are very similar
data_plot +
  geom_line(data=triceps, aes(x=age, y=predict(lm_full_natural)), size=3.5, col='red') +
  geom_line(data=triceps, aes(x=age, y=predict(ns_by_hand)), size=3, col='orange') +
  geom_line(data=triceps, aes(x=age, y=predict(ns_using_splines)), size=1, col='green')









