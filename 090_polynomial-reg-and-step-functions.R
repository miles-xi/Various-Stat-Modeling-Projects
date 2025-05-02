library(ISLR2)  #for data
attach(Wage)

# 7.8.1 polynomial regression and step functions

fit = lm(wage ~ poly(age, 4), data=Wage)  # poly() returns a matrix of orthogonal polynomials
coef(summary(fit))

# or, use raw polynomials
# predictions won't change, coefficient estimates change
fit2 = lm(wage ~ poly(age, 4, raw=TRUE), data=Wage)  
coef(summary(fit2))

# other equivalent ways
fit2a = lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)  # I(): a wrapper function
coef(fit2a)

fit2b = lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)

# get predictions and their SEs, using the generic predict()
age_range = range(age)
age_grid = seq(from = age_range[1], to = age_range[2])
preds = predict(fit, newdata= list(age=age_grid), se=TRUE)
se_bands = cbind(preds$fit + 2* preds$se.fit, preds$fit - 2* preds$se.fit)

# plot the data and the fit
par(mfrow=c(1, 2), mar=c(4.5, 4.5, 1, 1), oma=c(0, 0, 4, 0)) 
plot(age, wage, col='darkgrey', cex=0.5)
title('Degree 4 polynomial', outer = T)
lines(age_grid, preds$fit, lwd=2.5, col='blue')
matlines(age_grid, se_bands, lwd=1.5, col='blue', lty=3)

# predictions from fit or fit2 are essentially same
preds2 = predict(fit2, newdata= list(age=age_grid))
max(abs(preds$fit - preds2))

# choose an optimal degree of the polynomial
# F test using anova(), or cross-validation
m1 = lm(wage ~ age, data=Wage)
m2 = lm(wage ~ poly(age, 2), data=Wage)
m3 = lm(wage ~ poly(age, 3), data=Wage)
m4 = lm(wage ~ poly(age, 4), data=Wage)
m5 = lm(wage ~ poly(age, 5), data=Wage)
anova(m1, m2, m3, m4, m5)   #compare each model with its previous model


# logistic regression
glm = glm(I(wage > 250) ~ poly(age, 4), data=Wage, family = binomial)

pred_link = predict(glm, newdata = list(age=age_grid), se=TRUE)  # this uses default: type='link'

prob_1 = exp(pred_link$fit) / (1 + exp(pred_link$fit))

se_bands_logit = cbind(pred_link$fit + 2* pred_link$se.fit,
                       pred_link$fit - 2* pred_link$se.fit)
se_bands = exp(se_bands_logit) / (1 + exp(se_bands_logit))


# use type='response'
pred_response = predict(glm, newdata = list(age=age_grid), se=TRUE, type = 'response')
prob_2 = pred_response$fit
max(abs(prob_1 - prob_2))   # == 0, equivalent

# type='response' gives SE derived using the delta method
se_bands_2 = cbind(prob_2 + 2* pred_response$se.fit,
                  prob_2 - 2* pred_response$se.fit)  #this gives negative probability lower bounds

# plot the fit and the prediction
plot(age, I(wage>250), xlim=age_range, type='n', ylim=c(0, 0.2))
points(jitter(age), I((wage>250) / 5), cex=0.5, pch='|', col='darkgray')
lines(age_grid, prob_1, lwd=2.5, col='blue')
matlines(age_grid, se_bands, lwd=1.5, col='blue', lty=3)


# fit a step function
table(cut(age, 4))   # cut() puts observations of 'age' into bins
lm = lm(wage ~ cut(age, 4), data=Wage)
coef(summary(lm))





