library(AER)
data("NMES1988")
nmes = NMES1988[, c(1, 6:8, 13, 15, 18)]
str(nmes)  # variables include years of education, number of hospital stays

plot(table(nmes$visits))  # 
sum(nmes$visits < 1)  # close to 700 people had 0 visits
sum(nmes$visits > 50)  # 13 people had more than 50 visits

# model the count data using Poisson regression
mod1 = glm(visits ~., data = nmes, family = 'poisson')

# predict the expected mean count E(y) := mu, for each observation
mu = predict(mod1, type='response')
head(mu)

# Find the probability of count = 0 for each observation, that is
# Find P(Y=0) = exp(-lambda) * lambda^0 / 0!
head(dpois(x=0, mu))

# sum these probs of count = 0 
# which gives the expected number of zero counts over n observations
sum(dpois(x=0, mu))  # 46 << 700


# hurdle model
library(pscl)  # Poli Sci Computational Lab
mod_hurdle = hurdle(visits~.,
                    data = nmes,
                    dist = 'poisson',  # the distr. of positive count
                    zero.dist = 'binomial')  # the distr. of zero-count
summary(mod_hurdle)

head( predict(mod_hurdle, type = 'prob') )  # this returns P(Y=0), P(Y=1), ...,P(Y=89) for each obs.
# a 4406 x 90 matrix 

# get the expected number of zero counts over n observations
sum( predict(mod_hurdle, type='prob')[,1] ) # select the first column, then sum the probs

# get expected hurdle count E(Y|x) 
# where Y ~ hurdle poisson model with params p|x and mu|x

# we can derive that E(Y|x) = ... = (1-p) / (1 - exp(-mu)) * mu (see down below)
predict(mod_hurdle, type = 'zero')[1:5]  # the first part
predict(mod_hurdle, type = "count")[1:5]  # the second part
predict(mod_hurdle, type = "zero")[1:5] * predict(mod_hurdle, type = "count")[1:5]  # multiply

# the multiplication == type = 'response'
predict(mod_hurdle, type = "response")[1:5]

# assess the model fit
# requires rootogram()
#  The smooth red line is the theoretical Poisson curve. 
#  This is a "hanging rootogram",
#  so the bars which represent the difference between observed and predicted counts
#  "hang" from the curve.

# see underfitting at higher counts which points to overdispersion
# the variability of data >> Pois distr 
# this suggest using negative binomial 
mod_hurdle_nb = hurdle(visits~.,
                    data = nmes,
                    dist = 'negbin',  # the distr. of positive count
                    zero.dist = 'binomial')  # the distr. of zero-count

# compare two models
AIC(mod_hurdle)
AIC(mod_hurdle_nb)  # smaller, better

# each component can have different sets of predictors, use |
mod_hurdle_nb2 = hurdle(visits ~ . | gender + insurance, 
                         data = nmes,
                         dist = "negbin")



# Derive E(Y|x) = ... = (1-p) / (1 - exp(-mu)) * mu where Y ~ hurdle model(p, mu)
# P(Y=0) = p
# P(Y=1) = (1-p)/(1 - exp(-mu)) * exp(-mu) * mu^1 / 1!
# P(Y=2) = (1-p)/(1 - exp(-mu)) * exp(-mu) * mu^2 / 2!
# ...
# P(Y=k) = (1-p)/(1 - exp(-mu)) * exp(-mu) * mu^k / k!

# E(Y|x) = 0 * p + 1 * P(Y=1) + 2 * P(Y=2) + ...
# = (1-p)/(1 - exp(-mu)) * exp(-mu) * (mu^1 / 0! + mu^2 / 1! + ... + mu^n / (n-1)! + ...)
# = (1-p)/(1 - exp(-mu)) * exp(-mu) * mu * (mu^0 / 0! + mu^1 / 1! + ... + mu^(n-1) / (n-1)! + ...)   ## This is Maclaurin series,  = exp(mu)
# = (1-p)/(1 - exp(-mu)) * mu


# references: https://library.virginia.edu/data/articles/getting-started-with-hurdle-models





