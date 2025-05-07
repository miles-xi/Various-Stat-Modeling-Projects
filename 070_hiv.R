#### data preparation ####
# data is shared upon request at https://airtable.com/appRssVqTxNurdU2E/shrVDP51W5J2qcNeT
# then in google drive -> MACS PDS (public data set) -> three folders
# download, unzip, and convert .dta (SAS) to .dat (STATA) then load them to R
library(haven)  #for reading Stata files

# lab result data, select CD4 cell count and viral load, for each subject
lab = read_dta("~/Desktop/lab_rslt.dta")
lab = as.data.frame(lab[, c("CASEID", "VISIT", "LEU3N", "VLOAD")])  #"RO2VL", "R2SVL", "RTQ2VL", "RCOBVL" not necessary

# HIV status data, select subjects who experienced seroconvertion
status = as.data.frame(read_dta("~/Desktop/hivstats.dta"))
seroconversion = status[status$STATUS==4, ]


# from all subjects in 'lab', select those with seroconversion
data_match = lab[lab$CASEID %in% seroconversion$CASEID, ]

# add a column, POSVIS, indicating the first positive visit
data_match$POSVIS = seroconversion$POSVIS[match(data_match$CASEID, seroconversion$CASEID)]

# keep visits after or at seroconversion only
lab_post_sroconv = data_match[data_match$VISIT >= data_match$POSVIS, ]

# all subjects
subjects = unique(lab_post_sroconv$CASEID)

# remove subjects whose viral load at seroconversion is NA
valid_subjects = c()  #keep these
for (id in subjects){
  data_subset = lab_post_sroconv[lab_post_sroconv$CASEID == id, ]  #select a subset of data
  viral_load_at_posvis = data_subset[1, 'VLOAD']   #the first (1) row
  if (!is.na(viral_load_at_posvis)){
    valid_subjects = c(valid_subjects, id)
  }
}

lab_post_sroconv = lab_post_sroconv[lab_post_sroconv$CASEID %in% valid_subjects, ]

# determine the category (low, medium, high) of the viral load of each subject
get_category = function(vl){
  if (vl < 15000){
    return('low')
  } else if (vl > 46000){
    return('high')
  } else {
    return('medium')
  }
}

vl_class = data.frame(
  CASEID = numeric(),
  vload = numeric(),
  category = character()
)

for (id in valid_subjects){
  data_subset = lab_post_sroconv[lab_post_sroconv$CASEID == id, ]  #get a subset by id
  viral_load = data_subset[1, 'VLOAD']  #get the first viral load (vl)
  category = get_category(viral_load)
  vl_class = rbind(vl_class, data.frame(CASEID = id, vload=viral_load, category=category))
}

# merge back to the main dataset, 'lab_post_sroconv'
lab_post_sroconv = merge(lab_post_sroconv, vl_class, by='CASEID')

# convert the visit number (e.g., 10, 20) to year (0, 1, 2)
lab_post_sroconv$year = ((lab_post_sroconv$VISIT - lab_post_sroconv$POSVIS) / 10) / 2
lab_post_sroconv$year = round(lab_post_sroconv$year * 2) / 2   #round values such as 0.45 to 0.5
lab_post_sroconv$year_group = floor(lab_post_sroconv$year)  #group two consecutive visits (e.g., 0.0 and 0.5) in a year into a bin (e.g., 0-1)
write.csv(lab_post_sroconv, "lab_post_sroconv.csv", row.names = FALSE)


#### EDA ####
rm(list = ls())
data = read.csv("lab_post_sroconv.csv")
data = data[data$year < 5, ]   #only use observations of the first 4 years

# exclude observations with less than 3 observations
data = subset(data, ave(CASEID, CASEID, FUN=length) >2)

#### 1.2.1 group means over time ####
# average response (mean CD4 count), grouped by year & initial viral load
get_mean_and_se = function(x){
  mean = mean(x, na.rm=TRUE)
  se = sd(x) / sqrt(sum(!is.na(x)))
  n = sum(!is.na(x))
  return(c(mean=mean, se=se, n=n))
}
summary_tbl = aggregate(LEU3N ~ year + category,
                        data = data,
                        FUN = get_mean_and_se)
summary_tbl$mean_CD4 = summary_tbl$LEU3N[, "mean"]
summary_tbl$se_CD4 = summary_tbl$LEU3N[, "se"]
summary_tbl$n = summary_tbl$LEU3N[, "n"]
summary_tbl$LEU3N = NULL  #drop the column

# three lines in a single plot
library(ggplot2)
ggplot(summary_tbl, aes(x=year, y=mean_CD4, color=category)) +
  geom_line(linewidth=0.7) + 
  geom_point(size=1) +
  geom_errorbar(aes(ymin=mean_CD4 - 1.96*se_CD4, ymax=mean_CD4 + 1.96*se_CD4), width=0.2, alpha=0.7) + 
  labs(title = "CD4 Cell Count Over Time", y='Mean CD4') + 
  theme_minimal()

# three lines in three subplots
ggplot(summary_tbl, aes(x=year, y=mean_CD4, color=category)) +
  geom_line(linewidth=0.7) + 
  geom_point(size=1) +
  geom_errorbar(aes(ymin=mean_CD4 - 1.96*se_CD4, ymax=mean_CD4 + 1.96*se_CD4), width=0.2, alpha=0.7) + 
  facet_wrap(~category) + 
  labs(title = "CD4 Cell Count Over Time", y='Mean CD4') + 
  theme_minimal()

# a table
summary_tbl[order(summary_tbl$year, summary_tbl$category), ]

#### 1.2.2 Variation among individuals ####
# plot the CD4 trajectories for randomly selected subjects
set.seed(19890604)
temp = subset(data,
              CASEID %in% sample(unique(data$CASEID), 20))
ggplot(temp, aes(x=year, y=LEU3N)) +
  geom_line(linewidth=0.5) +
  geom_point(size=0.9) +
  facet_wrap(~ CASEID) +
  labs(title = "Individual CD4 Trajectories", y='CD4 Cell Count') +
  theme_bw()

# plot individual series stratified by covariate group
set.seed(19890604)
temp = subset(data,
              CASEID %in% sample(unique(data$CASEID), 90))
ggplot(temp, aes(x=year, y=LEU3N, group=CASEID)) +
  geom_line(linewidth=0.4) + 
  geom_point(size=0.5) +
  facet_wrap(~ category, ncol=3) + 
  labs(title = "CD4 Trajectories by Baseline Viral Load", y='CD4 Cell Count') +
  theme_minimal()
  
#### 1.2.3 Characterizing correlation and covariance #### 
# an array of scatter plots showing Y's at year j vs. Y's at year k.
library(tidyr)
temp = data[, c('CASEID', 'LEU3N', 'year')]
data_wide = pivot_wider(temp,
                        names_from = year,
                        values_from = LEU3N,
                        names_prefix = 'year_')
#data_wide  = na.omit(data_wide)  #not necessary

library(GGally)  #for ggpairs
ggpairs(data_wide[, -1],
        lower = list(continuous = wrap('points', size = 0.8, alpha = 0.5)),
        title = "CD4 Count Correlations Between Times"
        ) + 
  theme_bw()
# diagonal plots is the marginal distribution of the variables
# the plot shows within-person correlations 
# are high for observations close together in time
# but the correlation tends to decrease with increasing time separation 
# between the measurement times


#### 1.3 Derived variable analysis #### 
# 1) regress CD4 on time for each subject
# CD4_ij = beta0_i + beta1_i * t_ij + epsilon_ij
ids = unique(data$CASEID)
slopes = data.frame(
  CASEID = ids,
  slopes = numeric(length = length(ids))
  )

for (i in seq_along(ids)){
  temp = data[data$CASEID %in% ids[i], ]
  lm = lm(LEU3N ~ year, data=temp)
  slopes[i, 'slopes'] = coef(lm)[2]
}

slopes = merge(slopes, data[, c('CASEID', 'vload', 'category')], by='CASEID')
slopes = unique(slopes)

plot(slopes ~ log(vload), data=slopes,
     xlab = "initial viral load (log)", ylab = "slope",
     main = "Individual Slope vs Log(Baseline Viral Load)"
     )
abline(h = 0, lty = 2)
lm = lm(slopes ~ log(vload), data=slopes)  #significant at 10%
abline(lm)

# 2) the mean slopes grouped by the baseline viral load category
summarize_slopes_by_category = function(x){
  n = length(x)
  m = mean(x)
  se = sd(x) / srqt(n)
  return(c(mean=m, se=se, n=n))
}

slopes_tbl = aggregate(
  slopes ~ category, data=slopes, FUN = summarize_slopes_by_category
)
#   category slopes.mean  slopes.se   slopes.n
# 1     high  -72.921144   9.620864 175.000000
# 2      low  -61.581050   7.513668 182.000000
# 3   medium  -71.699915   9.182269 104.000000

# 3) data attrition
obs_per_subject = table(data$CASEID)
obs_summary = table(obs_per_subject)
obs_summary = data.frame(
  'Number of obs' = as.integer(names(obs_summary)),
  'Number of subjects' = as.integer(obs_summary) 
)

#### 1.5 Regression #### 
# 1)
# mu_ij := E(Y_ij | x_ij) = (beta0 + beta2 * L_ij + beta3 * M_ij) + 
#                           (beta1 + beta4 * L_ij + beta5 * M_ij) * month_ij
# where M_ij, H_ij are dummies indicating the baseline viral load
# The first model is a random intercept model
# Y_ij = mu_ij + b_i0 + epsilon_ij
# the 2nd model: Y_ij = mu_ij + b_i0 + b_i1 * month_ij + epsilon_ij
library(lme4)
data$category = as.factor(data$category)
lme1 = lmer(LEU3N ~ category*year + (1|CASEID), data=data)
summary(lme1)
# Fixed effects:
#                     Estimate  Std. Error t value
# (Intercept)          603.001     18.975  31.779
# categorylow          167.557     26.545   6.312
# categorymedium        80.506     30.853   2.609
# year                 -48.511      3.698 -13.120
# categorylow:year      -6.000      5.128  -1.170
# categorymedium:year   -7.962      5.888  -1.352

# estimated beta1 = -48.51. This means for subjects with high baseline viral load,
# the rate of decline in CD4 count is 48.511.
# For subjects with low baseline viral count, the estimated rate of decline is 48.51 + 6 = 54.51
# For medium, the estimated rate of decline is 48.51 + 8.0 = 56.51
# However, these interaction terms are not likely significant


lme2 = lmer(LEU3N ~ category*year + (1+year|CASEID), data=data)
summary(lme2)
# Fixed effects:
#                     Estimate Std. Error t value
# (Intercept)          609.532     19.769  30.833
# categorylow          160.920     27.638   5.822
# categorymedium        80.226     32.139   2.496
# year                 -56.109      6.813  -8.235
# categorylow:year      -1.046      9.462  -0.111
# categorymedium:year   -6.420     11.028  -0.582

# note that the t-values for the interaction terms categorylow/medium:year 
# dramatically decreased after adding the random slope

#overall model significance test & test the random effect
lm_null = lm(LEU3N ~ 1, data=data)
anova(lme2, lme1, lm_null)  #the two mixed effects models are highly significant 

# 2) check model assumptions
library(performance)  # to visually check assumptions
check_model(lme2, check=c('linearity', 'homogeneity', 'qq', 'outliers'))

# below, we focus on "homogeneity" and "outliers"
# homogeneity of variance
library(broom.mixed)
data = augment(lme2)
data$semi_std_res = data$.resid / sqrt(1 - data$.hat)
par(mfrow=c(1,2))
plot(semi_std_res ~ year, data=data)
plot(semi_std_res ~ .fitted, data=data)

# influential points 
as.data.frame(data[order(data$.cooksd, decreasing=TRUE), ][1:10, c(2:10)])
#    LEU3N category  year CASEID  .fitted    .resid     .hat  .cooksd   .fixed
# 1   3015     high  0.0   3210 1605.5953 1409.4047 0.3086153 9.609018 609.5324
# 2   2053     high  0.0   5600 1063.6584  989.3416 0.2918011 4.266764 609.5324
# 3    492      low  0.5   1293 1062.2661 -570.2661 0.3490226 2.006810 741.8751
# 4   1660     high  0.0   1007 1045.2060  614.7940 0.2918011 1.647652 609.5324
# 5   1684      low  0.0   5218 1191.5573  492.4427 0.3379588 1.400989 770.4525
# 6   1250      low  0.0   4690  836.5208  413.4792 0.3857444 1.309599 770.4525
# 7   1613     high  0.0   3143 1213.1184  399.8816 0.3931474 1.279032 609.5324
# 8    414     high  0.0   3453  851.6165 -437.6165 0.3461652 1.161889 609.5324
# 9   1210     high  0.0   4603  773.0764  436.9236 0.3461652 1.158212 609.5324
# 10  1163      low  0.5   7163  684.6683  478.3317 0.3140543 1.144230 741.8751

# the 1, 2, 4, 5, and 7 rows have more than 1500 CD4 cells 
# which are beyond the normal range (500 - 1500)
# 3 and 8 have less than 500 CD4 cells


















