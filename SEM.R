## Load Libraries
library(lavaan)
library(lavaanPlot)
library(dplyr) 
library(tidyr)
library(knitr)
library(mvnormalTest)

attach(data)
# We use the mvnormalTest package 
# for univariate (Shapiro-Wilk’s W) and multivariate normality 
# (Mardia’s Multivariate Skewness and Kurtosis tests).

mvnout <- mardia(data)
## Shapiro-Wilk Univariate normality test
mvnout$uv.shapiro

## Mardia Multivariate normaility test
mvnout$mv.test

# std.lv:If TRUE, the metric of each latent 
# variable is determined by fixing their 
# (residual) variances to 1.0. If FALSE, 
# the metric of each latent variable is 
# determined by fixing the factor loading 
# of the first indicator to 1.0. 

## Model specification
model <- '
  attitudes =~ sex.fool + sex.harm
  norms =~ frnd.sex + love.sex
  control =~ self.cntl + how.ref
  intention =~ int.abs + int.avoid + sex.fool
  intention ~ attitudes + norms + control
'
## sem function syntax
fit.mod <- sem(model, data=data, std.lv = TRUE, estimator = "MLM",optim.method="BFGS")
summary(fit.mod)

## fit measures
fitMeasures(fit.mod, c("chisq.scaled", "df.scaled", "pvalue.scaled"))
fitMeasures(fit.mod, c("rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "rmsea.pvalue.scaled"))
fitMeasures(fit.mod, c("cfi.scaled", "srmr"))


#parameter estimates of measurement model
standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "=~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

#R2 values
parameterEstimates(fit.mod, standardized=TRUE, rsquare = TRUE) %>% 
  filter(op == "r2") %>% 
  select(Item=rhs, R2 = est)

#Structural Model
standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

# covariance 
standardizedsolution(fit.mod, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, ci = TRUE)%>% 
  filter(op == "~~") %>% 
  select(LV=lhs, Item=rhs, Coefficient=est.std, ci.lower, ci.upper, SE=se, Z=z, 'p-value'=pvalue)

