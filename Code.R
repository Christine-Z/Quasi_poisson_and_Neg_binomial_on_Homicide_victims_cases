###############
# GLM Group 1 #
###############

###############
# Environment #
###############

# clear workspace:  
rm(list=ls()) 

# import library
library(dplyr) # explore data
library(DHARMa) # residual qq plot
library(car) # GOF test
library(countreg) # rootogram() install.packages("countreg", repos="http://R-Forge.R-project.org")

# set working dictionary
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# import data
data <- read.table('16-victim.txt', header = TRUE) # read data
head(data)
table(data)

# resp: The number of victims the respondent knows
# race: The race of the respondent (black or white)

# Scientific question: Does race help explain how many homicide victims a person knows?

##############
# Question 1 #
##############

# Fit a Poisson model
model_pos <- glm(resp ~ race, family = poisson, data = data )
sum_pos <- summary(model_pos)
sum_pos

##############
# Question 2 #
##############

# customized risk ratio function
glm.RR <- function(GLM.RESULT, digits = 2) {
  
  if (GLM.RESULT$family$family == "binomial") {
    LABEL <- "OR"
  } else if (GLM.RESULT$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(GLM.RESULT)
  CONFINT   <- stats::confint(GLM.RESULT)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

# Calculate the risk ratio and the corresponding confidence interval.
beta1 <- model_pos$coefficients[2] # according to the slides, beta1 is relative risk
glm.RR(model_pos, 5)

##############
# Question 3 #
##############

# Calculate the ratio of the means of the response for each race (mean response for black/mean response for white).
mu_black <- mean(data$resp[data$race == 'black'])
mu_white <- mean(data$resp[data$race == 'white'])
ratio <- mu_black / mu_white

##############
# Question 4 #
##############

# Calculate the predictions of the models for each race
newdata <- data.frame(race = c('black','white'))
predata <- newdata
predata$resp <- exp(predict(model_pos,newdata))
predata


##############
# Question 5 #
##############

# Analyze the GOF of the model

# Pearson test (Pearson statistic can be used when there are only categorical regressors in the model)
X2 <- sum(residuals(model_pos, type = "pearson")^2)
n <- dim(data)[1]
p <- length(coef(model_pos))
prtest <- data.frame(X2s=X2, pvalue=(1-pchisq(X2,n - p)))
prtest

# Deviance tests
Dev <- summary(model_pos)$deviance
df <- summary(model_pos)$df.residual
dvtest <- data.frame(Dev=Dev, df=df, pvalue=(1-pchisq(Dev,df)))
dvtest

# Likelihood ratio test
lrtest <- Anova(model_pos, test = 'LR',type = 3)
lrtest

# Wald test
wdtest <- Anova(model_pos, test = 'Wald', type = 3)
wdtest

# Graphic analysis
sim_model_pos <- simulateResiduals(model.pos, plot = T) # residual qq plot
hist(sim_model_pos)
root.pos <- rootogram(model_pos,ylab='Root Square of Frequency',main='Poisson')

#  ## GOF for uniformity of the residuals and overdispersion
testUniformity(sim_model_pos)
testDispersion(sim_model_pos)

##############
# Question 6 #
##############

# Fit a negative binomial model and get estimated model based variances (per race) for the counts.
model_nb <- glm.nb(resp ~ race, data = data)
sum_nb <- summary(model_nb)
sum_nb

# Compare them with the observed variances.
# obs
obs_var_b <- var(data[which(data$race == 'black'),1])
obs_var_w <- var(data[which(data$race == 'white'),1])
# model
beta0 <- model_nb$coefficients[1]
beta1 <- model_nb$coefficients[2]
theta <- model_nb$theta
md_var_b <- exp(beta0) + (1/theta)*exp(beta0)^2
md_var_w <- exp(beta0 + beta1) + (1/theta)*exp(beta0 + beta1)^2

comparison <- round(cbind(rbind(obs_var_b,obs_var_w),rbind(md_var_b,md_var_w),rbind(mu_black,mu_white)), 3)
colnames(comparison) <- c("observed","neg.bin_model_based","poisson")
comparison

##############
# Question 7 #
##############

# Fit a Quasi-likelihood model.
model_qs <- glm(resp ~ race, data = data, family = quasipoisson)
sum_qs <- summary(model_qs)
sum_qs
sqrt(sum_qs$dispersion) - 1 # standard error

# scaled deviance
D_star <- model_qs$deviance / sum_qs$dispersion
D_star
##############
# Discussion #
##############

