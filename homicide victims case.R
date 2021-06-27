################
Set Environment
################

library(dplyr)
library(MASS)
library(car)
library(DHARMa)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))#set working dictionary in current folder

############
DIY Function
############

### risk ratio ###
glm.RR <- function(model, digits = 2) {
  
  if (model$family$family == "binomial") {
    LABEL <- "OR"
  } else if (model$family$family == "poisson") {
    LABEL <- "RR"
  } else {
    stop("Not logistic or Poisson model")
  }
  
  COEF      <- stats::coef(model)
  CONFINT   <- stats::confint(model)
  TABLE     <- cbind(coef=COEF, CONFINT)
  TABLE.EXP <- round(exp(TABLE), digits)
  
  colnames(TABLE.EXP)[1] <- LABEL
  
  TABLE.EXP
}

### goodness of fit test ###
testtools <- function(model,data) {
  # Pearson test
  X2 <- sum(residuals(model, type = "pearson")^2)
  n1 <- dim(data)[1]
  p1 <- length (coef(model)) 
  pearson <- data.frame(X2s = X2,pvalue = (1-pchisq(X2,n1 - p1)))
  
  # Deviance test
  Dev <- summary(model)$deviance
  df <- summary(model)$df.residual
  deviance <- data.frame(Dev = Dev, df = df, pvalue = pchisq(Dev,df,lower.tail = FALSE))
  
  # Likelihood ratio test
  lr <- Anova(model, test = "LR",type=3)
  
  # Wald test
  wt <- Anova(model, test = "Wald",type=3)
  
  result <- list( 'Pearson test' = pearson,
                  'Deviance test' = deviance,
                  'Likelihood ratio test' = lr,
                  'Wald test' = wt)
  return(result)
}
  
##############################
Import and pre-process dataset
##############################

raw_data <- read.delim("16-victim.txt",header = TRUE)

pro_data<- raw_data
pro_data['race'] <- ifelse(pro_data$race=='black',0,1)

rm(raw_data)

##############
Model Fitting
##############

### poisson model ###
md_ps <- glm(resp ~ race, family = poisson(link = "log"), data = pro_data)
sum_ps <- summary(md_ps)

# Dispersion test #
disp_ps <- testDispersion(md_ps)#over-dispersion problem

# risk ratio of poisson model #
RR_ps <- glm.RR(md_ps)
#               RR      2.5%  97.5 %
#  (Intercept)  0.52    0.42  0.64
#   race        0.18    0.13  0.24

# ratio of mean of resp for each race #
mu_black <- mean(pro_data$resp[pro_data$race == 0])
mu_white <- mean(pro_data$resp[pro_data$race == 1])
r_mean <- mu_black/mu_white
# Ratio = 0.522/0.092 = 5.66 = 1/RR = 1/0.18
# average # of victims known is 5.66 as many for black people as for white people,
# similar to what we see from the RR (1/0.18) = 5.66

### negative binominal model ###
md_nb <- glm.nb(resp ~ race, data = pro_data)
sum_nb <- summary(md_nb)

# Residuals #
rsd_nb<- simulateResiduals(md_nb,plot = T)
unif_nb <- hist(rsd_nb)

# Dispersion test #
disp_nb <- testDispersion(md_nb)#no over-dispersion

### quasi poisson model ###
md_qs <- glm(resp ~ race, family=quasipoisson, data=pro_data)
sum_qs <- summary(md_qs)
# variance is 74.6% larger than the mean

###############
Goodness of fit
###############

### poisson model ###
gof_ps <- testtools(md_ps,pro_data)
### negative binominal model ###
gof_nb <- testtools(md_nb,pro_data)
### quasi poisson model ###
gof_qs <- testtools(md_qs,pro_data)
