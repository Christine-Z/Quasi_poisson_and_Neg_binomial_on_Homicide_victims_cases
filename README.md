# Homicide victims analysis by Quasi poisson and Negative binomial models
 
### Libraries: 
dplyr,ggplot2, MASS(negative binomial model), car, DHARMa(Dispersion test)

### DIY Functions:
glm.RR, testtools

## Dataset
1308 observation of homicide victims categoried by race(white/black)
**resp:** numbers of victims observed
**race:** black/white

## Model Fitting
### 1.Poisson Model
resp = exp( -0.6501 - 1.7331 ? race)
note: raceblack=0, racewhite=1
AIC: 1122

![image](https://user-images.githubusercontent.com/72392376/123554851-624f0c00-d782-11eb-9f59-239f9c52afa9.png)
![image](https://user-images.githubusercontent.com/72392376/123554864-75fa7280-d782-11eb-9d62-69b03a66a4d2.png)
![image](https://user-images.githubusercontent.com/72392376/123554874-84e12500-d782-11eb-8605-4695b351a75d.png)

Poisson model has an over-dispersion problem

### 2.Negative binomial Model
Theta:  0.2023
AIC: 1001.8

![image](https://user-images.githubusercontent.com/72392376/123554896-a7733e00-d782-11eb-84c7-1d7928daa3e3.png)

Negative binomial model solved the over-disperson problem

### 3.Quasi-poisson model

Dispersion parameter for quasi-poisson family taken to be 1.745694,which indicates the variance is 74.6% larger than the mean. 
Over-dispersion did not get solved.

## Goodness of fit test

| Models  | Poisson | Neg-Bin | Quasi-P |
| ------- | ------- | ------- | ------- |
| Pearson test  | Chisq=2279.87,p=0  | Chisq=1424.03,p=0.01  | Chisq=2279.87,p=0  |
| Deviance test  | Deviance=844.71,p=1  | Deviance=412.60,p=1  | Deviance=844.71,p=1  |
| LR test  | Chisq=118.09,p=0  | Chisq=58.98,p=0  | Chisq=67.65,p=0  |
| Wald test  | Chisq=139.83,p=0  | Chisq=52.82,p=0  | Chisq=80.10,p=0  |

Negative binomial model have the least deviance(412.6). 
Both negative binomial model and quasi-poisson model perform better than the classical poisson model.

## Risk Ratio and the ratio of mean response in poisson model

|     | RR  | 2.5% | 97.5 % |
| --- | --- | ---- | -------|
| (Intercept)  | 0.52  | 0.42 | 0.64 |
| race  | 0.18  | 0.13 | 0.24 |

mean response(race:black) = 0.522
mean response(race:white) = 0.092
Ratio = 0.522/0.092 = 5.66 = 1/RR = 1/0.18
Average number of victims known is 5.66 as many for black people as for white people

