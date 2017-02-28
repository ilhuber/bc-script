library(tidyverse)
library(caret)
#################################
dataf<-read.csv("../MRT data.csv")
dataf$ID<-1:nrow(dataf)
dataf<-dataf%>%dplyr::select(ID,SoilpH,Clay,Sand,Corg,Biomass,Temp,K1,K2)
### Partitioning
set.seed(998)
inTraining <- createDataPartition(dataf$ID, p = .75, list = FALSE)
training <- dataf[ inTraining,]
testing  <- dataf[-inTraining,]

##### Training options
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  allowParallel=T)  ## repeated ten times
#############################
############ Here I will try different models and then 
############ I will compare my models 
#######################################################
#1#traning glm with identity link function - log transform
log.glmiden <- train(log(K2) ~ . -ID-K1, data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl,
                 verbose = FALSE,
                 na.action = na.exclude,
                 family=gaussian(link =identity))

#2#traning glm with identity link function - sqrt transform
sqrt.glmiden <- train(sqrt(K2) ~ . -ID-K1, data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl,
                 verbose = FALSE,
                 na.action = na.exclude,
                 family=gaussian(link =identity))

#3#traning glm with identity link function
glmiden <- train(K2 ~ . -ID-K1, data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl,
                 verbose = FALSE,
                 na.action = na.exclude,
                 family=gaussian(link =identity))


#4#traning glm with identity link function and second order 
glmiden2 <- train(K2 ~ .^2 -ID-K1, data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl,
                 verbose = FALSE,
                 na.action = na.exclude,
                 family=gaussian(link =identity))

#5#traning glm with log link function 
glmlog <- train(K2 ~ . -ID-K1, data = training, 
                 method = "glmStepAIC", 
                 trControl = fitControl,
                 verbose = FALSE,
                 na.action = na.exclude,
                 family=gaussian(link =log))


##########################################
################### Diagnostics-comparison
###########################################
resamps <- resamples(list(GLMI = glmiden,
                          GLMI2 = glmiden2,
                          GLML = glmlog,
                          LOG.G=log.glmiden,
                          SQGLMI=sqrt.glmiden))


trellis.par.set(caretTheme())
bwplot(resamps, layout = c(2, 1),xlim=c(0,3.2))

#1VS1
plot(testing$K2[!is.na(testing$SoilpH)],
     predict(glmiden, newdata = testing[!is.na(testing$SoilpH),]),
     xlim=c(0,0.005),ylim=c(0,0.005),pch=19,cex=2,xlab="Measured",ylab="Simulated")

abline(a=0,b=1,lwd=3)

