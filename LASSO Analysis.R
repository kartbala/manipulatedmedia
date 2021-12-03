#This script analyzes the daily Iran data an implementation of the induced smoothing (IS) idea to lasso regularization models to allow estimation and inference on the model coefficients
#Linear and Poisson models are considered, when including/excluding the COVID deaths variables.

#clear memory 
rm( list=ls() ) 

#set working directory
setwd("C:/Users/Benjamin Bagozzi/Dropbox/Iran Analysis/Analysis")

#load relevant packages
library(MASS)
library(islasso)

#set random number seed
set.seed(10)

#Read-in combined daily Iran data
irandata <- read.csv(file = "IrnCombined.csv")

#Lasso with Poisson
out <- islasso(tweetcount ~ log(new_cases+1)+irneventpr+open+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1)+log(new_deaths+1)+log(global_new_deaths+1), data = irandata, family=poisson)
summary(out)
summary(out, pval = 0.1)

#Lasso with Gaussian
out <- islasso(tweetcount ~ log(new_cases+1)+irneventpr+open+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1)+log(new_deaths+1)+log(global_new_deaths+1), data = irandata, family=gaussian)
summary(out)
summary(out, pval = 0.1)

#Lasso with Poisson (excluding deaths)
out <- islasso(tweetcount ~ log(new_cases+1)+irneventpr+open+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1), data = irandata, family=poisson)
summary(out)
summary(out, pval = 0.1)

#Lasso with Gaussian (excluding deaths)
out <- islasso(tweetcount ~ log(new_cases+1)+irneventpr+open+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1), data = irandata, family=gaussian)
summary(out)
summary(out, pval = 0.1)
