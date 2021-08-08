#This script analyzes the daily Iran data using a series of Poisson exponentially weighted moving average (PEWMA) models. 
#These models are designed for count time series dependent variables that exhibit persistence.
#The code for the PEWMA models is contained in pests.r, whch is sourced in further below.

#clear memory 
rm( list=ls() ) 

#set working directory
setwd("C:/Users/Ben Bagozzi/Dropbox/Iran Analysis/Analysis")

#load relevant packages
library(MASS)
library(astsa)
library(prais)

#source in code for PEWMA models
source("pests.r")

#set random number seed
set.seed(10)

#Read-in combined daily Iran data
irandata <- read.csv(file = "IrnCombined.csv")
attach(irandata)

#Look first at ACF for Iran series
par(mfrow=c(1,2))
acftmp <- counts.acf(irandata$tweetcount)
tsplot(irandata$tweetcount, xaxt="n",ylab="Iran Misinfo Tweets",xlab="Daily Observations")
axis(1, at=c(0, 92, 214, 336),labels=c("01/2020", "04/2020", "8/2020", "12/2020"))

###########################################################################
############PEWMA Models with Cases as the Independent Variable############
###########################################################################

#Using 'cases' as the independent variable, baseline and then expanded
pewma.1 <- Pewma(tweetcount~-1+new_cases)
pewma.2 <- Pewma(tweetcount~-1+new_cases+irneventpr+price+covidtrend+irantrend)
pewma.3 <- Pewma(tweetcount~-1+new_cases+irneventpr+price+covidtrend+irantrend+globalnew_cases+iranprotest+iranmatconf) ###Won't Converge

#Using 'logged cases' as the independent variable, baseline and then expanded
pewma.4 <- Pewma(tweetcount~-1+log(new_cases+1))
pewma.5 <- Pewma(tweetcount~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend)
pewma.6 <- Pewma(tweetcount~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1))

############################################################################
############PEWMA Models with Deaths as the Independent Variable############
############################################################################

#Using 'deaths' as the independent variable, baseline and then expanded
pewma.7 <- Pewma(tweetcount~-1+new_deaths)
pewma.8 <- Pewma(tweetcount~-1+new_deaths+irneventpr+price+covidtrend+irantrend)
pewma.9 <- Pewma(tweetcount~-1+new_deaths+irneventpr+price+covidtrend+irantrend+global_new_deaths+iranprotest+iranmatconf)

#Using 'logged deaths' as the independent variable, baseline and then expanded
pewma.10 <- Pewma(tweetcount~-1+log(new_deaths+1),omega.init=.02)
pewma.11 <- Pewma(tweetcount~-1+log(new_deaths+1)+irneventpr+price+covidtrend+irantrend)
pewma.12 <- Pewma(tweetcount~-1+log(new_deaths+1)+irneventpr+price+covidtrend+irantrend+log(global_new_deaths+1)+log(iranprotest+1)+log(iranmatconf+1))

######################################################################################
############PEWMA Models with Proportion Cases as the Independent Variable############
######################################################################################

#Using 'proportion of global cases' as the independent variable, baseline and then expanded
pewma.13 <- Pewma(tweetcount~-1+newcasepr)
pewma.14 <- Pewma(tweetcount~-1+newcasepr+irneventpr+price+covidtrend+irantrend)
pewma.15 <- Pewma(tweetcount~-1+newcaseprs+irneventpr+price+covidtrend+irantrend+iranprotest+iranmatconf)

###################################################################################################
############PEWMA Models with Cases as the Independent Variable, Excluding Farsi Tweets############
###################################################################################################

#Using 'cases' as the independent variable, baseline and then expanded
pewma.16 <- Pewma(tweetsnofarsi~-1+new_cases)
pewma.17 <- Pewma(tweetsnofarsi~-1+new_cases+irneventpr+price+covidtrend+irantrend)
pewma.18 <- Pewma(tweetsnofarsi~-1+new_cases+irneventpr+price+covidtrend+irantrend+globalnew_cases+iranprotest+iranmatconf) ###Won't Converge

#Using 'logged cases' as the independent variable, baseline and then expanded
pewma.19 <- Pewma(tweetsnofarsi~-1+log(new_cases+1))
pewma.20 <- Pewma(tweetsnofarsi~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend)
pewma.21 <- Pewma(tweetsnofarsi~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1))

################################################################################################
############PEWMA Models with Cases as the Independent Variable, English-Only Tweets############
################################################################################################

#Using 'cases' as the independent variable, baseline and then expanded
pewma.22 <- Pewma(tweetsenglish~-1+new_cases)
pewma.23 <- Pewma(tweetsenglish~-1+new_cases+irneventpr+price+covidtrend+irantrend)
pewma.24 <- Pewma(tweetsenglish~-1+new_cases+irneventpr+price+covidtrend+irantrend+globalnew_cases+iranprotest+iranmatconf) ###Won't Converge

#Using 'logged cases' as the independent variable, baseline and then expanded
pewma.25 <- Pewma(tweetsenglish~-1+log(new_cases+1))
pewma.26 <- Pewma(tweetsenglish~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend)
pewma.27 <- Pewma(tweetsenglish~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1))

#######################################################################################
############PEWMA Models with Cases as the Independent Variable, with Drift############
#######################################################################################

#Using 'cases' as the independent variable, baseline and then expanded
pewma.28 <- Pewma(tweetcount~new_cases)
pewma.29 <- Pewma(tweetcount~new_cases+irneventpr+price+covidtrend+irantrend)
pewma.30 <- Pewma(tweetcount~new_cases+irneventpr+price+covidtrend+irantrend+globalnew_cases+iranprotest+iranmatconf) ###Won't Converge

#Using 'logged cases' as the independent variable, baseline and then expanded
pewma.31 <- Pewma(tweetcount~log(new_cases+1))  #Convergence Problems
pewma.32 <- Pewma(tweetcount~log(new_cases+1)+irneventpr+price+covidtrend+irantrend)
pewma.33 <- Pewma(tweetcount~log(new_cases+1)+irneventpr+price+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1)) ##Convergence problems


#####################################################################################################
############Models with Cases as the Independent Variable, using Prais Winston Regression############
#####################################################################################################
#Note, unlike the models above, these models assume a linear DV, and an AR(1) process. I don't think this is what we want. I think we want
#ARIMA models for analyses of these DVs under a linear model framework.

#Using 'cases' as the independent variable, baseline and then expanded
pw.1 <- prais_winsten(tweetcount~-1+new_cases,data=irandata)
summary(pw.1)
pw.2 <- prais_winsten(tweetcount~-1+new_cases+irneventpr+price+covidtrend+irantrend,data=irandata)
summary(pw.2)
pw.3 <- prais_winsten(tweetcount~-1+new_cases+irneventpr+price+covidtrend+irantrend+globalnew_cases+iranprotest+iranmatconf,data=irandata) ###Won't Converge
summary(pw.3)

#Using 'logged cases' as the independent variable, baseline and then expanded
pw.4 <- prais_winsten(tweetcount~-1+log(new_cases+1),data=irandata)
summary(pw.4)
pw.5<- prais_winsten(tweetcount~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend,data=irandata)
summary(pw.5)
pw.6 <- prais_winsten(tweetcount~-1+log(new_cases+1)+irneventpr+price+covidtrend+irantrend+log(globalnew_cases+1)+log(iranprotest+1)+log(iranmatconf+1),data=irandata)
summary(pw.6)
