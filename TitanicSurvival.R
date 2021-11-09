---
  title: "Titanic Survival"
author: "William Duquette"
date: "Due on Sunday, May 10 at 11:59PM"
output:
  html_document:
  df_print: paged
pdf_document: default
word_document: default
---
require(car)
require(ggplot2)
require(tidyverse)
require(EnvStats)
require(MASS)
library(MuMIn)
require(gridExtra)
require(Hmisc)
library(expss)
library(ResourceSelection)
library(pscl)
library(naniar)
library(tidyr)

#Read in and clean data
Titanic<-read.csv("Titanic.csv")
Titanic$Embarked=factor(Titanic$Embarked, levels=c("C","Q","S"), labels=c("Cherbourg", "Queenstown", "Southampton"))
Titanic$Pclass=factor(Titanic$Pclass, levels=c("1","2","3"), labels=c("Upper","Middle", "Lower"))
Titanic$Pclass=as.factor(Titanic$Pclass)
Titanic$sibsp=as.numeric(Titanic$sibsp)
Titanic$parch=as.numeric(Titanic$parch)
Titanic2=Titanic
Titanic2$Fare=log(Titanic$Fare+1)

#Variable Analysis
Pclassbar<-ggplot(Titanic, aes(x=Pclass, fill=Pclass))+geom_bar()+labs(title="Distribution of Pclass", x="Class of Passenger")

Pclassbar+guides(fill=guide_legend(title="Ticket Class"))

ggplot(Titanic, aes(x=Sex, fill=Sex))+geom_bar()+labs(title="Distribution of Sex", x="Sex of Passenger")

Agehist<-ggplot(Titanic, aes(x=Age, fill=Sex))+geom_histogram()+labs(title="Distribution of Age", x="Age of Passenger")
Agehist+facet_grid(~Sex)

sibsphist<-ggplot(Titanic, aes(x=sibsp, fill=Sex))+geom_histogram()+labs(title="Distribution of sibsp", x="Number of Siblings or Spouses Passenger had Aboard")
sibsphist+facet_grid(~Sex)

ggplot(Titanic, aes(x=sibsp, y=Age, col=Sex))+geom_point()

parchhist<-ggplot(Titanic, aes(x=parch, fill=Sex))+geom_histogram()+labs(title="Distribution of parch", x="Number of Parents or Children passenger had Aboard")
parchhist+facet_grid(~Sex)

ggplot(Titanic, aes(x=Fare))+geom_histogram(fill="deepskyblue")+labs(title="Distribution of Fare", x="Cost of Fare")

Embarkedbar<-ggplot(NULL, aes(x=na.omit(Titanic$Embarked), fill=na.omit(Titanic$Embarked)))+geom_bar()+labs(title="Distribution of Embarked", x="The Location Passenger Embarked From")

Embarkedbar+guides(fill=guide_legend(title="Embarked"))


#Exploratory Data Analysis
Titanictemp=subset(Titanic, is.na(Embarked)==FALSE)

ggplot(Titanic, aes(x=Survived))+geom_bar(fill="deepskyblue")+labs(title = "Distribution of response variable: Survived")

survivedbar1<-ggplot(Titanic, aes(x=Survived, fill=Pclass))+geom_bar()+labs(title = "Survived (Separated by Class)")
survivedbar1+facet_grid(~Pclass)+guides(fill=guide_legend(title="Ticket Class"))

survivedbar2<-ggplot(Titanic, aes(x=Survived, fill=Sex))+geom_bar()+labs(title = "Survived (Separated by Sex)")
survivedbar2+facet_grid(~Sex)

ggplot(Titanic, aes(y=Age, x=Survived, fill=Survived))+geom_boxplot()+labs(title = "Survived vs. Age")+coord_flip()

ggplot(Titanic, aes(y=sibsp, x=Survived, fill=Survived))+geom_boxplot()+labs(title= "Survied vs. sibsp")+coord_flip()

ggplot(Titanic, aes(y=parch, x=Survived, fill=Survived))+geom_boxplot()+labs(title="Survived vs. parch")+coord_flip()

ggplot(Titanic, aes(y=Fare, x=Survived, fill=Survived))+geom_boxplot()+labs(title="Survived vs. Fare")+coord_flip()

survivedbar3<-ggplot(Titanictemp, aes(x=Survived, fill=Embarked))+geom_bar()+labs(title = "Survived (Separated by Embarked)")
survivedbar3+facet_grid(~Embarked)+guides(fill=guide_legend(title="Embarked"))

# Checking Fare
Before<-ggplot(Titanic, aes(y=Fare, x=Survived, fill=Survived))+geom_boxplot()+labs(title="Before", y="Cost of Fare")+coord_flip()
After<-ggplot(Titanic2, aes(y=Fare, x=Survived, fill=Survived))+geom_boxplot()+labs(title="After", y="Cost of Fare")+coord_flip()
grid.arrange(Before, After, ncol=2)

# First Interaction Term
mod.int.1<-glm(Survived~Pclass*Sex, data=Titanic2, family=binomial)
summary(mod.int.1)

# Second Interaction Term
ggplot(Titanic2, aes(y=Age, x=Survived, fill=Sex))+geom_boxplot()+coord_flip()
mod.int.2<-glm(Survived~Sex*Age, data=Titanic, family=binomial)
summary(mod.int.2)

# Third interaction term
ggplot(Titanic2, aes(y=parch, x=Survived, fill=Sex, col=Sex))+geom_boxplot()+coord_flip()
mod.int.3<-glm(Survived~parch*Sex, data=Titanic2, family=binomial)
summary(mod.int.3)

mod.full<-glm(Survived~.-PassengerID+Sex*Age+Pclass*Sex+Sex*parch, data=na.omit(Titanic2), family=binomial)
mod.back<-step(mod.full, direction="backward", trace=0)
summary(mod.back)

vif(mod.back)

#Make final model
mod.final<-glm(Survived~Pclass+Sex+Age+sibsp+parch+Pclass*Sex+Sex*parch, data=Titanic2, family=binomial)
summary(mod.final)


#Check for Linearity with Log-Odds
probabilities = predict(mod.final, type = "response") # Change the model name from log1 to your model!
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")

vars = c("Pclass","Sex","Age", "sibsp", "Fare") # Change these variables to your name!
testdata = na.omit(Titanic2[vars]) #change birds to your dataset name! 

# Select only numeric predictors
data_num <- testdata %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(data_num)

# Bind the logit and tidying the data for plot
data_num <- data_num %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# Make the Scatter plots

ggplot(data_num, aes(logit, predictor.value))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

