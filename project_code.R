#####Project Code#####

library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(ResourceSelection)
library(regclass)

#read in data
chs <- read_sas("chs2012_public.sas7bdat.html")

##data cleaning

#subsetting only needed variables
myvars <- c("currentasthma12", "everasthma", "anymold", "anymice", "seenmice30days",
            "anyroach", "seenroach30days", "shshome12", "shsmokerinhome", 
            "agegroup5", "sex", "newrace", "environ3", "environ2", "environ1",
            "weightall", "smoker", "smokecat", "nspd", 
            "education", "emp3")
df <- chs[myvars]

#exclude observation with missing outcome
df <- df %>% drop_na(currentasthma12)

#restrict data to only those who ever had asthma
df <- df[df$everasthma == 1, ]

#handling missing data
df <- na.omit(df)

#recode binary variables
df$female <- df$sex-1
df$currentasthma12 <- ifelse(df$currentasthma12 == 2, 0, 1)
df$nspd <- ifelse(df$nspd == 2, 0, 1)
df$shshome12 <- ifelse(df$shshome12 == 2, 0, 1)
df$anymold <- ifelse(df$mold == 2, 0, 1)

##data exploration
#outcome
hist(df$currentasthma12)
#mice
boxplot(df$environ2 ~ df$currentasthma12)
hist(df$environ2)
hist(df$seenmice30days)
hist(df$anymice)
#roach
boxplot(df$environ1 ~ df$currentasthma12)
hist(df$environ1)
hist(df$seenroach30days)
hist(df$anyroach)
#mold
boxplot(df$environ3 ~ df$currentasthma12)
hist(df$environ3)
hist(df$anymold)
#second hand smoke
boxplot(df$shshome12 ~ df$currentasthma12)
hist(df$shshome12)
#bmi
boxplot(df$weightall ~ df$currentasthma12)
hist(df$weightall)
#neighborhood poverty
boxplot(df$neighpovgroup4_0711 ~ df$currentasthma12)
hist(df$neighpovgroup4_0711)
#smoker
boxplot(df$smoker ~ df$currentasthma12)
hist(df$smoker)
boxplot(df$smokecat ~ df$currentasthma12)
hist(df$smokecat)
#education
boxplot(df$education ~ df$currentasthma12)
hist(df$education)
#newrace
boxplot(df$newrace ~ df$currentasthma12)
hist(df$newrace)
#sex
boxplot(df$sex ~ df$currentasthma12)
hist(df$sex)
#age
boxplot(df$agegroup5 ~ df$currentasthma12)
hist(df$agegroup5)
#nspd
boxplot(df$nspd ~ df$currentasthma12)
hist(df$nspd)
#employment
boxplot(df$employment12 ~ df$currentasthma12)
hist(df$employment12)
boxplot(df$emp3 ~ df$currentasthma12)
hist(df$emp3)

#summary
summary(df)

#table1
table1(~ factor(sex) + factor(agegroup5) + factor(newrace) | currentasthma12, data = df)

##regression models
#crude exposure models
mice <- glm(currentasthma12 ~ environ2, data = df, family = binomial())
summary(mice)
roach <- glm(currentasthma12 ~ seenroach30days, data = df, family = binomial())
summary(roach)
mold <- glm(currentasthma12 ~ anymold, data = df, family = binomial())
summary(mold)
shs <- glm(currentasthma12 ~ shshome12, data = df, family = binomial())
summary(shs)

#full model
mice2 <- glm(currentasthma12 ~ environ2 + as.factor(weightall) + 
                    as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                    sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mice2)
roach2 <- glm(currentasthma12 ~ seenroach30days + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(roach2)
mold2 <- glm(currentasthma12 ~ anymold + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mold2)
shs2 <- glm(currentasthma12 ~ shshome12 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(shs2)

#check for em
mice3 <- glm(currentasthma12 ~ environ2 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + environ2*as.factor(newrace) +
               environ2*as.factor(emp3) + environ2*as.factor(agegroup5) + environ2*sex + environ2*as.factor(smokecat) + 
               environ2*as.factor(weightall), data = df, family = binomial())
summary(mice3)
roach3 <- glm(currentasthma12 ~ seenroach30days + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + seenroach30days*as.factor(newrace) +
               seenroach30days*as.factor(emp3) + seenroach30days*as.factor(agegroup5) + seenroach30days*sex 
              + seenroach30days*as.factor(smokecat) + 
               seenroach30days*as.factor(weightall), data = df, family = binomial())
summary(roach3) #sex is an effect modifer
mold3 <- glm(currentasthma12 ~ anymold + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + anymold*as.factor(newrace) +
               anymold*as.factor(emp3) + anymold*as.factor(agegroup5) + anymold*sex + anymold*as.factor(smokecat) + 
               anymold*as.factor(weightall), data = df, family = binomial())
summary(mold3)
shs3 <- glm(currentasthma12 ~ shshome12 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + shshome12*as.factor(newrace) +
               shshome12*as.factor(emp3) + shshome12*as.factor(agegroup5) + shshome12*sex + shshome12*as.factor(smokecat) + 
               shshome12*as.factor(weightall), data = df, family = binomial())
summary(shs3)

##covariate selection procedures
glm1 <- glm(currentasthma12 ~ as.factor(weightall) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
#forward selection
stepModel1 <- step(glm1, direction = c("forward"), trace = 0)
summary(stepModel1)
#backward selection
stepModel2 <- step(glm1, direction = c("backward"), trace = 0)
summary(stepModel2)
#stepwise
stepModel3 <- step(glm1, direction = c("both"), trace = 0)
summary(stepModel3) #stepwise indicated that weightall, sex, agegroup5, nspd,
#and emp3 are best predictors

##models for exposure based on selection procedures
mice4 <- glm(currentasthma12 ~ environ2 +  
              as.factor(weightall) + 
              sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mice4) #best model for mice
roach4 <- glm(currentasthma12 ~ seenroach30days +  
                as.factor(weightall) + 
                sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(roach4) #best model for roach
mold4 <- glm(currentasthma12 ~ anymold +  
               as.factor(weightall) + 
               sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mold4) #best model for mold
shs4 <- glm(currentasthma12 ~ shshome12 +  
              as.factor(weightall) + 
              sex + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(shs4) #best model for shshome12

##include interaction term for sex in roach mod
roach5 <- glm(currentasthma12 ~ seenroach30days +  
                as.factor(weightall) + 
                sex + as.factor(agegroup5) + nspd + as.factor(emp3) + 
                seenroach30days*sex, data = df, family = binomial())
summary(roach5)

##assess multicolinearity
VIF(mice4)
VIF(roach5)
VIF(mold4)
VIF(shs4)

##model evaluation
#calibration
hoslem.test(mice4$y, fitted(mice4), g=12)
hoslem.test(roach5$y, fitted(roach5), g=13)
hoslem.test(mold4$y, fitted(mold4), g=12)
hoslem.test(shs4$y, fitted(shs4), g=12)















