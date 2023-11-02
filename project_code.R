#####Project Code#####

library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)

#read in data
chs <- read_sas("chs2012_public.sas7bdat.html")

##data cleaning

#subsetting only needed variables
myvars <- c("currentasthma12", "everasthma", "anymold", "anymice", "seenmice30days",
            "anyroach", "seenroach30days", "shshome12", "shsmokerinhome", 
            "agegroup5", "sex", "newrace", "environ3", "environ2", "environ1",
            "povertygroup", "weightall", "smoker", "smokecat", "nspd", 
            "education", "employment12", "emp3", "neighpovgroup4_0711")
df <- chs[myvars]

#exclude observation with missing outcome
df <- df %>% drop_na(currentasthma12)

#restrict data to only those who ever had asthma
df <- df[df$everasthma == 1, ]

#handling missing data
df <- na.omit(df)

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

#summary
summary(df)

#table1
table1(~ factor(sex) + factor(agegroup5) + factor(newrace) | currentasthma12, data = df)

##regression models
#full model
full_model <- glm(currentasthma12 ~ anymice + anyroach + anymold + shshome12 + 
                    as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
                    as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                    sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(full_model)
#reduced model
reduced_model <- glm(currentasthma12 ~ anymice + anyroach + anymold + shshome12 + 
                       as.factor(weightall) + sex + as.factor(agegroup5) + nspd, 
                     data = df, family = binomial())
summary(reduced_model)
#full model with interaction terms for the exposures
full_model2 <- glm(currentasthma12 ~ anymice + anyroach + anymold + shshome12 + 
                    as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
                    as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                    sex + as.factor(agegroup5) + nspd + anymice*anyroach + anymice*anymold + 
                    anymice*shshome12 + anyroach*anymold + anyroach*shshome12 + 
                    anymold*shshome12, data = df, family = binomial())
summary(full_model2)
#separate model for each exposure
mice <- glm(currentasthma12 ~ anymice +  
                     as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
                     as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                     sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(mice)
roach <- glm(currentasthma12 ~ anyroach +  
              as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(roach)
mold <- glm(currentasthma12 ~ anymold +  
              as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(mold)
shs <- glm(currentasthma12 ~ shshome12 +  
              as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(shs)

##covariate selection procedures
glm1 <- glm(as.factor(currentasthma12) ~ as.factor(weightall) + as.factor(neighpovgroup4_0711) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
#forward selection
stepModel1 <- step(glm1, direction = c("forward"), trace = 0)
summary(stepModel1)
#backward selection
stepModel2 <- step(glm1, direction = c("backward"), trace = 0)
summary(stepModel2)
#stepwise
stepModel3 <- step(glm1, direction = c("both"), trace = 0)
summary(stepModel3)

##models for exposure based on selection procedures
mice2 <- glm(as.factor(currentasthma12) ~ anymice +  
              as.factor(weightall) + 
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(mice2)
roach2 <- glm(as.factor(currentasthma12) ~ anyroach +  
                as.factor(weightall) + 
                sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(roach2)
mold2 <- glm(as.factor(currentasthma12) ~ anymold +  
               as.factor(weightall) + 
               sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(mold2)
shs2 <- glm(as.factor(currentasthma12) ~ shshome12 +  
              as.factor(weightall) + 
              sex + as.factor(agegroup5) + nspd, data = df, family = binomial())
summary(shs2)