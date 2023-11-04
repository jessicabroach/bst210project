#####Project Code#####

library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(ggplot2)
library(broom)
library(ResourceSelection)
library(regclass)
library(glmnet)
library(vip)

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
df$anymold <- ifelse(df$anymold == 2, 0, 1)

##data exploration
#outcome
hist(df$currentasthma12)
#mice
boxplot(df$environ2 ~ df$currentasthma12)
hist(df$environ2)
hist(log(df$environ2))
hist(df$seenmice30days)
hist(df$anymice)
#roach
boxplot(df$environ1 ~ df$currentasthma12)
hist(df$environ1)
hist(log(df$environ1))
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
roach <- glm(currentasthma12 ~ environ1, data = df, family = binomial())
summary(roach)
mold <- glm(currentasthma12 ~ anymold, data = df, family = binomial())
summary(mold)
shs <- glm(currentasthma12 ~ shshome12, data = df, family = binomial())
summary(shs)

#full model
mice2 <- glm(currentasthma12 ~ environ2 + as.factor(weightall) + 
                    as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                    female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mice2)
roach2 <- glm(currentasthma12 ~ environ1 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(roach2)
mold2 <- glm(currentasthma12 ~ anymold + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mold2)
shs2 <- glm(currentasthma12 ~ shshome12 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(shs2)

#check for confounding
#mice
cmice <- glm(currentasthma12 ~ environ2 + as.factor(smokecat), data = df, family = binomial())
summary(cmice)
100 * ((0.023415 - 0.023936)/0.023936)
cmice2 <- glm(currentasthma12 ~ environ2 + as.factor(newrace), data = df, family = binomial())
summary(cmice2)
100 * ((0.023415 - 0.022918)/0.022918)
cmice3 <- glm(currentasthma12 ~ environ2 + as.factor(education), data = df, family = binomial())
summary(cmice3)
100 * ((0.023415 - 0.022312)/0.022312)
cmice4 <- glm(currentasthma12 ~ environ2 + as.factor(agegroup5), data = df, family = binomial())
summary(cmice4)
100 * ((0.023415 - 0.021911)/0.021911)
#roach
croach <- glm(currentasthma12 ~ environ1 + as.factor(smokecat), data = df, family = binomial())
summary(croach)
100 * ((0.010878 - 0.011372)/0.011372)
croach2 <- glm(currentasthma12 ~ environ1 + as.factor(newrace), data = df, family = binomial())
summary(croach2)
100 * ((0.010878 - 0.009642)/0.009642) #confounder
croach3 <- glm(currentasthma12 ~ environ1 + as.factor(education), data = df, family = binomial())
summary(croach3)
100 * ((0.010878 - 0.009907)/0.009907)
croach4 <- glm(currentasthma12 ~ environ1 + as.factor(agegroup5), data = df, family = binomial())
summary(croach4)
100 * ((0.010878 - 0.009834)/0.009834) #confounder
#mold
cmold <- glm(currentasthma12 ~ anymold + as.factor(smokecat), data = df, family = binomial())
summary(cmold)
100 * ((0.3617 - 0.3581)/0.3581)
cmold2 <- glm(currentasthma12 ~ anymold + as.factor(newrace), data = df, family = binomial())
summary(cmold2)
100 * ((0.3617 - 0.3467)/0.3467)
cmold3 <- glm(currentasthma12 ~ anymold + as.factor(education), data = df, family = binomial())
summary(cmold3)
100 * ((0.3617 - 0.34762)/0.34762)
cmold4 <- glm(currentasthma12 ~ anymold + as.factor(agegroup5), data = df, family = binomial())
summary(cmold4)
100 * ((0.3617 - 0.3539)/0.3539)
#second hand
cshs <- glm(currentasthma12 ~ shshome12 + as.factor(smokecat), data = df, family = binomial())
summary(cshs)
100 * ((0.54003 - 0.4433)/0.4433) #confounder
cshs2 <- glm(currentasthma12 ~ shshome12 + as.factor(newrace), data = df, family = binomial())
summary(cshs2)
100 * ((0.54003 - 0.51760)/0.51760)
cshs3 <- glm(currentasthma12 ~ shshome12 + as.factor(education), data = df, family = binomial())
summary(cshs3)
100 * ((0.54003 - 0.53304)/0.53304)
cshs4 <- glm(currentasthma12 ~ shshome12 + as.factor(agegroup5), data = df, family = binomial())
summary(cshs4)
100 * ((0.54003 - 0.5318)/0.5318)

#check for em
mice3 <- glm(currentasthma12 ~ environ2 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + environ2*as.factor(newrace) +
               environ2*as.factor(emp3) + environ2*as.factor(agegroup5) + environ2*female + environ2*as.factor(smokecat) + 
               environ2*as.factor(weightall), data = df, family = binomial())
summary(mice3)
roach3 <- glm(currentasthma12 ~ environ1 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + environ1*as.factor(newrace) +
               environ1*as.factor(emp3) + environ1*as.factor(agegroup5) + environ1*female 
              + environ1*as.factor(smokecat) + 
               environ1*as.factor(weightall), data = df, family = binomial())
summary(roach3)
mold3 <- glm(currentasthma12 ~ anymold + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + anymold*as.factor(newrace) +
               anymold*as.factor(emp3) + anymold*as.factor(agegroup5) + anymold*female + anymold*as.factor(smokecat) + 
               anymold*as.factor(weightall), data = df, family = binomial())
summary(mold3)
shs3 <- glm(currentasthma12 ~ shshome12 + as.factor(weightall) + 
               as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
               sex + as.factor(agegroup5) + nspd + as.factor(emp3) + shshome12*as.factor(newrace) +
               shshome12*as.factor(emp3) + shshome12*as.factor(agegroup5) + shshome12*female + shshome12*as.factor(smokecat) + 
               shshome12*as.factor(weightall), data = df, family = binomial())
summary(shs3)

##covariate selection procedures
glm1 <- glm(currentasthma12 ~ as.factor(weightall) + 
              as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
              female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
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
              female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mice4) #best model for mice
roach4 <- glm(currentasthma12 ~ environ1 +  
                as.factor(weightall) + 
                female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(roach4) #best model for roach
mold4 <- glm(currentasthma12 ~ anymold +  
               as.factor(weightall) + 
               female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(mold4) #best model for mold
shs4 <- glm(currentasthma12 ~ shshome12 +  
              as.factor(weightall) + as.factor(smokecat) +
              female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(shs4) #best model for shshome12

##final models and evaluation##
finalmice <- glm(currentasthma12 ~ environ2 +  
                      as.factor(weightall) + 
                      female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(finalmice)
finalroach <- glm(currentasthma12 ~ environ1 + as.factor(weightall) + as.factor(newrace) +
                    female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(finalroach) 
finalmold <- glm(currentasthma12 ~ anymold + as.factor(weightall) + 
                             as.factor(smokecat) + as.factor(education) + as.factor(newrace) +
                             female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(finalmold)
finalshs <- glm(currentasthma12 ~ shshome12 +  
                  as.factor(weightall) + as.factor(smokecat) +
                  female + as.factor(agegroup5) + nspd + as.factor(emp3), data = df, family = binomial())
summary(finalshs)
##assess multicolinearity
VIF(finalmice)
VIF(finalroach)
VIF(finalmold)
VIF(finalshs)

##model evaluation
#calibration
hoslem.test(finalmice$y, fitted(finalmice), g=12)
hoslem.test(finalroach$y, fitted(finalroach), g=15)
hoslem.test(finalmold$y, fitted(finalmold), g=22)
hoslem.test(finalshs$y, fitted(finalshs), g=15)

##odds ratios for final models
#mice
exp(coef(finalmice)[2])
exp(coef(finalmice)[2] + c(-1, 1)*1.96*sqrt(vcov(finalmice)[2,2])) 
#roach
exp(coef(finalroach)[2])
exp(coef(finalroach)[2] + c(-1, 1)*1.96*sqrt(vcov(finalroach)[2,2])) 
#mold
exp(coef(finalmold)[2])
exp(coef(finalmold)[2] + c(-1, 1)*1.96*sqrt(vcov(finalmold)[2,2])) 
#shs
exp(coef(finalshs)[2])
exp(coef(finalshs)[2] + c(-1, 1)*1.96*sqrt(vcov(finalshs)[2,2])) 

##sensitivity analysis using penalized models
# Model select via Penalized regression methods
set.seed(17)
lambda_grid <- .2 ^ (-50:50)

# Prepare X matrix (minus death) for input to glmnet
sens <- df[,c("currentasthma12","agegroup5","newrace","weightall","smokecat","nspd",
                    "education","emp3","female")]
x <- model.matrix(currentasthma12~., data=sens)[,-c(1)]
y <- sens$currentasthma12
names(x)<- c("agegroup5","newrace","weightall","smokecat","nspd",
                    "education","emp3","female")

# Ridge
ridge = glmnet(x,y, alpha=0,family="binomial",
                    lambda=lambda_grid, data=sens)
#print(ridge.fram)
vip(ridge, num_features=8, geom="point", include_type=TRUE)
par(mfrow=c(1,2))
plot(ridge)
cv.ridge <- cv.glmnet(x,y, alpha=0, family="binomial", data=sens)
plot(cv.ridge)
lambda_min <- cv.ridge$lambda.min
lambda_1se <- cv.ridge$lambda.1se
coef(cv.ridge,s=lambda_1se)

# LASSO
lasso = glmnet(x,y, alpha=1,family="binomial",
                    lambda=lambda_grid, data=sens)
vip(lasso, num_features=12, geom="point", include_type=TRUE)
par(mfrow=c(1,2))
plot(lasso)
cv.lasso <- cv.glmnet(x,y, alpha=1, family="binomial")
plot(cv.lasso)
lambda_min <- cv.lasso$lambda.min
lambda_1se <- cv.lasso$lambda.1se
coef(cv.lasso,s=lambda_1se)

# Elastic Net
EN = glmnet(x,y, alpha=0.5, family="binomial",
                 lambda=lambda_grid, data=sens)
vip(EN, num_features=12, geom="point", include_type=TRUE)
par(mfrow=c(1,2))
plot(EN)
cv.EN <- cv.glmnet(x,y, alpha=0.5, family="binomial")
plot(cv.EN)
lambda_min <- cv.EN$lambda.min
lambda_1se <- cv.EN$lambda.1se
coef(cv.EN,s=lambda_1se)

# all plot together
par(mfrow=c(1,3))

plot(ridge)
plot(lasso)
plot(EN)

plot(cv.ridge)
plot(cv.lasso)
plot(cv.EN)

out <- cbind(coef(cv.ridge,s=lambda_1se),coef(cv.lasso,s=lambda_1se),
             coef(cv.EN,s=lambda_1se))
colnames(out) <- c("Ridge", "LASSO", "EN")
out







