#####Project Code#####

library(haven)
library(table1)
library(tidyr)
library(dplyr)
library(ggplot2)

#read in data
chs <- read_sas("chs2012_public.sas7bdat.html")

##data cleaning

#subsetting only needed variables
myvars <- c("currentasthma12", "everasthma", "anymold", "anymice", "seenmice30days",
            "anyroach", "seenroach30days", "shshome12", "shsmokerinhome", 
            "agegroup5", "sex", "newrace", "environ3", "environ2", "environ1",
            "povertygroup", "bmi", "weight12in4", "smoker", "smokecat", "nspd", 
            "mood49", "education")
df <- chs[myvars]

#exclude observation with missing outcome
df <- df %>% drop_na(currentasthma12)

#restrict data to only those who ever had asthma
df <- df[df$everasthma == 1, ]

#handling missing data

##data exploration
#mice
boxplot(df$currentasthma12, df$environ2) #possible outliers for those that have
#not had asthma attack in the last 12 months
ggplot(data = df) + 
  geom_bar(mapping = aes(seenmice30days))
ggplot(data = df) + 
  geom_bar(mapping = aes(anymice))
table(df$currentasthma12, df$anymice)
#roach
boxplot(df$currentasthma12, df$environ1) #possible outliers for those that have
#not had asthma attach in the last 12 months
ggplot(data = df) + 
  geom_bar(mapping = aes(seenroach30days))
ggplot(data = df) + 
  geom_bar(mapping = aes(anyroach))
table(df$currentasthma12, df$anyroach)
#mold
boxplot(df$currentasthma12, df$environ3)
ggplot(data = df) + 
  geom_bar(mapping = aes(anymold))
ggplot(data = df) + 
  geom_bar(mapping = aes(environ3))
table(df$currentasthma12, df$anymold)
#bmi
boxplot(df$currentasthma12, df$bmi)
hist(df$bmi)
#age
table(df$currentasthma12, df$agegroup5)

#summary
summary(df)

#table1
table1(~ factor(sex) + factor(agegroup5) + factor(newrace) | currentasthma12, data = df)
