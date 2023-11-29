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
