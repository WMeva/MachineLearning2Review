# All packages needed
library(ISLR2)
library(leaps)
library(glmnet)
library(pls)
library(splines)
library(gam)
library(tree)

set.seed(2702) # Setting a seed
NewHitters <- na.omit(Hitters)  # Deleting NA values

# Splitting the Data 
train<-sample(c(TRUE, FALSE), nrow(NewHitters), replace=TRUE)
test <-(!train)

x<-model.matrix(Salary~., NewHitters)[,-1] # x matrix
y<-NewHitters$Salary # y vector

train <- sample(1:nrow(x), size=nrow(x)/2)
test<-(-train)




#############
# CHAPTER 6
#############

# SUBSET SELECTION METHODS

# 1. Best Subset Selection

regfit.full <-regsubsets(Salary ~., nvmax=19,NewHitters)  # Without nvmax, it performs up to 8-variable model
regfit.summary <-summary(regfit.full)
regfit.summary$adjr2 #Adjusted R^2
regfit.summary$rsq # R^2
coef(regfit.full, 11) # 11 was calculated by which.max(regfit.summary$adjr2)

# 2. Forward Selection
regfit.fwd<-regsubsets(Salary~., data=NewHitters, nvmax=19, method="forward")

# 3. Backward Selection
regfit.bwd<-regsubsets(Salary~., data=NewHitters, nvmax=19, method="backward")


# CHOOSE THE BEST MODEL

# Validation Approach 
mses<-rep(NA, 19)
for (i in 1:19){
  yhat<-predict(regfit.best, NewHitters[test,],i)
  mses[i]<-mean((NewHitters$Salary[test]-yhat)^2)
}
print(which.min(mses))
coef(regfit.best,7)

# Cross-Validation
k<-10
n<-nrow(NewHitters)
set.seed(1)
folds<-sample(rep(1:k, length=n))
cv.errors<-matrix(data=NA, nrow=k, ncol=19, dimnames=list(NULL, paste(1:19)))
for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data=NewHitters[folds != j, ], nvmax=19) 
  for (i in 1:19) {
    pred <- predict(best.fit, NewHitters[folds==j, ],id=i)
    cv.errors[j,i]<- mean((NewHitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors<-apply(X=cv.errors,MARGIN=2, mean) #MARGIN=2 indicates column
which.min(mean.cv.errors) 
coef(regfit.full, 10)


# RIDGE REGRESSION
power.value<-seq(from=10, to=-2, length=100) 
grid<-10^power.value

ridge.mod<-glmnet(x[train, ], y[train], alpha =0, lambda = grid, thresh=1e-12)
ridge.pred<-predict(ridge.mod, s=4, newx=x[test,])
mse.4<-mean((ridge.pred-y[test])^2) 
lm.pred<-predict(ridge.mod, s=0, newx=x[test,], exact=T, x=x[train, ], y=y[train])
mse <- mean((lm.pred-y[test])^2)
predict(ridge.mod, s=4, type="coefficients")

# Cross Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=0)
bestlam<-cv.out$lambda.min
ridge.pred<-predict(ridge.mod, s=bestlam, newx=x[test,])
mse <- mean((ridge.pred-y[test])^2)
out <-glmnet(x,y,alpha=0)
ridge.coef <- predict(out, type="coefficients", s=bestlam)



# LASSO
lasso.mod<-glmnet(x[train, ], y[train], alpha=1, lambda=grid)  # Alpha = 1 for Lasso
lasso.summary <- summary(lasso.mod)

# Cross-Validation
cv.out<-cv.glmnet(x[train, ], y[train], alpha=1)
bestlam <- cv.out$lambda.min

lasso.pred<-predict(lasso.mod, s=bestlam,newx=x[test,])
mse <- mean((lasso.pred-y[test])^2) 
out<-glmnet(x,y, alpha=1, lambda=grid)
lasso.coef<-predict(out, type="coefficients", s=bestlam)



# PCR

pcr.fit<-pcr(Salary~., data=NewHitters, subset = train, scale=TRUE, validation="CV")
pcr.pred<-predict(pcr.fit, x[test, ], ncomp=5)
mse <- mean((pcr.pred-y[test])^2)
pcr.fit<-pcr(y~x, scale=TRUE, ncomp=5)
summary(pcr.fit) 


# PLS

pls.fit<-plsr(Salary~., data=NewHitters, subset = train, scale=TRUE, validation="CV")
summary(pls.fit) 
pls.pred<-predict(pls.fit, x[test, ], ncomp=1)
mse <- mean((pls.pred-y[test])^2) 
pls.fit<-plsr(y~x, scale=TRUE, ncomp=1)
summary(pls.fit) 


###############
#  CHAPTER 7
###############

# POLYNOMIAL REGRESSION

# Fit a Poly Model
fit4 <- lm(wage ~ poly(age,4), data=Wage)
summary(fit4)$coef 
summary(fit4)$df 
summary(fit4)$adj.r.squared 

# Raw Poly Terms
fit4.manual <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=Wage)
summary(fit4.manual)$coef 
summary(fit4.manual)$df 
summary(fit4.manual)$adj.r.squared 


fit4.cbind <- lm(wage ~ cbind(age, age^2, age^3, age^4), data=Wage)
summary(fit4.cbind)$coef 
fit4.raw <-(lm(wage ~ poly(age, 4, raw=T), data=Wage))
summary(fit4.raw)$coef 


# Create  Values for Prediction
age.bounds <-range(Wage$age)
age.grid<-seq(from=age.bounds[1], to= age.bounds[2])
age.x<-list(age=age.grid)

# Predict Wage Using Age
fit4.pred <-predict(fit4, newdata=age.x, se = TRUE)
fit4.pred$fit 
fit4.manual.pred <-predict(fit4.manual, newdata=age.x, se = TRUE)
fit4.manual.pred$fit


# CIs for Predictions
se.ubound <- fit4.pred$fit + 2 * fit4.pred$se.fit 
se.lbound <- fit4.pred$fit - 2 * fit4.pred$se.fit
se.bands <- cbind(se.ubound, se.lbound)


# Choose Optimal Degree of Poly
fit.1 <- lm(wage ~ age)
fit.2 <- lm(wage ~ poly(age,2))
fit.3 <- lm(wage ~ poly(age,3))
fit.4 <- lm(wage ~ poly(age,4))
fit.5 <- lm(wage ~ poly(age,5))
poly.anova<-anova(fit.1, fit.2, fit.3, fit.4, fit.5)
poly.anova$`Pr(>F)`
poly.anova$RSS[1] - poly.anova$RSS[2] 


# Poly() vs Anova()
summary(fit.5)$coef 
(3.1446392)^2 

# Classification
fit.classification <- glm(I(wage > 250) ~ poly(age, 4), family = binomial)
summary(fit.classification)$coef 

preds.logit<-predict(fit.classification, newdata=age.x, type="link")
summary(preds.logit) 

preds.prob<-predict(fit.classification, newdata=age.x, type="response")
summary(preds.prob) 

# Step Function
table(cut(age,4))
fit.step <- lm(wage ~ cut(age, 4))
summary(fit.step)$coef




# SPLINES

fit.3knots <-lm(wage ~ bs(age, knots=c(25, 40, 60)))
summary(fit.3knots)$coef
wage_hat <- predict(fit.3knots, newdata=age.x, se=T)
wage_hat$se.fit 
se.ubound <- wage_hat$fit + 2 * wage_hat$se.fit 
se.lbound <- wage_hat$fit - 2 * wage_hat$se.fit
se.bands <- cbind(se.ubound, se.lbound)
se.bands 

#plot the 3-knot splines model
plot(age, wage, col="gray")
lines(age.grid, se.ubound, lty="dashed")
lines(age.grid, se.lbound, lty="dashed")
title("3-Knot Splines")


# bs() function
bs.3knots <-bs(age, knots=c(25,40,60))
class(bs.3knots) 
dim(bs.3knots)
attr(bs.3knots, "degree") 
bs.6df <-bs(age, df=6)
attr(bs.6df, "degree") 
attr(bs.6df, "knots") 
bs.degree4 <-bs(age, degree=4,df=6)
attr(bs.degree4, "knots") 
attr(bs(age, degree=2,df=6), "knots") 


# ns() function
ns.4df <- ns(age, df=4)
attr(ns.4df, "knots")
fit.ns <- lm(wage~ns.4df)
summary(fit.ns)$coef
pred.ns <- predict(fit.ns, age.x)
pred.ns


# Smoothing Splines
fit.loess2<-loess(wage~ age, span=.2)
pred.loess2<-predict(fit.loess2, data.frame(age.x))

fit.loess5<-loess(wage~ age, span=.5)
pred.loess5<-predict(fit.loess5, data.frame(age.x))
summary(pred.loess5)

#Plot the local regression lines
plot(age, wage, xlim=age.bounds, cex=.5, color="darkgrey")
title("Local Regression")
lines(age.grid, pred.loess2, col="red", lwd=2)
lines(age.grid, pred.loess5, col="blue", lwd=2)
legend("topright", legend=c("span = .2", "span = .5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)


# Local Regression
fit.loess2<-loess(wage~ age, span=.2)
pred.loess2<-predict(fit.loess2, data.frame(age.x))

fit.loess5<-loess(wage~ age, span=.5)
pred.loess5<-predict(fit.loess5, data.frame(age.x))

#Plot the local regression lines
plot(age, wage, xlim=age.bounds, cex=.5, color="darkgrey")
title("Local Regression")
lines(age.grid, pred.loess2, col="red", lwd=2)
lines(age.grid, pred.loess5, col="blue", lwd=2)
legend("topright", legend=c("span = .2", "span = .5"), col=c("red", "blue"), lty=1, lwd=2, cex=.8)




#    GAMs


# GAM with ns()
gam1 <-lm(wage ~ns(year,4) + ns(age, 5) + education)
summary(gam1) 
plot.Gam(gam1, se=TRUE, col="red")


# GAM with smoothing splines
gam.m3 <- gam(wage~s(year, 4) +s(age, 5)+ education)
summary(gam.m3)



# ANOVA for model comparison
gam.m1 <- gam(wage~ s(age, 5)+ education)
gam.m2 <- gam(wage~ year +s(age, 5)+education)
gam.m3 <- gam(wage~ s(year, 4) +s(age, 5)+education)
anova(gam.m1, gam.m2, gam.m3, test="F")


# GAM predictions


# GAM with lo()
gam.lo <- gam(wage~s(year, 4)+lo(age, span=.7)+education)
summary(gam.lo)


# lo() with interaction
gam.lo.i <-gam(wage ~ lo(year,age,span=.5) + education)
summary(gam.lo.i) 


# GAM for classification
gam.lr <- gam(I(wage > 250) ~ year + s(age,df = 5) + education, faimly = binomial)
summary(gam.lr)
plot(gam.lr, se=TRUE, col="red")


# Subset in gam()
table(education, I(wage > 250)) #Q23-1
gam.lr.s <- gam(I(wage > 250) ~ year + s(age,df = 5) + education, subset=(education != "1. < HS Grad"))
summary(gam.lr.s) #Q23-2
plot(gam.lr.s, se=TRUE, col="red")



################
#   CHAPTER 8
################

# Fitting Classification Tree
High <- factor(ifelse(Sales <= 8, "No", "Yes"))
Carseats <- data.frame(Carseats, High)
tree.carseats <- tree(High~. -Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)

tree.carseats
attach(Carseats)
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High~. -Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(104+50)/200

set.seed(7)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')

prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred <- predict(prune.carseats, Carseats.test,type ="class")
table(tree.pred, high.test)
(97+58)/200

prune.carseats <- prune.misclass(tree.carseats,best=14)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(102+52)/200

