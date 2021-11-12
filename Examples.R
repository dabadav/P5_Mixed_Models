#
# mixed models
#

rm(list=ls())
#load("f:/oxford.rda")
load(url("http://www-eio.upc.es/~jan/Data/smsp/oxford.rda"))
ls()
X
class(X)
head(X)

library(nlme)
oxford <- groupedData(height~age|subject, data=X)

head(oxford)
head(oxford,20)

class(oxford)
colnames(oxford)
formula(oxford)

table(oxford$subject)

nobs <- nrow(oxford)
nobs

nboys <- nobs/9
nboys

length(table(oxford$subject))

plot(oxford)

x <- runif(20)
y <- runif(20)
plot(x,y)
plot(x)

model.1 <- lm(height~age,data=oxford)
summary(model.1)

attach(oxford)
plot(age,height,pch=1)
abline(model.1,col="green",lwd=2)

e <- resid(model.1)

yhat <- fitted(model.1)

hist(e,breaks=20)

qqnorm(e)
qqline(e)

plot(yhat,e)
abline(h=0)

table(age)
plot(1:nobs,e,type="l")
points(1:nobs,e,pch=19)
abline(h=0)

plot(1:nobs,e[sample(1:nobs)])
abline(h=0)

boxplot(e~oxford$subject)
abline(h=0)

output <- lmList(oxford)
output

M <- coefficients(output)
M

boxplot(M[,1],main="Intercepts")
windows()
boxplot(M[,2],main="Slopes")

inter <- intervals(lmList(oxford))
inter
plot(inter)

model.2 <- lme(height~age,data=oxford,
               random=~1|subject)
summary(model.2)

attributes(model.2)

model.2$sigma
model.2$varFix
model.2$apVar

s <-c(8.096601,1.31075)
va <- s^2
intraclasscor <- va[1]/sum(va)
intraclasscor

anova(model.2,model.1)

logLik(model.1)
ll.1 <- logLik(model.1)
aic.1 <- -2*ll.1 + 2*3
aic.1

AIC(model.1)
AIC(model.2)

model.3 <- lme(height~age,data=oxford,
               random=~age|subject)
summary(model.3)

anova(model.3,model.2)

plot(model.3)

e3 <- resid(model.3)

boxplot(e3~oxford$subject)
abline(h=0)

windows()
qqnorm(e3)
qqline(e3)

?augPred

plot(augPred(model.3))

plot(model.3,resid(.)~age|subject)


oxford$age2 <- age^2

output2 <- lmList(height~age+age2,data=oxford)
output2

M2 <- coefficients(output2)

inter2 <- intervals(lmList(height~age+age2,data=oxford))

plot(inter2)

model.3B <- lme(height~age+age2,data=oxford,
               random=~age|subject)

summary(model.3B)

model.4 <- lme(height~age+age2,data=oxford,
               random=~age+age2|subject)
summary(model.4)

anova(model.4,model.3B)

plot(model.4)

plot(model.4,resid(.)~age|subject)



















