#Regressão Espúria

library(lmtest) 

set.seed (123456) 

e1 <- rnorm(500) 
e2 <- rnorm(500)

trd <- 1:500

y1 <- 0.8*trd + cumsum(e1)
y2 <- 0.6*trd + cumsum(e2)

par(mfrow=c(1,1))
plot.ts(y1,col='blue')
par(new=TRUE)
plot.ts(y2,col='red')

sr.reg <- lm(y1~y2)
summary(sr.reg)

acf(sr.reg$residuals)

sr.dw <- dwtest(sr.reg)$statistic

# Cointegração

install.packages('urca')
library(urca)

#Séries Sintéticas
set.seed(123456)

e1=rnorm(200)
e2=rnorm(200)

y1=ts(cumsum(e1))

y2=ts(0.6*y1+e2)

par(mfrow=c(1,1))
plot.ts(y1,col='blue')
par(new=TRUE)
plot.ts(y2,col='red')

#Procedimento de Engle-Granger
eq.reg=lm(y2~y1)
summary(eq.reg)

erro=ts(residuals(eq.reg))
plot(erro)
acf(erro)

erro.df=ur.df(erro,type='none',lags=24,selectlags='BIC')
summary(erro.df)


#Procedimento de Phillips-Ouliaries
X=data.frame(y1,y2)

x.po=ca.po(X, demean='constant',lag='short',type='Pz')
#Defina demean =constant para considerar as séries do modelo com constante (drift). 
#Utilize esta opção se as séries envolvidas possuírem média constante diferente de zero. 
#A opção demean =trend considera o caso com séries com tendência e drift e o 
#caso demean =none contempla a situação de séries sem tendência e drift.
#Se lag='short' : [4(T/100)^0.25]. 
#Se lag='long': [12(T/100)^0.25], 
#em que T indica o tamanho da série.
#Pz = estatística do traço
#Pu=estatística da razão de variância

summary(x.po)

plot(x.po)

# Modelo VEC com Procedimento Engle-Granger
install.packages('dynlm')
library(dynlm)

ecm.reg1 = dynlm(d(y1) ~L(erro)+L(d(y1))+L(d(y2)))
summary(ecm.reg1)

ecm.reg2 = dynlm(d(y2) ~L(erro)+L(d(y1))+L(d(y2)))
summary(ecm.reg2)
acf(ecm.reg2$residuals)


#Causalidade de Granger
install.packages('lmtest')
require(lmtest)

grangertest(diff(y2)~diff(y1), order=1)


# Modelo VEC com Procedimento Phillips-Ouliaries
resid_coint=ts(x.po@res)

po.reg2=dynlm(d(y2)~L(resid_coint)+L(d(y1))+L(d(y2)))
summary(po.reg2)




