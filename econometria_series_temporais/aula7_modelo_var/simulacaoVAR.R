#Processos Estacionários
#Prof. Rosangela Ballini

#Exemplos de Séries Temporais

library(readxl)
#Exercício IPCA
#leitura dos dados
#IPCA <- read_excel("IPCA.xlsx")
ipca=ts(IPCA[,2], start=1998, freq=12)
par(mfrow=c(1,1))
plot(ipca,main='IPCA',ylab='Indice',xlab='Meses',col='blue',bty='l')
grid()
par(mfrow=c(2,1))
acf(ipca,lag.max=36)
pacf(ipca,lag.max=36)


#Instalação de pacotes
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("FinTS")

#Carregando pacotes
library(tseries)
library(forecast)
library(lmtest)
library(FinTS)

### Candidatos: ARMA(2,2), ARMA(2,1), ARMA(1,1). ARMA(1,0)
fit1=coeftest(Arima(ipca,order=c(2,0,2),method='ML',include.mean = TRUE));fit1
fit2<-coeftest(Arima(ipca,order=c(2,0,1),include.mean = TRUE,method='ML')); fit2
fit3<-coeftest(Arima(ipca,order=c(1,0,1),include.mean = TRUE,method='ML')); fit3
fit4<-coeftest(Arima(ipca,order=c(1,0,0),include.mean = TRUE,method='ML')); fit4

ajust1=Arima(ipca,order=c(2,0,2),method='ML',include.mean = TRUE)
ajust2<-Arima(ipca,order=c(2,0,1),include.mean = TRUE,method='ML')
ajust3<-Arima(ipca,order=c(1,0,1),include.mean = TRUE,method='ML')
ajust4<-Arima(ipca,order=c(1,0,0),include.mean = TRUE,method='ML')

AIC(ajust1,ajust2,ajust3,ajust4)
BIC(ajust1,ajust2,ajust3,ajust4)


### Etapa 3 - VERIFICAÇÃO

#Análise das autocorrelações

Box.test(ajust4$residuals,lag=4,type="Ljung-Box", fitdf=1)
Box.test(ajust4$residuals,lag=8,type="Ljung-Box", fitdf=1)
Box.test(ajust4$residuals,lag=12,type="Ljung-Box", fitdf=1) 

#Correlogramas dos resíduos
par(mfrow=c(1,2))
acf(resid(ajust4),main='Resíduos ARMA(1,0)',ci.type='ma')
pacf(resid(ajust4),main='Resíduos ARMA(1,0)')

#Normalidade dos resmduos
par(mfrow=c(2,2))
hist(ajust4$residuals, freq=F, ylab='Densidade', xlab='Resíduos', main='Resíduos', col=4)
plot(density(ajust4$residuals, kernel = c("gaussian")), main="Resíduos", col=4, lwd=2)   #Função de densidade estimada
qqnorm(ajust4$residuals, ylab='Quantis amostrais', xlab='Quantis teóricos', main='Quantil-Quantil', col=2)
qqline(ajust4$residuals)  

shapiro.test(ajust4$residuals)   # Teste Shapiro-Wilk
jarque.bera.test(ajust4$residuals) # Teste Jarque-Bera

ArchTest(ajust4$residuals, lag=4) # Teste ARCH - Teste p/ heteroc. condicional
ArchTest(ajust4$residuals, lag=8)
ArchTest(ajust4$residuals, lag=12)


#Previs?es
prev <- predict(ajust4, n.ahead=4)
prev
#graficos
par(mfrow=c(1,1))

ts.plot(window(ipca, start=2007),
        prev$pred,
        prev$pred+1.96*prev$se,
        prev$pred-1.96*prev$se,col=c(1,2,2,2), lty=c(1,1,2,2))

#Avaliar as previs?es
#Modelo 1
erro1 <- matrix(NA, nrow=length(window(ipca, start=2000)), ncol=1)
for (i in 1:length(erro1)){
  erro1[i] <- ipca[(length(window(ipca, end=c(1999, 12)))+i)] -
    predict(
      arima(ipca[1:(length(window(ipca, end=c(1999, 12)))+i-1)], order=c(1,0,0)),
      n.ahead=1)$pred
  erro1 <- ts(erro1, start=2000, freq=12)
}

reqm1 <- sqrt(sum(erro1^2)/length(erro1))
reqm1
eam1 <- sum(abs(erro1))/length(erro1)
eam1


