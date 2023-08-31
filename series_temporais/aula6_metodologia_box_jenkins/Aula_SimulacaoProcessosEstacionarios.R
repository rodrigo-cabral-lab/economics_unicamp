#Processos Estacionários
#Prof. Rosangela Ballini


#Processos Estocásticos

#Modelo AR
set.seed(12345)
N=1000

#séries simuladas - Processos AR
y1=arima.sim(model=list(ar=0.9),n=N)

par(mfrow=c(3,1))
plot.ts(y1,xlab='tempo',ylab='observações')
acf(y1,main=expression(paste("AR(1) com ", theta, "=0.9")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y1,main=expression(paste("AR(1) com ", theta, "=0.9")),xlab='defasagem',ylab=expression(Y[t]))

y2=arima.sim(model=list(order(1,0,0),ar=-0.8),n=N)

par(mfrow=c(3,1))
plot.ts(y2,xlab='tempo',ylab='observações')
acf(y2,main=expression(paste("AR(1) com ", theta, "=-0.8")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y2,main=expression(paste("AR(1) com ", theta, "=-0.8")),xlab='defasagem',ylab=expression(Y[t]))


par(mfrow=c(3,1))
y3=arima.sim(model=list(order(2,0,0),ar=c(0.75, -0.5)),n=N)

plot.ts(y3,xlab='tempo',ylab='observações')
acf(y3,main=expression(paste("AR(2) com ", phi, "=0.75 e -0.5")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y3,main=expression(paste("AR(2) com ", phi, "=0.75 e -0.5")),xlab='defasagem',ylab=expression(Y[t]))


#séries simuladas - Processos MA
y4=arima.sim(model=list(order(0,0,1),ma=0.8),n=N)

par(mfrow=c(3,1))
plot.ts(y4,xlab='tempo',ylab='observações')
acf(y4,main=expression(paste("MA(1) com ", theta, "=0.8")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y4,main=expression(paste("MA(1) com ", theta, "=0.8")),xlab='defasagem',ylab=expression(Y[t]))

y5=arima.sim(model=list(order(0,0,2),ma=c(-0.6,0.8)),n=N)

par(mfrow=c(3,1))
plot.ts(y5,xlab='tempo',ylab='observações')
acf(y5,main=expression(paste("MA(2) com ", theta, "=(-0.6,0.8)")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y5,main=expression(paste("MA(2) com ", theta, "=(-0.6,0.8)")),xlab='defasagem',ylab=expression(Y[t]))

#séries simuladas - Processos ARMA
y6=arima.sim(model=list(order(1,0,1),ar=0.8,ma=0.6),n=N)

par(mfrow=c(3,1))
plot.ts(y6,xlab='tempo',ylab='observações')
acf(y6,main=expression(paste("ARMA(1,1) com ", phi, "=0.8,", theta, " =0.6")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y6,main=expression(paste("ARMA(1,1) com ", phi,  "=0.8,", theta, " =0.6")),xlab='defasagem',ylab=expression(Y[t]))

y7=arima.sim(model=list(order(2,0,1),ar=c(-0.3,0.5),ma=0.8),n=N)

par(mfrow=c(3,1))
plot.ts(y7,xlab='tempo',ylab='observações')
acf(y7,main=expression(paste("ARMA(1,1) com ", theta, "=(-0.6,0.8)")),xlab='defasagem',ylab=expression(Y[t]))
pacf(y7,main=expression(paste("ARMA(2,1) com ", theta, "=(-0.6,0.8)")),xlab='defasagem',ylab=expression(Y[t]))


#Exerc?cio IPCA
getwd()
setwd("")
#leitura dos dados
ipca=ts(IPCA[,2], start=1995, freq=12)
par(mfrow=c(3,1))
plot.ts(ipca,main='IPCA',ylab='Indice',xlab='Meses',col='blue',bty='l')
grid()
acf(ipca,lag.max=36)
pacf(ipca)

