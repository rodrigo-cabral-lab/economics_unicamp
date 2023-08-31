#Aplicação Filtro HP

gdp=ts(DadosAula2[,2],start=c(1947,1),freq=4)
plot(gdp)

install.packages('mFilter')
library(mFilter)

hp <- hpfilter(gdp, freq = 1600)

par(mfrow=c(2,1),cex=.8)
plot(hp$x, main="Hodrick-Prescott filter: Trend",
     col=1, ylab="")
lines(hp$trend,col=2)

plot(hp$cycle,
     main="Hodrick-Prescott filter: Cycle",
     col=2, ylab="", ylim=range(hp$cycle,na.rm=TRUE))
