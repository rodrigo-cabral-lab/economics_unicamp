#Modelo VAR estrutural

#Instalar pacotes
install.packages('xts')
install.packages('MASS')
install.packages('urca')
install.packages('MTS')
install.packages('vars')
install.packages('lmtest')

#Carregar os pacotes
require(xts)
require(MASS)
require(urca)
require(MTS)
require(vars)
require(lmtest)
require(tseries)

#Carregar Dados
data(Canada)
View(Canada)

plot(Canada)
#Identificação do modelo
m2=VARselect(diffM(Canada),lag.max=8,type='const')
m2


#Estimaçao do VAR
modelo=vars::VAR(diffM(Canada),p=2,type='const')
modelo

#Resumo por equação
summary(modelo,equation='e')
plot(modelo,names='e')

summary(modelo,equation='prod')
plot(modelo,names='prod')


summary(modelo,equation='rw')
plot(modelo,names='rw')

summary(modelo,equation='U')
plot(modelo,names='U')

#Discussão sobre a significância estatística das estimativas
#Ajuste do Modelo VAR com Restrições
modelo.ser=restrict(modelo,method='ser',thresh = 2)

#usa para ter a matriz de restrição a significância estatística das estimativas
summary(modelo.ser)
modelo.ser$restrictions #apresenta os resultados 
Acoef(modelo.ser) #apresenta os coeficientes da matriz

#Previsão
model.forec=predict(modelo.ser,n.ahead=10,ci=0.95)
plot(model.forec)

#Lista de matrizes contendo os valores previstos, limites superior e inferior 
model.forec$fcst


#Restrições Manuais
mat=matrix(rep(1,36),nrow=4,ncol=9) 
#4  variáveis=4 linhas e 9 é o número de parâmetros por equação
mat #1a linha: e, 2a linha: prod, 3a linha: rw, 4a linha: U
mat[1,3]=0
mat[1,4]=0
mat
modelo.manual=restrict(modelo,method='manual',resmat=mat)#resmat=matriz de restrição
modelo.manual
summary(modelo.manual)
modelo.manual$varresult
Acoef(modelo.manual) #apresenta os coeficientes da matriz sem termos determinísticos
Bcoef(modelo.manual) #apresenta com termos determinísticos



#Ajuste modelo SVAR: usa estimador de máxima verossimilhança
#Usando modelo A: restrição sobre a matriz contemporânea
amat=diag(4)
diag(amat)=NA
amat
amat[1,2]=NA
amat[1,3]=NA
amat[3,2]=NA
amat[4,1]=NA
amat

modelo.svarA=SVAR(modelo,estmethod ='direct',Amat=amat,Bmat=NULL,hessian=T,method='BFGS')
modelo.svarA$A
modelo.svarA$Ase
modelo.svarA$B

#Construção das Função impulso resposta
svar.ira <- irf(modelo.svarA, impulse = "rw", response = c("prod","e","U"), boot = T,cumulative=F,n.ahead=15)
svar.ira
plot(svar.ira)

#Decomposição da Variância
svarA.dec=fevd(modelo.svarA,n.ahead=20)
100*svarA.dec$prod
100*svarA.dec$e
100*svarA.dec$rw
100*svarA.dec$U



























