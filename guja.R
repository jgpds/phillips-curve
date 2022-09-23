#ATIVIDADE
#UNIVERSIDADE FEDERAL DA PARA?BA
#DEPARTAMENTO DE ECONOMIA
#DISCIPLINA: ECONOMETRIA - TURMA 2021.2
#PROF.: SINEZIO FERNANDES MAIA
#ALUNO: JOAO GUILHERME PEREIRA DOS SANTOS
#guja
setwd("C:/Users/55819/Desktop/R Trabalho/ggplot2")
rm(list = ls())
date()
options(scipen = 9999)
options(max.print = 100000)

Guja211M=read.table("Guja211.txt", head=T)
Guja211M

Guja211<-ts(Guja211M, frequency=4, start=c(1947,1))
Guja211

plot.ts(Guja211, col="blue",
        main="Produto Interno Bruto", xlab="Anos", lwd=2)

LogGuja211=log(Guja211)

plot.ts(LogGuja211, col="blue",
        main="logaritmo do PIB", xlab="Anos", lwd=2)

acf(Guja211M, lag=36, plot=F)

LogGuja211M=log(Guja211M)
acf(LogGuja211M, lag=36, plot=F)

#correlograma

acf(Guja211M, lag=36, col="red", lwd=3, main="PIB")
acf(LogGuja211M, lag=36, col="red", main="logaritmo do PIB")

# teste q box pierce

Box.test(LogGuja211, lag=1, type="Box-Pierce")
Box.test(LogGuja211, lag=2, type="Box-Pierce")
Box.test(LogGuja211, lag=4, type="Box-Pierce")
Box.test(LogGuja211, lag=8, type="Box-Pierce")
Box.test(LogGuja211, lag=16, type="Box-Pierce")
Box.test(LogGuja211, lag=20, type="Box-Pierce")

require(urca)

#Testes Dickey-Fuller
 #modelo1
dickey1<-ur.df(LogGuja211, lags=0, type="none")
summary(dickey1)
 #modelo2
dickey2<-ur.df(LogGuja211, lags=0, type="drift")
summary(dickey2)
 #modelo3
dickey3<-ur.df(LogGuja211, lags=0, type="trend")
summary(dickey3)

# Dickey-Fuller Ampliado
dickey4<-ur.df(LogGuja211, lags=4, type="none")
summary(dickey4)
dickey5<-ur.df(LogGuja211, lags=10, type="none")
summary(dickey5)

# Teste menor AIC
dickey6<-ur.df(LogGuja211, lags=10, type="none", selectlags="AIC")
summary(dickey6)

# criterio bic
dickey7<-ur.df(LogGuja211, lags=10, type="none", selectlags="BIC")
summary(dickey7)
#criterio bic com intercepto e sem tendencia
dickey8<-ur.df(LogGuja211, lags=10, type="none", selectlags="BIC")
summary(dickey8)
#criterio bic com intercepto e  tendencia
dickey8<-ur.df(LogGuja211, lags=10, type="trend", selectlags="BIC")
summary(dickey8)

#Teste de Dickey-Fuller Ampliado - 1 DIFERENÇA

dickeyd1<-ur.df(diff(LogGuja211), lags=0, type="drift")
summary(dickeyd1)

dickeyd1b<-ur.df(diff(LogGuja211), lags=10, type="none", selectlags = "BIC")
summary(dickeyd1b)

dickeyd2<-ur.df(diff(LogGuja211), lags=10, type="drift", selectlags = "BIC")
summary(dickeyd2)

dickeyd3<-ur.df(diff(LogGuja211), lags=10, type="trend", selectlags = "BIC")
summary(dickeyd3)

#Teste de Dickey-Fuller Ampliado - 2 DIFERENÇA

dickeydd1<-ur.df(diff(diff(LogGuja211)), lags=10, type="none", selectlags = "BIC")
summary(dickeydd1)

dickeydd2<-ur.df(diff(diff(LogGuja211)), lags=10, type="drift", selectlags = "BIC")
summary(dickeydd2)

dickeydd2<-ur.df(diff(diff(LogGuja211)), lags=10, type="drift", selectlags = "BIC")
summary(dickeydd2)

dickeydd3<-ur.df(diff(diff(LogGuja211)), lags=0, type="trend", selectlags = "BIC")
summary(dickeydd3)
