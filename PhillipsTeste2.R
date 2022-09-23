
#ATIVIDADE
#UNIVERSIDADE FEDERAL DA PARA?BA
#DEPARTAMENTO DE ECONOMIA
#DISCIPLINA: ECONOMETRIA - TURMA 2021.2
#PROF.: SINEZIO FERNANDES MAIA
#ALUNO: JOAO GUILHERME PEREIRA DOS SANTOS

# Analisar modelos
# 2003-2021 check
# 2003-2010 check
# 2011-2016.5 check
# 2016.5-2018 check
# 2019-2021
#install.packages('sandwich') #pacote para calcular erros padrões robustos
#install.packages("pacman")
#install.packages("tsbox") #passar hiato para dataframe
#install.packages("writexl") #exportar df para excel
#install.packages("urca") 
#install.packages("rbcb")
setwd("C:/Users/55819/Desktop/R Trabalho/ggplot2")
rm(list = ls())
date()
options(scipen = 9999)
options(max.print = 100000)
require(urca)
library("writexl")
library(tsbox)
library(ggplot2)
library(faraway)
library(mFilter) 
library(dynlm)
library(fBasics)
library(lmtest) 
library(whitestrap)
library(FinTS) #arch
library(moments)
library(scales)

CambioNominal<-rbcb::get_series(3695, start_date = "2003-01-01",
                             end_date = "2021-12-30");CambioNominal

IPP<-rbcb::get_series(225, start_date = "2003-01-01",
                                end_date = "2021-12-30")

IPP$`225`

IPADI<-rbcb::get_series(11758, start_date = "2003-01-01",
                         end_date = "2021-12-30")
IPADI$`11758`

IPADIBR<-rbcb::get_series(11757, start_date = "2003-01-01",
                        end_date = "2021-12-30")
IPADIBR$`11757`=(IPADIBR$`11757`/74.41)*100;IPADIBR$`11757`#data numero indice em 1982..(algo assim)




        #       CAMBIO REAL (IPP US E IPA-DI BR)
        #
IPPdata=read.table("tentativaIPP.txt", head=T); IPPdata
CambioReal<-(CambioNominal$`3695`*IPPdata$IPPUS)/IPPdata$IPPBR
CambioReal #USANDO IPP(US) E IPA-DI (CÓDIGO 11757) (BR)


    
       #        CAMBIO REAL (IPP US E IPA-10 BR)
IPA10=read.table("ipa10alterado.txt", head=T); IPA10$IPA.10
CambioReal2<-(CambioNominal$`3695`*IPPdata$IPPUS)/IPA10$IPA.10
CambioReal2 #usando IPP(US) E IPA-10 (IPEA) (BR)

# write_xlsx(Phillips,"C:/Users/55819/Desktop/R Trabalho/TabelaDadosExcel.xlsx") Exportar pra excel
Phillips=read.table("Dadoscomn.txt", head=T); Phillips
Phillips=cbind(Phillips, CambioReal2);Phillips
PhillipsGraph=Phillips;
PhillipsGraph$Data<-as.Date(PhillipsGraph$Data, format = "%d/%m/%Y")
names(Phillips) <- c("n","Governo","Data", "PibR", "ipca", "eipca", "Cambio", "CambioReal2")
Phillips$Cambio=Phillips$CambioReal2
attach(Phillips)

Phillips
ipca
length(ipca)

head(ipca, n=3); head(eipca, n=3)
tail(eipca, n=3); tail(eipca, n=3)

## Transformando em Séries Temporais ##
ipca<-ts(ipca,frequency=12, start=c(2003,1), end=c(2021,12));ipca;plot(ipca)
  ipcaLULA<-window(ipca,frequency=12, start=c(2003,1), end=c(2010,12))
  ipcaDILMA<-window(ipca,frequency=12, start=c(2011,1), end=c(2016,5))
  ipcaTEMER<-window(ipca,frequency=12, start=c(2016,6), end=c(2018,12))
  ipcaBOLSONARO<-window(ipca,frequency=12, start=c(2019,1), end=c(2021,12))
eipca<-ts(eipca,frequency=12, start=c(2003,1), end=c(2021,12));eipca;plot(eipca)
  eipcaLULA<-window(eipca,frequency=12, start=c(2003,1), end=c(2010,12))
  eipcaDILMA<-window(eipca,frequency=12, start=c(2011,1), end=c(2016,5))
  eipcaTEMER<-window(eipca,frequency=12, start=c(2016,6), end=c(2018,12))
  eipcaBOLSONARO<-window(eipca,frequency=12, start=c(2019,1), end=c(2021,12))
Cambio<-ts(Cambio,frequency=12, start=c(2003,1), end=c(2021,12));Cambio;plot(Cambio)
  CambioLULA<-window(Cambio,frequency=12, start=c(2003,1), end=c(2010,12))
  CambioDILMA<-window(Cambio,frequency=12, start=c(2011,1), end=c(2016,5))
  CambioTEMER<-window(Cambio,frequency=12, start=c(2016,6), end=c(2018,12))
  CambioBOLSONARO<-window(Cambio,frequency=12, start=c(2019,1), end=c(2021,12))
pib<-ts(PibR, frequency=12, start = c(2003,1), end=c(2021,12));pib;plot(pib)
  PibRLULA<-window(pib,frequency=12, start=c(2003,1), end=c(2010,12))
  PibRDILMA<-window(pib,frequency=12, start=c(2011,1), end=c(2016,5))
  PibRTEMER<-window(pib,frequency=12, start=c(2016,6), end=c(2018,12))
  PibRBOLSONARO<-window(pib,frequency=12, start=c(2019,1), end=c(2021,12))
dados=cbind(ipca, eipca, pib, Cambio); dados
plot(dados)


Pib.hp<-hpfilter(na.omit(pib, type='lambda', freq=14400))
hiato<-Pib.hp$cycle; hiato

hiatodf = ts(hiato, start=c(2003,1), frequency = 12)
hiatodf = ts_df(hiatodf);hiatodf
hiatodf2<-data.frame(PhillipsGraph$Data,hiatodf$value);hiatodf2

plot(hiato)
hiato<-ts(hiato,frequency = 12, start=c(2003,1), end=c(2021,12));hiato

PhillipsSimples=cbind(ipca,eipca,hiato,Cambio);PhillipsSimples
plot(PhillipsSimples)
regressao=PhillipsSimples

## EM LOG ##
min(ipca);min(eipca);min(Cambio);min(hiato)
# ipca em log #
lnipca1=log(ipca);lnipca1
# eipca em log #
lneipca1=log(eipca);lneipca1
# Cambio em log #
lnCambio1=log(Cambio);lnCambio1
# hiato em log #
hiato1=hiato-min(hiato);hiato1
lnhiato1=log(hiato+(2*(-min(hiato))));lnhiato1;plot(lnhiato1)


lag<-stats::lag






                
                #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                #                                                                                                           #
                #                     P H I L L I P S     B A C K W A R D     L O O K I N G   ( L I N E A R )               #
                #                                                                                                           #
                #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                  # MODELO COMPLETO
{
PhillipsB=dynlm(ipca~lag(ipca,-1)+lag(ipca,-2)+lag(hiato,-1)+Cambio)
summary(PhillipsB)
regressao=PhillipsB
variavel=Cambio
name='Cambio'
Var_Dependente=ipca
name0="IPCA"
Var_Independente1=lag(ipca,-1)
name1='IPCA Defasado em 1'
Var_Independente2=lag(ipca,-2)
name2='IPCA Defasado em 2'
Var_Independente3=lag(hiato,-1)
name3='Hiato Defasado em 1'
Var_Independente4=Cambio
name4='Cambio'
}
                #2003-2010 BACKWARD-LOOKING LINEAR#
{
ipca2003_2010<-window(ipca, frequency=12, start=c(2003,1), end=c(2010,12))
hiato2003_2010<-window(hiato, frequency=12, start=c(2003,1), end=c(2010,12))
Cambio2003_2010<-window(Cambio, frequency=12, start=c(2003,1), end=c(2010,12))

PhillipsB2003_2010=dynlm(ipca2003_2010~lag(ipca2003_2010,-1)+lag(ipca2003_2010,-2)+lag(hiato2003_2010,-1)+Cambio2003_2010)
summary(PhillipsB2003_2010)
regressao=PhillipsB2003_2010

variavel=Cambio2003_2010
name='Cambio2003_2010'
Var_Independente1=lag(ipca2003_2010,-1)
name1='ipca2003_2010 Defasado em 1'
Var_Independente2=lag(ipca2003_2010,-2)
name2='ipca2003_2010 Defasado em 2'
Var_Independente3=lag(hiato2003_2010,-1)
name3='hiato2003_2010 Defasado em 1'
Var_Independente4=Cambio2003_2010
name4='Cambio2003_2010'
}

                #2011-2016.5 BACKWARD-LOOKING LINEAR#
{
ipca2011_2016.5<-window(ipca, frequency=12, start=c(2011,1), end=c(2016,5))
hiato2011_2016.5<-window(hiato, frequency=12, start=c(2011,1), end=c(2016,5))
Cambio2011_2016.5<-window(Cambio, frequency=12, start=c(2011,1), end=c(2016,5))

PhillipsB2011_2016.5=dynlm(ipca2011_2016.5~lag(ipca2011_2016.5,-1)+lag(ipca2011_2016.5,-2)+lag(hiato2011_2016.5,-1)+Cambio2011_2016.5)
summary(PhillipsB2011_2016.5)
regressao=PhillipsB2011_2016.5
residuos=residuals(regressao)
variavel=Cambio2011_2016.5
name='Cambio2011_2016.5'
Var_Independente1=lag(ipca2011_2016.5,-1)
name1='ipca2011_2016.5 Defasado em 1'
Var_Independente2=lag(ipca2011_2016.5,-2)
name2='ipca2011_2016.5 Defasado em 2'
Var_Independente3=lag(hiato2011_2016.5,-1)
name3='hiato2011_2016.5 Defasado em 1'
Var_Independente4=Cambio2011_2016.5
name4='Cambio2011_2016.5'
}

                #2016.5-2018 BACKWARD-LOOKING LINEAR#
{
ipca2016.5_2018<-window(ipca, frequency=12, start=c(2016,5), end=c(2018,12))
hiato2016.5_2018<-window(hiato, frequency=12, start=c(2016,5), end=c(2018,12))
Cambio2016.5_2018<-window(Cambio, frequency=12, start=c(2016,5), end=c(2018,12))

PhillipsB2016.5_2018=dynlm(ipca2016.5_2018~lag(ipca2016.5_2018,-1)+lag(ipca2016.5_2018,-2)+lag(hiato2016.5_2018,-1)+Cambio2016.5_2018)
summary(PhillipsB2016.5_2018)
regressao=PhillipsB2016.5_2018

variavel=Cambio2016.5_2018
name='Cambio2016.5_2018'
Var_Independente1=lag(ipca2016.5_2018,-1)
name1='ipca2016.5_2018 Defasado em 1'
Var_Independente2=lag(ipca2016.5_2018,-2)
name2='ipca2016.5_2018 Defasado em 2'
Var_Independente3=lag(hiato2016.5_2018,-1)
name3='hiato2016.5_2018 Defasado em 1'
Var_Independente4=Cambio2016.5_2018
name4='Cambio2016.5_2018'
}

                #2019-2021 BACKWARD-LOOKING LINEAR#
{
ipca2019_2021<-window(ipca, frequency=12, start=c(2019,1), end=c(2021,12))
hiato2019_2021<-window(hiato, frequency=12, start=c(2019,1), end=c(2021,12))
Cambio2019_2021<-window(Cambio, frequency=12, start=c(2019,1), end=c(2021,12))

PhillipsB2019_2021=dynlm(ipca2019_2021~lag(ipca2019_2021,-1)+lag(ipca2019_2021,-2)+lag(hiato2019_2021,-1)+Cambio2019_2021)
summary(PhillipsB2019_2021)
regressao=PhillipsB2019_2021

variavel=Cambio2019_2021
name='Cambio2019_2021'
Var_Independente1=lag(ipca2019_2021,-1)
name1='ipca2019_2021 Defasado em 1'
Var_Independente2=lag(ipca2019_2021,-2)
name2='ipca2019_2021 Defasado em 2'
Var_Independente3=lag(hiato2019_2021,-1)
name3='hiato2019_2021 Defasado em 1'
Var_Independente4=Cambio2019_2021
name4='Cambio2019_2021'
}

                #2008.8-2014.12 BACKWARD-LOOKING LINEAR# (MELHOR MODELO)
{
ipca2008.8_2014.12<-window(ipca, frequency=12, start=c(2008,8), end=c(2014,12))
hiato2008.8_2014.12<-window(hiato, frequency=12, start=c(2008,1), end=c(2014,12))
Cambio2008.8_2014.12<-window(Cambio, frequency=12, start=c(2008,1), end=c(2014,12))

PhillipsB2008.8_2014.12=dynlm(ipca2008.8_2014.12~lag(ipca2008.8_2014.12,-1)+lag(ipca2008.8_2014.12,-2)+lag(hiato2008.8_2014.12,-1)+Cambio2008.8_2014.12)
summary(PhillipsB2008.8_2014.12)
regressao=PhillipsB2008.8_2014.12

variavel=Cambio2008.8_2014.12
name='Cambio2008.8_2014.12'
Var_Independente1=lag(ipca2008.8_2014.12,-1)
name1='ipca2008.8_2014.12 Defasado em 1'
Var_Independente2=lag(ipca2008.8_2014.12,-2)
name2='ipca2008.8_2014.12 Defasado em 2'
Var_Independente3=lag(hiato2008.8_2014.12,-1)
name3='hiato2008.8_2014.12 Defasado em 1'
Var_Independente4=Cambio2008.8_2014.12
name4='Cambio2008.8_2014.12'
}                
                #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                #                                                                                                           #
                #                   P H I L L I P S     F O R W A R D     L O O K I N G   ( L I N E A R )                   #
                #                                                                                                           #
                #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#



{
PhillipsF=dynlm(ipca~lag(ipca,-1)+eipca+lag(hiato,-1)+Cambio)
summary(PhillipsF)
regressao=PhillipsF
variavel=Cambio
name='Cambio'
Var_Independente1=lag(ipca,-1)
name1='Ipca Defasado em 1'
Var_Independente2=eipca
name2='Expectativa da Inflação (eipca)'
Var_Independente3=lag(hiato,-1)
name3='Hiato Defasado em 1'
Var_Independente4=Cambio
name4='Cambio'
}

                #2003-2010 FORWARD-LOOKING LINEAR#
{
ipca2003_2010<-window(ipca, frequency=12, start=c(2003,1), end=c(2010,12))
eipca2003_2010<-window(eipca, frequency=12, start=c(2003,1), end=c(2010,12))
hiato2003_2010<-window(hiato, frequency=12, start=c(2003,1), end=c(2010,12))
Cambio2003_2010<-window(Cambio, frequency=12, start=c(2003,1), end=c(2010,12))

PhillipsF2003_2010=dynlm(ipca2003_2010~lag(ipca2003_2010,-1)+eipca2003_2010+lag(hiato2003_2010,-1)+Cambio2003_2010)
summary(PhillipsF2003_2010)
regressao=PhillipsF2003_2010

variavel=Cambio2003_2010
name='Cambio2003_2010'
Var_Independente1=lag(ipca2003_2010,-1)
name1='ipca2003_2010 Defasado em 1'
Var_Independente2=eipca2003_2010
name2='Expectatica da inflação (eipca)'
Var_Independente3=lag(hiato2003_2010,-1)
name3='hiato2003_2010 Defasado em 1'
Var_Independente4=Cambio2003_2010
name4='Cambio2003_2010'
}

                #2011-2016.5 FORWARD-LOOKING LINEAR#
{
ipca2011_2016.5<-window(ipca, frequency=12, start=c(2011,1), end=c(2016,5))
eipca2011_2016.5<-window(eipca, frequency=12, start=c(2011,1), end=c(2016,5))
hiato2011_2016.5<-window(hiato, frequency=12, start=c(2011,1), end=c(2016,5))
Cambio2011_2016.5<-window(Cambio, frequency=12, start=c(2011,1), end=c(2016,5))

PhillipsF2011_2016.5=dynlm(ipca2011_2016.5~lag(ipca2011_2016.5,-1)+eipca2011_2016.5+lag(hiato2011_2016.5,-1)+Cambio2011_2016.5)
summary(PhillipsF2011_2016.5)
regressao=PhillipsF2011_2016.5

variavel=Cambio2011_2016.5
name='Cambio2011_2016.5'
Var_Independente1=lag(ipca2011_2016.5,-1)
name1='ipca2011_2016.5 Defasado em 1'
Var_Independente2=eipca2011_2016.5
name2='Expectatica da inflação (eipca)'
Var_Independente3=lag(hiato2011_2016.5,-1)
name3='hiato2011_2016.5 Defasado em 1'
Var_Independente4=Cambio2011_2016.5
name4='Cambio2011_2016.5'
}

                #2016.5-2018 FORWARD-LOOKING LINEAR#
{
ipca2016.5_2018<-window(ipca, frequency=12, start=c(2016,5), end=c(2018,12))
eipca2016.5_2018<-window(eipca, frequency=12, start=c(2016,5), end=c(2018,12))
hiato2016.5_2018<-window(hiato, frequency=12, start=c(2016,5), end=c(2018,12))
Cambio2016.5_2018<-window(Cambio, frequency=12, start=c(2016,5), end=c(2018,12))

PhillipsF2016.5_2018=dynlm(ipca2016.5_2018~lag(ipca2016.5_2018,-1)+eipca2016.5_2018+lag(hiato2016.5_2018,-1)+Cambio2016.5_2018)
summary(PhillipsF2016.5_2018)
regressao=PhillipsF2016.5_2018

variavel=Cambio2016.5_2018
name='Cambio2016.5_2018'
Var_Independente1=lag(ipca2016.5_2018,-1)
name1='ipca2016.5_2018 Defasado em 1'
Var_Independente2=eipca2016.5_2018
name2='Expectatica da inflação (eipca)'
Var_Independente3=lag(hiato2016.5_2018,-1)
name3='hiato2016.5_2018 Defasado em 1'
Var_Independente4=Cambio2016.5_2018
name4='Cambio2016.5_2018'
}

                #2019-2021 FORWARD-LOOKING LINEAR#
{
ipca2019_2021<-window(ipca, frequency=12, start=c(2019,1), end=c(2021,12))
eipca2019_2021<-window(eipca, frequency=12, start=c(2019,1), end=c(2021,12))
hiato2019_2021<-window(hiato, frequency=12, start=c(2019,1), end=c(2021,12))
Cambio2019_2021<-window(Cambio, frequency=12, start=c(2019,1), end=c(2021,12))

PhillipsF2019_2021=dynlm(ipca2019_2021~lag(ipca2019_2021,-1)+eipca2019_2021+lag(hiato2019_2021,-1)+Cambio2019_2021)
summary(PhillipsF2019_2021)
regressao=PhillipsF2019_2021

variavel=Cambio2019_2021
name='Cambio2019_2021'
Var_Independente1=lag(ipca2019_2021,-1)
name1='ipca2019_2021 Defasado em 1'
Var_Independente2=eipca2019_2021
name2='Expectatica da inflação (eipca)'
Var_Independente3=lag(hiato2019_2021,-1)
name3='hiato2019_2021 Defasado em 1'
Var_Independente4=Cambio2019_2021
name4='Cambio2019_2021'
}














            
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
              #                                                                                                                 #
              #                     P H I L L I P S     B A C K W A R D     L O O K I N G   ( P O T E N C I A L )               #
              #                                                                                                                 #
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#




                # MODELO COMPLETO 2003 - 2021 BACKWARD-LOOKING POTENCIAL #
{
PhillipsBPot=dynlm(lnipca1~lag(lnipca1,-1)+lag(lnipca1,-2)+lag(lnhiato1,-1)+lnCambio1)
summary(PhillipsBPot)
regressao=PhillipsBPot
variavel=lnCambio1
name='lnCambio1'
Var_Independente1=lag(lnipca1,-1)
name1='lnipca1 Defasado em 1'
Var_Independente2=lag(lnipca1,-2)
name2='lnipca1 Defasado em 2'
Var_Independente3=lag(lnhiato1,-1)
name3='lnhiato1 Defasado em 1'
Var_Independente4=lnCambio1
name4='lnCambio1'
}
                #  2003 - 2010 BACKWARD-LOOKING POTENCIAL #

{
  lnipca12003_2010<-window(lnipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  lnhiato12003_2010<-window(lnhiato1, frequency=12, start=c(2003,1), end=c(2010,12))
  lnCambio12003_2010<-window(lnCambio1, frequency=12, start=c(2003,1), end=c(2010,12))
  PhillipsBPot2003_2010=dynlm(lnipca12003_2010~lag(lnipca12003_2010,-1)+lag(lnipca12003_2010,-2)+lag(lnhiato12003_2010,-1)+lnCambio12003_2010)
  summary(PhillipsBPot2003_2010)
  regressao=PhillipsBPot2003_2010
  residuos=residuals(regressao)
  variavel=lnCambio12003_2010
  name='lnCambio12003_2010'
  Var_Independente1=lag(lnipca12003_2010,-1)
  name1='lnipca12003_2010 Defasado em 1'
  Var_Independente2=lag(lnipca12003_2010,-2)
  name2='lnipca12003_2010 Defasado em 2'
  Var_Independente3=lag(lnhiato12003_2010,-1)
  name3='lnhiato12003_2010 Defasado em 1'
  Var_Independente4=lnCambio12003_2010
  name4='lnCambio12003_2010'
} 

                #  2011 - 2016.5 BACKWARD-LOOKING POTENCIAL #

{
  lnipca12011_2016.5<-window(lnipca1, frequency=12, start=c(2011,1), end=c(2016,5))
  lnhiato12011_2016.5<-window(lnhiato1, frequency=12, start=c(2011,1), end=c(2016,5))
  lnCambio12011_2016.5<-window(lnCambio1, frequency=12, start=c(2011,1), end=c(2016,5))
  PhillipsBPot2011_2016.5=dynlm(lnipca12011_2016.5~lag(lnipca12011_2016.5,-1)+lag(lnipca12011_2016.5,-2)+lag(lnhiato12011_2016.5,-1)+lnCambio12011_2016.5)
  summary(PhillipsBPot2011_2016.5)
  regressao=PhillipsBPot2011_2016.5
  variavel=lnCambio12011_2016.5
  name='lnCambio12011_2016.5'
  Var_Independente1=lag(lnipca12011_2016.5,-1)
  name1='lnipca12011_2016.5 Defasado em 1'
  Var_Independente2=lag(lnipca12011_2016.5,-2)
  name2='lnipca12011_2016.5 Defasado em 2'
  Var_Independente3=lag(lnhiato12011_2016.5,-1)
  name3='lnhiato12011_2016.5 Defasado em 1'
  Var_Independente4=lnCambio12011_2016.5
  name4='lnCambio12011_2016.5'
} 
                #  2016.5 - 2018 BACKWARD-LOOKING POTENCIAL #

{
  lnipca12016.5_2018<-window(lnipca1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnhiato12016.5_2018<-window(lnhiato1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnCambio12016.5_2018<-window(lnCambio1, frequency=12, start=c(2016,5), end=c(2018,12))
  PhillipsBPot2016.5_2018=dynlm(lnipca12016.5_2018~lag(lnipca12016.5_2018,-1)+lag(lnipca12016.5_2018,-2)+lag(lnhiato12016.5_2018,-1)+lnCambio12016.5_2018)
  summary(PhillipsBPot2016.5_2018)
  regressao=PhillipsBPot2016.5_2018
  residuos=residuals(regressao)
  variavel=lnCambio12016.5_2018
  name='lnCambio12016.5_2018'
  Var_Independente1=lag(lnipca12016.5_2018,-1)
  name1='lnipca12016.5_2018 Defasado em 1'
  Var_Independente2=lag(lnipca12016.5_2018,-2)
  name2='lnipca12016.5_2018 Defasado em 2'
  Var_Independente3=lag(lnhiato12016.5_2018,-1)
  name3='lnhiato12016.5_2018 Defasado em 1'
  Var_Independente4=lnCambio12016.5_2018
  name4='lnCambio12016.5_2018'
}

                #  2019 - 2021 BACKWARD-LOOKING POTENCIAL #

{
  lnipca12019_2021<-window(lnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  lnhiato12019_2021<-window(lnhiato1, frequency=12, start=c(2019,1), end=c(2021,12))
  lnCambio12019_2021<-window(lnCambio1, frequency=12, start=c(2019,1), end=c(2021,12))
  PhillipsBPot2019_2021=dynlm(lnipca12019_2021~lag(lnipca12019_2021,-1)+lag(lnipca12019_2021,-2)+lag(lnhiato12019_2021,-1)+lnCambio12019_2021)
  summary(PhillipsBPot2019_2021)
  regressao=PhillipsBPot2019_2021
  residuos=residuals(regressao)
  variavel=lnCambio12019_2021
  name='lnCambio12019_2021'
  Var_Independente1=lag(lnipca12019_2021,-1)
  name1='lnipca12019_2021 Defasado em 1'
  Var_Independente2=lag(lnipca12019_2021,-2)
  name2='lnipca12019_2021 Defasado em 2'
  Var_Independente3=lag(lnhiato12019_2021,-1)
  name3='lnhiato12019_2021 Defasado em 1'
  Var_Independente4=lnCambio12019_2021
  name4='lnCambio12016.5_2018'
}
                
                # 2008.8 - 2014.12 BACKWARD-LOOKING POTENCIAL #
{
  lnipca12008.8_2014.12<-window(lnipca1, frequency=12, start=c(2008,8), end=c(2014,12))
  lnhiato12008.8_2014.12<-window(lnhiato1, frequency=12, start=c(2008,8), end=c(2014,12))
  lnCambio12008.8_2014.12<-window(lnCambio1, frequency=12, start=c(2008,8), end=c(2014,12))
  PhillipsBPot2008.8_2014.12=dynlm(lnipca12008.8_2014.12~lag(lnipca12008.8_2014.12,-1)+lag(lnipca12008.8_2014.12,-2)+lag(lnhiato12008.8_2014.12,-1)+lnCambio12008.8_2014.12)
  summary(PhillipsBPot2008.8_2014.12)
  regressao=PhillipsBPot2008.8_2014.12
  variavel=lnCambio12008.8_2014.12
  name='lnCambio12008.8_2014.12'
  Var_Independente1=lag(lnipca12008.8_2014.12,-1)
  name1='lnipca12008.8_2014.12 Defasado em 1'
  Var_Independente2=lag(lnipca12008.8_2014.12,-2)
  name2='lnipca12008.8_2014.12 Defasado em 2'
  Var_Independente3=lag(lnhiato12008.8_2014.12,-1)
  name3='lnhiato12008.8_2014.12 Defasado em 1'
  Var_Independente4=lnCambio12008.8_2014.12
  name4='lnCambio12008.8_2014.12'
}

                  
                  #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                  #                                                                                                                 #
                  #                     P H I L L I P S     F O W A R D      L O O K I N G   ( P O T E N C I A L )                  #
                  #                                                                                                                 #
                  #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#




                # MODELO COMPLETO 2003 - 2021 FORWARD-LOOKING POTENCIAL #
{
  PhillipsFPot=dynlm(lnipca1~lag(lnipca1,-1)+lneipca1+lag(lnhiato1,-1)+lnCambio1)
  summary(PhillipsFPot)
  regressao=PhillipsFPot
  variavel=lnCambio1
  name='lnCambio1'
  Var_Independente1=lag(lnipca1,-1)
  name1='lnipca1 Defasado em 1'
  Var_Independente2=lneipca1
  name2='lneipca1 Defasado em 2'
  Var_Independente3=lag(lnhiato1,-1)
  name3='lnhiato1 Defasado em 1'
  Var_Independente4=lnCambio1
  name4='lnCambio1'
}

                #  2003 - 2010 FORWARD-LOOKING POTENCIAL #

{
  lnipca12003_2010<-window(lnipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  lneipca12003_2010<-window(lneipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  lnhiato12003_2010<-window(lnhiato1, frequency=12, start=c(2003,1), end=c(2010,12))
  lnCambio12003_2010<-window(lnCambio1, frequency=12, start=c(2003,1), end=c(2010,12))
  PhillipsFPot2003_2010=dynlm(lnipca12003_2010~lag(lnipca12003_2010,-1)+lneipca12003_2010+lag(lnhiato12003_2010,-1)+lnCambio12003_2010)
  summary(PhillipsFPot2003_2010)
  regressao=PhillipsFPot2003_2010
  variavel=lnCambio12003_2010
  name='lnCambio12003_2010'
  Var_Independente1=lag(lnipca12003_2010,-1)
  name1='lnipca12003_2010 Defasado em 1'
  Var_Independente2=lneipca12003_2010
  name2='lneipca12003_2010 Defasado em 2'
  Var_Independente3=lag(lnhiato12003_2010,-1)
  name3='lnhiato12003_2010 Defasado em 1'
  Var_Independente4=lnCambio12003_2010
  name4='lnCambio12003_2010'
} 

                #  2011 - 2016.5 FORWARD-LOOKING POTENCIAL #

{
  lnipca12011_2016.5<-window(lnipca1, frequency=12, start=c(2011,1), end=c(2016,5))
  lneipca12011_2016.5<-window(lneipca1, frequency=12, start=c(2011,1), end=c(2016,5))
  lnhiato12011_2016.5<-window(lnhiato1, frequency=12, start=c(2011,1), end=c(2016,5))
  lnCambio12011_2016.5<-window(lnCambio1, frequency=12, start=c(2011,1), end=c(2016,5))
  PhillipsFPot2011_2016.5=dynlm(lnipca12011_2016.5~lag(lnipca12011_2016.5,-1)+lneipca12011_2016.5+lag(lnhiato12011_2016.5,-1)+lnCambio12011_2016.5)
  summary(PhillipsFPot2011_2016.5)
  regressao=PhillipsFPot2011_2016.5
  variavel=lnCambio12011_2016.5
  name='lnCambio12011_2016.5'
  Var_Independente1=lag(lnipca12011_2016.5,-1)
  name1='lnipca12011_2016.5 Defasado em 1'
  Var_Independente2=lneipca12011_2016.5
  name2='lneipca12011_2016.5 Defasado em 2'
  Var_Independente3=lag(lnhiato12011_2016.5,-1)
  name3='lnhiato12011_2016.5 Defasado em 1'
  Var_Independente4=lnCambio12011_2016.5
  name4='lnCambio12011_2016.5'
} 
                #  2016.5 - 2018 FORWARD-LOOKING POTENCIAL #

{
  lnipca12016.5_2018<-window(lnipca1, frequency=12, start=c(2016,5), end=c(2018,12))
  lneipca12016.5_2018<-window(lneipca1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnhiato12016.5_2018<-window(lnhiato1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnCambio12016.5_2018<-window(lnCambio1, frequency=12, start=c(2016,5), end=c(2018,12))
  PhillipsFPot2016.5_2018=dynlm(lnipca12016.5_2018~lag(lnipca12016.5_2018,-1)+lneipca12016.5_2018+lag(lnhiato12016.5_2018,-1)+lnCambio12016.5_2018)
  summary(PhillipsFPot2016.5_2018)
  regressao=PhillipsFPot2016.5_2018
  variavel=lnCambio12016.5_2018
  name='lnCambio12016.5_2018'
  Var_Independente1=lag(lnipca12016.5_2018,-1)
  name1='lnipca12016.5_2018 Defasado em 1'
  Var_Independente2=lneipca12016.5_2018
  name2='lneipca12016.5_2018 Defasado em 2'
  Var_Independente3=lag(lnhiato12016.5_2018,-1)
  name3='lnhiato12016.5_2018 Defasado em 1'
  Var_Independente4=lnCambio12016.5_2018
  name4='lnCambio12016.5_2018'
}

                #  2019 - 2021 FORWARD-LOOKING POTENCIAL #

{
  lnipca12019_2021<-window(lnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  lneipca12019_2021<-window(lneipca1, frequency=12, start=c(2016,5), end=c(2021,12))
  lnhiato12019_2021<-window(lnhiato1, frequency=12, start=c(2016,5), end=c(2021,12))
  lnCambio12019_2021<-window(lnCambio1, frequency=12, start=c(2016,5), end=c(2021,12))
  PhillipsFPot2019_2021=dynlm(lnipca12019_2021~lag(lnipca12019_2021,-1)+lneipca12019_2021+lag(lnhiato12019_2021,-1)+lnCambio12019_2021)
  summary(PhillipsFPot2019_2021)
  regressao=PhillipsFPot2019_2021
  variavel=lnCambio12019_2021
  name='lnCambio12019_2021'
  Var_Independente1=lag(lnipca12019_2021,-1)
  name1='lnipca12019_2021 Defasado em 1'
  Var_Independente2=lneipca12019_2021
  name2='lneipca12019_2021 Defasado em 2'
  Var_Independente3=lag(lnhiato12019_2021,-1)
  name3='lnhiato12019_2021 Defasado em 1'
  Var_Independente4=lnCambio12019_2021
  name4='lnCambio12019_2021'
}
                # 2008.8_2014.12 FORWARD-LOOKING POTENCIAL # (BACKWARD-LOOKING MELHOR)

{
lnipca12008.8_2014.12<-window(lnipca1, frequency=12, start=c(2008,8), end=c(2014,12))
lneipca12008.8_2014.12<-window(lneipca1, frequency=12, star=c(2008,8), end=c(2014,12))
lnhiato12008.8_2014.12<-window(lnhiato1, frequency=12, start=c(2008,8), end=c(2014,12))
lnCambio12008.8_2014.12<-window(lnCambio1, frequency=12, start=c(2008,8), end=c(2014,12))
PhillipsFPot2008.8_2014.12=dynlm(lnipca12008.8_2014.12~lag(lnipca12008.8_2014.12,-1)+lneipca12008.8_2014.12+lag(lnhiato12008.8_2014.12,-1)+lnCambio12008.8_2014.12)
summary(PhillipsFPot2008.8_2014.12)
regressao=PhillipsFPot2008.8_2014.12
variavel=lnCambio12008.8_2014.12
name='lnCambio12008.8_2014.12'
Var_Independente1=lag(lnipca12008.8_2014.12,-1)
name1='lnipca12008.8_2014.12 Defasado em 1'
Var_Independente2=lag(lnipca12008.8_2014.12,-2)
name2='lnipca12008.8_2014.12 Defasado em 2'
Var_Independente3=lag(lnhiato12008.8_2014.12,-1)
name3='lnhiato12008.8_2014.12 Defasado em 1'
Var_Independente4=lnCambio12008.8_2014.12
name4='lnCambio12008.8_2014.12'
}

              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
              #                                                                                                                 #
              #           P H I L L I P S     B A C K W A R D     L O O K I N G   ( L I N E A R - 1a DIFERENCA )                #
              #                                                                                                                 #
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

#MODELO COMPLETO  2003-2021            
  {
                  Dipca=diff(ipca);plot(Dipca)
                  Dhiato=diff(hiato);plot(Dhiato)
                  DCambio=diff(Cambio);plot(DCambio)
                  
                  PhillipsB=dynlm(Dipca~lag(Dipca,-1)+lag(Dipca,-2)+lag(Dhiato,-1)+DCambio)
                  summary(PhillipsB)
                  regressao=PhillipsB
                  variavel=DCambio
                  name='DCambio'
                  Var_Independente1=lag(Dipca,-1)
                  name1='Dipca Defasado em 1'
                  Var_Independente2=lag(Dipca,-2)
                  name2='Dipca Defasado em 2'
                  Var_Independente3=lag(Dhiato,-1)
                  name3='Dhiato Defasado em 1'
                  Var_Independente4=DCambio
                  name4='DCambio'
                  residuos=residuals(regressao)
  }
#2003-2010
  {
    Dipca=diff(ipca)
    Dhiato=diff(hiato)
    DCambio=diff(Cambio)
    
    Dipca2003_2010<-window(Dipca, frequency=12, start=c(2003,1), end=c(2010,12))
    Dhiato2003_2010<-window(Dhiato, frequency=12, start=c(2003,1), end=c(2010,12))
    DCambio2003_2010<-window(DCambio, frequency=12, start=c(2003,1), end=c(2010,12))
    
    PhillipsBLD2003_2010=dynlm(Dipca2003_2010~lag(Dipca2003_2010,-1)+lag(Dipca2003_2010,-2)+lag(Dhiato2003_2010,-1)+DCambio2003_2010)
    summary(PhillipsBLD2003_2010)
    regressao=PhillipsBLD2003_2010
    
    variavel=DCambio2003_2010
    name='DCambio2003_2010'
    Var_Independente1=lag(Dipca2003_2010,-1)
    name1='Dipca2003_2010 Defasado em 1'
    Var_Independente2=lag(Dipca2003_2010,-2)
    name2='Dipca2003_2010 Defasado em 2'
    Var_Independente3=lag(Dhiato2003_2010,-1)
    name3='Dhiato2003_2010 Defasado em 1'
    Var_Independente4=DCambio2003_2010
    name4='DCambio2003_2010'
    residuos=residuals(regressao)
  }
#2011-2016.5
  {
    Dipca=diff(ipca)
    Dhiato=diff(hiato)
    DCambio=diff(Cambio)
    
    Dipca2011_2016.8<-window(Dipca, frequency=12, start=c(2011,1), end=c(2016,5))
    Dhiato2011_2016.8<-window(Dhiato, frequency=12, start=c(2011,1), end=c(2016,5))
    DCambio2011_2016.8<-window(DCambio, frequency=12, start=c(2011,1), end=c(2016,5))
    
    Var_Dependente=Dipca2011_2016.8
    name0='Dipca2011_2016.8'
    Var_Independente1=lag(Dipca2011_2016.8,-1)
    name1='Dipca2011_2016.8 Defasado em 1'
    Var_Independente2=lag(Dipca2011_2016.8,-2)
    name2='Dipca2011_2016.8 Defasado em 2'
    Var_Independente3=lag(Dhiato2011_2016.8,-1)
    name3='Dhiato2011_2016.8 Defasado em 1'
    Var_Independente4=DCambio2011_2016.8
    name4='DCambio2011_2016.8' 
    
    PhillipsBLD2011_2016.8=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBLD2011_2016.8)
    regressao=PhillipsBLD2011_2016.8
    residuos=residuals(regressao)
  }
#2016.6-2018
  {
    Dipca=diff(ipca)
    Dhiato=diff(hiato)
    DCambio=diff(Cambio)
    
    Dipca2016.9_2018<-window(Dipca, frequency=12, start=c(2016,6), end=c(2018,12))
    Dhiato2016.9_2018<-window(Dhiato, frequency=12, start=c(2016,6), end=c(2018,12))
    DCambio2016.9_2018<-window(DCambio, frequency=12, start=c(2016,6), end=c(2018,12))
    
    Var_Dependente=Dipca2016.9_2018
    name0='Dipca2016.9_2018'
    Var_Independente1=lag(Dipca2016.9_2018,-1)
    name1='Dipca2016.9_2018 Defasado em 1'
    Var_Independente2=lag(Dipca2016.9_2018,-2)
    name2='Dipca2016.9_2018 Defasado em 2'
    Var_Independente3=lag(Dhiato2016.9_2018,-1)
    name3='Dhiato2016.9_2018 Defasado em 1'
    Var_Independente4=DCambio2016.9_2018
    name4='DCambio2016.9_2018' 
    
    PhillipsBLD2016.9_2018=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBLD2016.9_2018)
    regressao=PhillipsBLD2016.9_2018
    residuos=residuals(regressao)
  }
#2019-2021
  {
    Dipca=diff(ipca)
    Dhiato=diff(hiato)
    DCambio=diff(Cambio)
    
    Dipca2019_2021<-window(Dipca, frequency=12, start=c(2019,1), end=c(2021,12))
    Dhiato2019_2021<-window(Dhiato, frequency=12, start=c(2019,1), end=c(2021,12))
    DCambio2019_2021<-window(DCambio, frequency=12, start=c(2019,1), end=c(2021,12))
    
    Var_Dependente=Dipca2019_2021
    name0='Dipca2019_2021'
    Var_Independente1=lag(Dipca2019_2021,-1)
    name1='Dipca2019_2021 Defasado em 1'
    Var_Independente2=lag(Dipca2019_2021,-2)
    name2='Dipca2019_2021 Defasado em 2'
    Var_Independente3=lag(Dhiato2019_2021,-1)
    name3='Dhiato2019_2021 Defasado em 1'
    Var_Independente4=DCambio2019_2021
    name4='DCambio2019_2021' 
    
    PhillipsBLD2019_2021=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBLD2019_2021)
    regressao=PhillipsBLD2019_2021 
    residuos=residuals(regressao)
  }
  

              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
              #                                                                                                                 #
              #           P H I L L I P S     B A C K W A R D     L O O K I N G   ( P O T E N C I A L - 1a DIFERENCA )          #
              #                                                                                                                 #
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

  #MODELO COMPLETO 2003-2021
{
  Dlnipca1=diff(lnipca1);plot(Dlnipca1)
  Dlnhiato1=diff(lnhiato1);plot(Dlnhiato1)
  DlnCambio1=diff(lnCambio1);plot(DlnCambio1)
  PhillipsBPotDiff=dynlm(Dlnipca1~lag(Dlnipca1,-1)+lag(Dlnipca1,-2)+lag(Dlnhiato1,-1)+DlnCambio1)
  summary(PhillipsBPotDiff)
  regressao=PhillipsBPotDiff
  variavel=DlnCambio1
  name='DlnCambio1'
  Var_Dependente=Dlnipca1
  Var_Independente1=lag(Dlnipca1,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=lag(Dlnipca1,-2)
  name2='Dlnipca1 Defasado em 2'
  Var_Independente3=lag(Dlnhiato1,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1
  name4='DlnCambio1'
  residuos=residuals(regressao)
}
  #2003-2010
  {
    Dlnipca1=diff(lnipca1)
    Dlnhiato1=diff(lnhiato1)
    DlnCambio1=diff(lnCambio1)
    
    Dlnipca12003_2010<-window(Dlnipca1, frequency=12, start=c(2003,1), end=c(2010,12))
    Dlnhiato12003_2010<-window(Dlnhiato1, frequency=12, start=c(2003,1), end=c(2010,12))
    DlnCambio12003_2010<-window(DlnCambio1, frequency=12, start=c(2003,1), end=c(2010,12))
    
    Var_Dependente=Dlnipca12003_2010
    name0='Dlnipca12003_2010'
    Var_Independente1=lag(Dlnipca12003_2010,-1)
    name1='Dlnipca12003_2010 Defasado em 1'
    Var_Independente2=lag(Dlnipca12003_2010,-2)
    name2='Dlnipca12003_2010 Defasado em 2'
    Var_Independente3=lag(Dlnhiato12003_2010,-1)
    name3='Dlnhiato12003_2010 Defasado em 1'
    Var_Independente4=DlnCambio12003_2010
    name4='DlnCambio12003_2010' 
    
    PhillipsBPD2003_2010=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBPD2003_2010)
    regressao=PhillipsBPD2003_2010
    residuos=residuals(regressao)
  }
  #2011-2016.5
  {
    Dlnipca1=diff(lnipca1)
    Dlnhiato1=diff(lnhiato1)
    DlnCambio1=diff(lnCambio1)
    
    Dlnipca12011_2016.8<-window(Dlnipca1, frequency=12, start=c(2011,1), end=c(2016,5))
    Dlnhiato12011_2016.8<-window(Dlnhiato1, frequency=12, start=c(2011,1), end=c(2016,5))
    DlnCambio12011_2016.8<-window(DlnCambio1, frequency=12, start=c(2011,1), end=c(2016,5))
    
    Var_Dependente=Dlnipca12011_2016.8
    name0='Dlnipca12011_2016.8'
    Var_Independente1=lag(Dlnipca12011_2016.8,-1)
    name1='Dlnipca12011_2016.8 Defasado em 1'
    Var_Independente2=lag(Dlnipca12011_2016.8,-2)
    name2='Dlnipca12011_2016.8 Defasado em 2'
    Var_Independente3=lag(Dlnhiato12011_2016.8,-1)
    name3='Dlnhiato12011_2016.8 Defasado em 1'
    Var_Independente4=DlnCambio12011_2016.8
    name4='DlnCambio12011_2016.8' 
    
    PhillipsBPD2011_2016.8=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBPD2011_2016.8)
    regressao=PhillipsBPD2011_2016.8
    residuos=residuals(regressao)
  }
  #2016.6-2018
  {
    Dlnipca1=diff(lnipca1)
    Dlnhiato1=diff(lnhiato1)
    DlnCambio1=diff(lnCambio1)
    
    Dlnipca12016.9_2018<-window(Dlnipca1, frequency=12, start=c(2016,6), end=c(2018,12))
    Dlnhiato12016.9_2018<-window(Dlnhiato1, frequency=12, start=c(2016,6), end=c(2018,12))
    DlnCambio12016.9_2018<-window(DlnCambio1, frequency=12, start=c(2016,6), end=c(2018,12))
    
    Var_Dependente=Dlnipca12016.9_2018
    name0='Dlnipca12016.9_2018'
    Var_Independente1=lag(Dlnipca12016.9_2018,-1)
    name1='Dlnipca12016.9_2018 Defasado em 1'
    Var_Independente2=lag(Dlnipca12016.9_2018,-2)
    name2='Dlnipca12016.9_2018 Defasado em 2'
    Var_Independente3=lag(Dlnhiato12016.9_2018,-1)
    name3='Dlnhiato12016.9_2018 Defasado em 1'
    Var_Independente4=DlnCambio12016.9_2018
    name4='DlnCambio12016.9_2018' 
    
    PhillipsBPD2016.9_2018=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBPD2016.9_2018)
    regressao=PhillipsBPD2016.9_2018
    residuos=residuals(regressao)
  }
  #2019-2021
  {
    Dlnipca1=diff(lnipca1)
    Dlnhiato1=diff(lnhiato1)
    DlnCambio1=diff(lnCambio1)
    
    Dlnipca12019_2021<-window(Dlnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
    Dlnhiato12019_2021<-window(Dlnhiato1, frequency=12, start=c(2019,1), end=c(2021,12))
    DlnCambio12019_2021<-window(DlnCambio1, frequency=12, start=c(2019,1), end=c(2021,12))
    
    Var_Dependente=Dlnipca12019_2021
    name0='Dlnipca12019_2021'
    Var_Independente1=lag(Dlnipca12019_2021,-1)
    name1='Dlnipca12019_2021 Defasado em 1'
    Var_Independente2=lag(Dlnipca12019_2021,-2)
    name2='Dlnipca12019_2021 Defasado em 2'
    Var_Independente3=lag(Dlnhiato12019_2021,-1)
    name3='Dlnhiato12019_2021 Defasado em 1'
    Var_Independente4=DlnCambio12019_2021
    name4='DlnCambio12019_2021' 
    
    PhillipsBPD2019_2021=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
    summary(PhillipsBPD2019_2021)
    regressao=PhillipsBPD2019_2021  
    residuos=residuals(regressao)
  }


              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
              #                                                                                                                 #
              #           P H I L L I P S     F O R W A R D    L O O K I N G   ( L I N E A R - 1a DIFERENCA )                   #
              #                                                                                                                 #
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

#MODELO COMPLETO 2003-2021
{
    Dipca=diff(ipca);plot(Dipca)
    Dhiato=diff(hiato);plot(Dhiato)
    DCambio=diff(Cambio);plot(DCambio)
    Deipca=diff(eipca);plot(Deipca)
    PhillipsFLinDiff=dynlm(Dipca~lag(Dipca,-1)+Deipca+lag(Dhiato,-1)+DCambio)
    summary(PhillipsFLinDiff)
    regressao=PhillipsFLinDiff

    Var_Dependente=Dipca
    name0='Dipca'
    Var_Independente1=Deipca
    name1='Deipca'
    Var_Independente2=Dhiato
    name2='Dhiato'
    Var_Independente3=DCambio
    name3='DCambio'
    residuos=residuals(regressao)
  }
#2003-2010
{
  Dipca=diff(ipca)
  Dhiato=diff(hiato)
  DCambio=diff(Cambio)
  Deipca=diff(eipca)
  
  Dipca2003_2010<-window(Dipca, frequency=12, start=c(2003,1), end=c(2010,12))
  Deipca2003_2010<-window(Deipca, frequency=12, start=c(2003,1), end=c(2010,12))
  Dhiato2003_2010<-window(Dhiato, frequency=12, start=c(2003,1), end=c(2010,12))
  DCambio2003_2010<-window(DCambio, frequency=12, start=c(2003,1), end=c(2010,12))
  
  Var_Dependente=Dipca2003_2010
  name0='Dipca2003_2010'
  Var_Independente1=lag(Dipca2003_2010,-1)
  name1='Dipca2003_2010 Defasado em 1'
  Var_Independente2=Deipca2003_2010
  name2='Deipca2003_2010'
  Var_Independente3=lag(Dhiato2003_2010,-1)
  name3='Dhiato2003_2010 Defasado em 1'
  Var_Independente4=DCambio2003_2010
  name4='DCambio2003_2010' 
  
  PhillipsBLD2003_2010=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
  summary(PhillipsBLD2003_2010)
  regressao=PhillipsBLD2003_2010
  residuos=residuals(regressao)
}
#2011-2016.5
{
  Dipca=diff(ipca)
  Dhiato=diff(hiato)
  DCambio=diff(Cambio)
  Deipca=diff(eipca)
  
  Dipca2011_2016.8<-window(Dipca, frequency=12, start=c(2011,1), end=c(2016,5))
  Deipca2011_2016.8<-window(Deipca, frequency=12, start=c(2011,1), end=c(2016,5))
  Dhiato2011_2016.8<-window(Dhiato, frequency=12, start=c(2011,1), end=c(2016,5))
  DCambio2011_2016.8<-window(DCambio, frequency=12, start=c(2011,1), end=c(2016,5))
  
  Var_Dependente=Dipca2011_2016.8
  name0='Dipca2011_2016.8'
  Var_Independente1=lag(Dipca2011_2016.8,-1)
  name1='Dipca2011_2016.8 Defasado em 1'
  Var_Independente2=Deipca2011_2016.8
  name2='Deipca2011_2016.8'
  Var_Independente3=lag(Dhiato2011_2016.8,-1)
  name3='Dhiato2011_2016.8 Defasado em 1'
  Var_Independente4=DCambio2011_2016.8
  name4='DCambio2011_2016.8' 
  
  PhillipsBLD2011_2016.8=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
  summary(PhillipsBLD2011_2016.8)
  regressao=PhillipsBLD2011_2016.8
  residuos=residuals(regressao)
}
#2016.6-2018
{
  Dipca=diff(ipca)
  Dhiato=diff(hiato)
  DCambio=diff(Cambio)
  Deipca=diff(eipca)
  
  Dipca2016.9_2018<-window(Dipca, frequency=12, start=c(2016,6), end=c(2018,12))
  Deipca2016.9_2018<-window(Deipca, frequency=12, start=c(2016,6), end=c(2018,12))
  Dhiato2016.9_2018<-window(Dhiato, frequency=12, start=c(2016,6), end=c(2018,12))
  DCambio2016.9_2018<-window(DCambio, frequency=12, start=c(2016,6), end=c(2018,12))
  
  Var_Dependente=Dipca2016.9_2018
  name0='Dipca2016.9_2018'
  Var_Independente1=lag(Dipca2016.9_2018,-1)
  name1='Dipca2016.9_2018 Defasado em 1'
  Var_Independente2=Deipca2016.9_2018
  name2='Deipca2016.9_2018'
  Var_Independente3=lag(Dhiato2016.9_2018,-1)
  name3='Dhiato2016.9_2018 Defasado em 1'
  Var_Independente4=DCambio2016.9_2018
  name4='DCambio2016.9_2018' 
  
  PhillipsBLD2016.9_2018=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
  summary(PhillipsBLD2016.9_2018)
  regressao=PhillipsBLD2016.9_2018
  residuos=residuals(regressao)
}
#2019-2021
{
  Dipca=diff(ipca)
  Dhiato=diff(hiato)
  DCambio=diff(Cambio)
  Deipca=diff(eipca)
  
  Dipca2019_2021<-window(Dipca, frequency=12, start=c(2019,1), end=c(2021,12))
  Deipca2019_2021<-window(Deipca, frequency=12, start=c(2019,1), end=c(2021,12))
  Dhiato2019_2021<-window(Dhiato, frequency=12, start=c(2019,1), end=c(2021,12))
  DCambio2019_2021<-window(DCambio, frequency=12, start=c(2019,1), end=c(2021,12))
  
  Var_Dependente=Dipca2019_2021
  name0='Dipca2019_2021'
  Var_Independente1=lag(Dipca2019_2021,-1)
  name1='Dipca2019_2021 Defasado em 1'
  Var_Independente2=Deipca2019_2021
  name2='Deipca2019_2021'
  Var_Independente3=lag(Dhiato2019_2021,-1)
  name3='Dhiato2019_2021 Defasado em 1'
  Var_Independente4=DCambio2019_2021
  name4='DCambio2019_2021' 
  
  PhillipsBLD2019_2021=dynlm(Var_Dependente~Var_Independente1+Var_Independente2+Var_Independente3+Var_Independente4)
  summary(PhillipsBLD2019_2021)
  regressao=PhillipsBLD2019_2021
  residuos=residuals(regressao)
}



            #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
            #                                                                                                                 #
            #           P H I L L I P S     F O R W A R D     L O O K I N G   ( P O T E N C I A L - 1a DIFERENCA )            #
            #                                                                                                                 #
            #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

  #MODELO COMPLETO
{
  Dlnipca1=diff(lnipca1);plot(lnipca1)
  Dlnhiato1=diff(lnhiato1);plot(lnhiato1)
  DlnCambio1=diff(lnCambio1);plot(lnCambio1)
  Dlneipca1=diff(lneipca1);plot(lneipca1)
  
  PhillipsFPotDiff=dynlm(Dlnipca1~lag(Dlnipca1,-1)+Dlneipca1+lag(Dlnhiato1,-1)+DlnCambio1)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  variavel=DlnCambio1
  name='DlnCambio1'
  Var_Dependente=Dlnipca1
  Var_Independente1=lag(Dlnipca1,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1
  name4='DlnCambio1'
  residuos=residuals(regressao)
}
#2003-2010
{
  Dlnipca1=diff(lnipca1);plot(lnipca1)
  Dlnhiato1=diff(lnhiato1);plot(lnhiato1)
  DlnCambio1=diff(lnCambio1);plot(lnCambio1)
  Dlneipca1=diff(lneipca1);plot(lneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2003,1), end=c(2010,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2003,1), end=c(2010,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+lag(Dlnhiato1aux,-1)+DlnCambio1aux)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao)

}
#2011-2016.5
{
  Dlnipca1=diff(lnipca1);plot(lnipca1)
  Dlnhiato1=diff(lnhiato1);plot(lnhiato1)
  DlnCambio1=diff(lnCambio1);plot(lnCambio1)
  Dlneipca1=diff(lneipca1);plot(lneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2011,1), end=c(2016,5))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2011,1), end=c(2016,5))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2011,1), end=c(2016,5))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2011,1), end=c(2016,5))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+lag(Dlnhiato1aux,-1)+DlnCambio1aux)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao)
}
#2016.6-2018
{
  Dlnipca1=diff(lnipca1);plot(lnipca1)
  Dlnhiato1=diff(lnhiato1);plot(lnhiato1)
  DlnCambio1=diff(lnCambio1);plot(lnCambio1)
  Dlneipca1=diff(lneipca1);plot(lneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2016,6), end=c(2018,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2016,6), end=c(2018,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2016,6), end=c(2018,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2016,6), end=c(2018,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+lag(Dlnhiato1aux,-1)+DlnCambio1aux)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao)
}
#2019-2021
{
  Dlnipca1=diff(lnipca1);plot(Dlnipca1)
  Dlnhiato1=diff(lnhiato1);plot(Dlnhiato1)
  DlnCambio1=diff(lnCambio1);plot(DlnCambio1)
  Dlneipca1=diff(lneipca1);plot(Dlneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2019,1), end=c(2021,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2019,1), end=c(2021,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+lag(Dlnhiato1aux,-1)+DlnCambio1aux)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao)
}







              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
              #                                                                           #
              #                           TESTES MELHORES MODELOS                         #     
              #                                                                           #
              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
#FORWARD
{
#MODELO COMPLETO 2003-2021 TUDO OK MENOS AS VARIÁVEIS NO TESTE DE RAIZ  UNITARIA (RESIDUOS OK)
{
  Dlnipca1=diff(lnipca1);plot(Dlnipca1)
  Dlnhiato1=diff(lnhiato1);plot(Dlnhiato1)
  DlnCambio1=diff(lnCambio1);plot(DlnCambio1)
  Dlneipca1=diff(lneipca1);plot(Dlneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2019,1), end=c(2021,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2019,1), end=c(2021,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+lag(Dlnipca1aux,-2)+Dlneipca1aux+Dlnhiato1aux+lag(Dlnhiato1aux,-1)+lag(DlnCambio1aux,-2))
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=lag(DlnCambio1aux,-2)
  name4='DlnCambio1'
  residuos=residuals(regressao)
}
#2003-2010
{
  Dlnipca1=diff(lnipca1);plot(Dlnipca1)
  Dlnhiato1=diff(lnhiato1);plot(Dlnhiato1)
  DlnCambio1=diff(lnCambio1);plot(DlnCambio1)
  Dlneipca1=diff(lneipca1);plot(Dlneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2003,1), end=c(2010,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2003,1), end=c(2010,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2003,1), end=c(2010,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+lag(Dlnhiato1aux,-1)+DlnCambio1aux)
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao) ;plot(residuos)
}
#2011-2016.5
{
  lnipca12016.5_2018<-window(lnipca1, frequency=12, start=c(2016,5), end=c(2018,12))
  lneipca12016.5_2018<-window(lneipca1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnhiato12016.5_2018<-window(lnhiato1, frequency=12, start=c(2016,5), end=c(2018,12))
  lnCambio12016.5_2018<-window(lnCambio1, frequency=12, start=c(2016,5), end=c(2018,12))
  PhillipsFPot2016.5_2018=dynlm(lnipca12016.5_2018~lag(lnipca12016.5_2018,-1)+lneipca12016.5_2018+lag(lnhiato12016.5_2018,-1)+lnCambio12016.5_2018+lag(lnCambio12016.5_2018,-2))
  summary(PhillipsFPot2016.5_2018)
  regressao=PhillipsFPot2016.5_2018
  variavel=lnCambio12016.5_2018
  name='lnCambio12016.5_2018'
  Var_Independente1=lag(lnipca12016.5_2018,-1)
  name1='lnipca12016.5_2018 Defasado em 1'
  Var_Independente2=lneipca12016.5_2018
  name2='lneipca12016.5_2018 Defasado em 2'
  Var_Independente3=lag(lnhiato12016.5_2018,-1)
  name3='lnhiato12016.5_2018 Defasado em 1'
  Var_Independente4=lnCambio12016.5_2018
  name4='lnCambio12016.5_2018'
  dickey1<-ur.df(residuos, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}
#2019-2021
{
  Dlnipca1=diff(lnipca1)
  Dlnhiato1=diff(lnhiato1)
  DlnCambio1=diff(lnCambio1)
  Dlneipca1=diff(lneipca1)
  
  Dlnipca1aux<-window(Dlnipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlneipca1aux<-window(Dlneipca1, frequency=12, start=c(2019,1), end=c(2021,12))
  Dlnhiato1aux<-window(Dlnhiato1, frequency=12, start=c(2019,1), end=c(2021,12))
  DlnCambio1aux<-window(DlnCambio1, frequency=12, start=c(2019,1), end=c(2021,12))
  
  PhillipsFPotDiff=dynlm(Dlnipca1aux~lag(Dlnipca1aux,-1)+Dlneipca1aux+Dlnhiato1aux+DlnCambio1aux+lag(DlnCambio1aux,-2))
  summary(PhillipsFPotDiff)
  regressao=PhillipsFPotDiff
  Var_Dependente=Dlnipca1aux
  name0='Dlnipca1aux'
  Var_Independente1=lag(Dlnipca1aux,-1)
  name1='Dlnipca1 Defasado em 1'
  Var_Independente2=Dlneipca1aux
  name2='Dlneipca1'
  Var_Independente3=lag(Dlnhiato1aux,-1)
  name3='Dlnhiato1 Defasado em 1'
  Var_Independente4=DlnCambio1aux
  name4='DlnCambio1'
  residuos=residuals(regressao)
  dickey1<-ur.df(residuos, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}
}

#BACKWARD
{
  
}

            #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
            #                                                                                    #
            #                               TESTES ECONOMETRICOS                                 #
            #                                                                                    #
            #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#


regressao
variavel

residuos<-residuals(regressao)
residuos  
plot(residuos, col="violetred1")  
plot(residuos, type="l", col="dodgerblue3", xaxp=c(2003,2021,18)) 
abline(h=0, col="violetred1", lw=3)
plot(variavel, residuos, xlab=name)
#RESIDUOS VS VARIAVEL INDEPENDENTE 1
plot(Var_Independente1, residuos, col="dodgerblue3", xlab=name1, main="RESIDUOS CONTRA VARIAVEL INDEPENDENTE 1")
abline(h=0, col="violetred1", lw=3)
#RESIDUOS VS VARIAVEL INDEPENDENTE 2
plot(Var_Independente2, residuos, col="dodgerblue3", xlab=name2, main="RESIDUOS CONTRA VARIAVEL INDEPENDENTE 2")
abline(h=0, col="violetred1", lw=3)
#RESIDUOS VS VARIAVEL INDEPENDENTE 3
plot(Var_Independente3, residuos, col="dodgerblue3", xlab=name3, main="RESIDUOS CONTRA VARIAVEL INDEPENDENTE 3")
abline(h=0, col="violetred1", lw=3)
#RESIDUOS VS VARIAVEL INDEPENDENTE 4
plot(Var_Independente4, residuos, col="dodgerblue3", xlab=name4, main="RESIDUOS CONTRA VARIAVEL INDEPENDENTE 4")
abline(h=0, col="violetred1", lw=3)

        
                          #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                          #                                                #
                          #              TESTES HOMOCEDASTICIDADE          #
                          #                                                #
                          #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
# TESTE DE PARK(PG383)

residuosP=log((residuos)^2);residuosP
Park=dynlm(residuosP~log(variavel))
summary(Park)

#Teste de Glejser(pg384)
residuosG=abs(residuos)
Glejser1=dynlm(residuosG~variavel)
summary(Glejser1)

RaizSelic=sqrt(variavel)
Glejser2=dynlm(residuosG~RaizSelic)
summary(Glejser2)

#Teste de Goldfeld-Quandt (pg.386)
#install.packages("lmtest")
gqtest(regressao, fraction=15, alternative = "greater")  #H0: Heteroscedasticidade não presente / Ha: Heteroscedasticidade presente

#Teste de Breusch-Pagan-Godfrey (pg389)
bptest(regressao)

#Teste de White (pg391)
#install.packages("whitestrap")

white_test(regressao) #H0 = variância constante(homocedastico)  H1= (heterocedastico)



                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                        #                                                         #
                        #                 AUTO CORRELACAO SERIAL                  #
                        #                                                         #
                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

{
#teste de durbin-watson (pg435-436)

dwtest(regressao)           #pontos criticos 0 ------  2   -------- 4

#teste de breusch-godfrey (pg439)
bgtest(regressao)
bgtest(regressao, order=2)
bgtest(regressao, order=4)

#Teste de ARCH (pg450)
#install.packages("FinTS")
ArchTest(residuos, lags=2)
ArchTest(residuos, lags=4)   # PROBABILIDADE O MODELO NAO TER AUTOCORRELACAO E NAO TER HOMOCEDASTICIDADE
}
  
                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                        #                                                         #
                        #                    ESTACIONARIDADE                      #
                        #                                                         #
                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

{
#Correlograma~~~~
acf(residuos, lag=36, col="violetred1", lwd=5, main="ACF com 36 Defasagem", ylab= "ACF", xlab="Defasagem")
Box.test(residuos, lag=12, type="Box-Pierce")
}

                              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                              #                                              #
                              #            TESTE DE RAIZ UNITARIA            #
                              #                                              #
                              #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

{
par(mfrow=c(2,3))
plot(Var_Dependente, ylab=name0)
plot(Var_Independente1, ylab=name1)
plot(Var_Independente2, ylab=name2)
plot(Var_Independente3, ylab=name3)
plot(Var_Independente4, ylab=name4)

par(mfrow=c(2,2))
acf(Var_Dependente, lag=36, col="violetred1", lwd=5, main=name0, ylab= "ACF", xlab="Defasagem")
acf(Var_Independente1, lag=36, col="violetred1", lwd=5, main=name1, ylab= "ACF", xlab="Defasagem")
acf(Var_Independente2, lag=36, col="violetred1", lwd=5, main=name2, ylab= "ACF", xlab="Defasagem")
acf(Var_Independente3, lag=36, col="violetred1", lwd=5, main=name3, ylab= "ACF", xlab="Defasagem")
acf(Var_Independente4, lag=36, col="violetred1", lwd=5, main=name4, ylab= "ACF", xlab="Defasagem")

acf(residuos, lag=36, col="violetred1", lwd=5, main="Residuos", ylab= "ACF", xlab="Defasagem")

Box.test(Var_Dependente, lag=12, type="Box-Pierce") #IPCA
Box.test(Var_Independente1, lag=12, type="Box-Pierce") #IPCA lag 1 / IPCA lag 1
Box.test(Var_Independente2, lag=12, type="Box-Pierce") #IPCA lag 2 / EIPCA
Box.test(Var_Independente3, lag=12, type="Box-Pierce") #HIATO / HIATO
Box.test(Var_Independente4, lag=12, type="Box-Pierce") #Cambio / Cambio 
Box.test(residuos, lag=12, type="Box-Pierce")



dickey1<-ur.df(Var_Dependente, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
dickey1<-ur.df(Var_Independente1, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
dickey1<-ur.df(Var_Independente2, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
dickey1<-ur.df(Var_Independente3, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
dickey1<-ur.df(Var_Independente4, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
dickey1<-ur.df(residuos, lags=12, type="trend", selectlags="AIC")
summary(dickey1)
}

  
                     #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                     #                                                               #
                     #                    TESTE DE NORMALIDADE                       #
                     #                                                               #
                     #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                         
{
require(fBasics)
jarqueberaTest(residuos)
hist(residuos, main="", col="dodgerblue3", prob=T, xlab=names(residuos)[1], breaks=30)
curve(expr = dnorm(x, mean=mean(residuos), sd=sd(residuos)), col="violetred1", add=TRUE, lwd=3)
skewness(residuos)
kurtosis(residuos)
par(mfrow=c(1,1))
qqnorm(residuos, col="dodgerblue3")

qqline(residuos, col="violetred1", lwd=3)
shapiro.test(residuos)
}

                    #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                    #                                                                 #
                    #                    TESTE DE MULTICOLINEARIDADE                  #
                    #                                                                 #
                    #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
{
# Matriz de correlacao (MODELO MAIS SIMPLES POSSIVEL)
as.matrix(cor(PhillipsSimples))
cor.test(ipca,hiato)
cor.test(ipca,Cambio)
cor.test(ipca,eipca)

# Fator de incremento da variancia (se > 10  = coef. correlacao >0.90 ou seja grande correlacao)
vif(regressao)
}

                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                        #                                                     #
                        #         TESTE DE RAIZ UNITARIA (EM NÍVEL)           #
                        #                                                     #
                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
{
# teste q box pierce

Box.test(regressao, lag=1, type="Box-Pierce")
Box.test(regressao, lag=2, type="Box-Pierce")
Box.test(regressao, lag=4, type="Box-Pierce")
Box.test(regressao, lag=8, type="Box-Pierce")
Box.test(regressao, lag=16, type="Box-Pierce")
Box.test(regressao, lag=20, type="Box-Pierce")

require(urca)

#Testes Dickey-Fuller
#modelo1
dickey1<-ur.df(regressao, lags=0, type="none")
summary(dickey1)
#modelo2
dickey2<-ur.df(regressao, lags=0, type="drift")
summary(dickey2)
#modelo3
dickey3<-ur.df(regressao, lags=0, type="trend")
summary(dickey3)

# Dickey-Fuller Ampliado
dickey4<-ur.df(regressao, lags=4, type="none")
summary(dickey4)
dickey5<-ur.df(regressao, lags=10, type="none")
summary(dickey5)

# Teste menor AIC
dickey6<-ur.df(regressao, lags=10, type="none", selectlags="AIC")
summary(dickey6)

# criterio bic
dickey7<-ur.df(regressao, lags=10, type="none", selectlags="BIC")
summary(dickey7)
#criterio bic com intercepto e sem tendencia
dickey8<-ur.df(regressao, lags=10, type="none", selectlags="BIC")
summary(dickey8)
#criterio bic com intercepto e  tendencia
dickey8<-ur.df(regressao, lags=10, type="trend", selectlags="BIC")
summary(dickey8)




#Regressao teste linear
PhillipsF=dynlm(diff(ipca)~lag(diff(ipca))+diff(eipca)+lag(diff(hiato))+diff(Cambio))
summary(PhillipsF)
regressao=PhillipsF
#regressao teste linear diff no cambio
PhillipsF=dynlm(diff(ipca)~lag(ipca,-1)+eipca+lag(hiato,-1)+diff(Cambio))
summary(PhillipsF)
regressao=PhillipsF
#regressao teste em log
PhillipsBPot=dynlm(diff(lnipca1)~lag(diff(lnipca1))+lag(diff(lnipca1))+lag(diff(lnhiato1))+diff(lnCambio1))
summary(PhillipsBPot)
regressao=PhillipsBPot
}




                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                        #                                                     #
                        #           EFEITO MARGINAL E ELASTICIDADE            #
                        #                                                     #
                        #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#


{
#PARA NFSP
EfeitoMargX1=IS$coef[5]*((mean(hiato))/mean(lag(nfspR,-1)))
EfeitoMargX1
ElastX1=IS$coef[5]
ElastX1
#Hiato defasado -1
EfeitoMargX2=IS$coef[3]*((mean(hiato1))/mean(lag(lnhiato1,-2)))
EfeitoMargX2
ElastX2=IS$coef[3]
ElastX2
#Hiato defasado -2

#PARA SELIC
EfeitoMargX3=IS$coef[4]*((mean(lnhiato1))/mean(lag(lnselic1,-1)))
EfeitoMargX3
ElastX3=IS$coef[4]
ElastX3

}                                  
                                  #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                                  #                                    #
                                  #          TESTES DE PLOTS           #
                                  #                                    #
                                  #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
{
grafico<-ggplot(data=PhillipsGraph)+geom_point(aes(x= Data, y=PibR, color=Governo, shape=Governo, size=Cambio))
grafico
grafico+theme()
grafico+theme_bw()
grafico+theme_classic()
grafico+theme_dark()
grafico+theme_get()
grafico+theme_gray()
grafico+theme_grey()
grafico+theme_light()
grafico+theme_linedraw()               
grafico+theme_minimal()
grafico+theme_replace()
grafico+theme_set()
grafico+theme_test()
grafico+theme_update()
grafico+theme_void()
grafico<-ggplot(data=PhillipsGraph)+geom_line(size=1.3,aes(x= Data, y=PibR, color=Governo))
grafico

# GRAFICO PIBR POR GOVERNO
graficopibr<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=PibR, color=Governo))+
  labs(title = "PIB Real por governo", x= "Data",y="PIB Real (R$ Milhões)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficopibr

# GRAFICO PIBR POR GOVERNO 2008.8 - 2014.12










# GRAFICO HIATO POR GOVERNO
graficohiato<-ggplot(data=hiatodf2)+
  geom_line(size=1.3, aes(x= PhillipsGraph.Data, y= hiatodf$value, color=Governo))+
  labs(title = "HIATO DO PRODUTO POR GOVERNO", x= "Data",y="Hiato (R$ Milhões)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficohiato


# GRAFICO IPCA POR GOVERNO
graficoipca<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=ipca, color=Governo))+
  labs(title = "Taxa de Inflação histórica por governos", x= "Data",y="IPCA (%)",subtitle = "2003.1 - 2021.12")+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"))+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoipca

BoxplotPIBGorvenos<-ggplot(data=PhillipsGraph)+geom_boxplot(aes(x= Governo, y=PibR))
BoxplotPIBGorvenos
BoxplotIPCAGorvenos<-ggplot(data=PhillipsGraph)+geom_boxplot(aes(x= Governo, y=ipca), fill="dodgerblue3", color="violetred1")
BoxplotIPCAGorvenos

# GRAFICO IPCA X EIPCA
graficoipca<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=ipca), color="dodgerblue3")+
  geom_line(size=1.3, aes(x= Data, y=eipca), color="violetred1")+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("12 months"))+
  labs(title = "IPCA x EIPCA", x= "Data",y="(%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoipca

# GRAFICO IPCA X EIPCA 
p1<-ggplot(data=PhillipsGraph, aes (x= Data))
p1<-p1+geom_line(size=1.2, aes(y=ipca, colour="blue"))
p1<-p1+geom_line(size=1.2, aes(y=eipca, colour="red"))
p1<-p1+scale_color_manual(name="Variáveis"
                          ,labels= c("IPCA", "EIPCA")
                          , values=c("violetred1", "dodgerblue3"))
p1<-p1+labs(title = "IPCA x EIPCA", x= "Data",y="%",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("12 months"))
p1

# GRAFICO PIBR X HIATO (teste)
p1<-ggplot(data=PhillipsGraph, aes (x= Data))
p1<-p1+geom_line(size=1.2, aes(y=PibR, colour="blue"))
p1<-p1+geom_line(size=1.2, aes(y=hiato, colour="red"))
p1<-p1+scale_color_manual(name="Variáveis"
                          ,labels= c("PibR", "hiato")
                          , values=c("violetred1", "dodgerblue3"))
p1<-p1+labs(title = "PibR x Hiato", x= "Data",y="R$ mi",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
p1

# GRAFICO IPCA X CAMBIO Real
p1<-ggplot(data=PhillipsGraph, aes (x= Data))
p1<-p1+geom_line(size=1.2, aes(y=ipca, colour="blue"))
p1<-p1+geom_line(size=1.2, aes(y=CambioReal2, colour="red"))
p1<-p1+scale_color_manual(name="Variáveis"
                          ,labels= c("IPCA", "Cambio Real")
                          , values=c("violetred1", "dodgerblue3"))
p1<-p1+labs(title = "IPCA x Cambio Real", x= "",y="(%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"))
p1

# GRAFICO IPCA X HIATO
plot(ipca, xlim=c(2003,2021), ylim=c(0, 18),xlab="", ylab="IPCA", lwd=2, col="dodgerblue3")
par(new=T)
plot(hiato, xlim=c(2003,2021), ylim=c(-375798, 394295), xlab="", ylab="", lwd=2,
     xaxt="n", yaxt="n", col="violetred1")



# MEDIA DO IPCA PARA CADA GOVERNO
ggplot(data=PhillipsGraph, aes(x=Governo, y=ipca, color=Governo, size=3))+
  geom_point(stat = "summary", fun="mean")

# GRAFICO GAMBIO NOMINAL POR GOVERNO
graficoCambio<-ggplot(data=PhillipsGraph)+geom_line(size=1.3, aes(x= Data, y=Cambio, color=Governo))
graficoCambio

graficoCambio<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=Cambio, color=Governo))+
  labs(title = "Taxa de Câmbio histórica por governos", x= "Data",y="Câmbio (%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoCambio

# GRAFICO GAMBIO REAL POR GOVERNO
graficoCambio<-ggplot(data=PhillipsGraph)+geom_line(size=1.3, aes(x= Data, y=CambioReal2, color=Governo))
graficoCambio

graficoCambio<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=CambioReal2, color=Governo))+
  labs(title = "Taxa de Câmbio histórica por governos", x= "Data",y="Câmbio (%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoCambio

# GRAFICO GAMBIO REAL POR GOVERNO COM LINHA MÉDIA
graficoCambio<-ggplot(data=PhillipsGraph)+geom_line(size=1.3, aes(x= Data, y=CambioReal2, color=Governo))
graficoCambio

graficoCambio<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=CambioReal2, color=Governo))+
  labs(title = "Taxa de Câmbio histórica por governos", x= "Data",y="Câmbio (%)",subtitle = "2003.1 - 2021.12 (Média 5.08)")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))+
        geom_hline(yintercept = mean(CambioReal2), colour="red")+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"))
graficoCambio


#PERIODOS DE RECESSAO
graficopibr2<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=PibR, color=Governo))+
  labs(title = "Períodos de Recessão", x= "",y="PIB Real (R$ Milhões)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))+
        annotate("rect", xmin=as.Date("2008-11-01"), xmax=as.Date("2009-09-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "black")+
        annotate("rect", xmin=as.Date("2014-11-15"), xmax=as.Date("2016-08-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "black")+
        annotate("rect", xmin=as.Date("2019-10-15"), xmax=as.Date("2021-01-01"), ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "black")+
  scale_x_date(labels = date_format("%Y"), breaks = date_breaks("2 years"))
  
graficopibr2















#testes skewness qqplot
              # Draw two plots next to each other
              par(mfrow = c(1, 2))
              
              # normal_density are the y-values for the normal curve
              # zs are the x-values for the normal curve
              n <- 1000
              normal_density <- dnorm(seq(-4, 4, 0.01))
              zs <- seq(-4, 4, 0.01)
              
              # Add some spice to the default histogram function
              hist_ <- function(x, ...){
                hist(x, breaks = 30, col="dodgerblue3", xlab = "Z", ylab = "",  yaxt='n', freq = FALSE, ...)
                lines(zs, normal_density, type = "l", col = "violetred1", lwd = 2)
              }
              
              # Gaussian Normal
              # rnorm() generates random numbers from a normal distribution
              # gaussian_rv is the dataset that will be compared to the Gaussian distribution
              gaussian_rv <- rnorm(n)
              
              # Draw the histogram
              hist_(gaussian_rv, main = "Gaussian Distribution")
              
              # Draw the Q-Q plot
              qqnorm(gaussian_rv, col="dodgerblue3")
              qqline(gaussian_rv, col = "violetred1", lwd = 2)
              skew_right <- c(gaussian_rv[gaussian_rv > 0] * 2.5, gaussian_rv)
              
              hist_(skew_right, main = "Assimetria Positiva", ylim = c(0, max(normal_density)))
              
              qqnorm(skew_right, col="dodgerblue3")
              qqline(skew_right, col = "violetred1", lwd = 2)
              
              # Skewed Left
              # skew_left is the dataset that will be compared to the Gaussian distribution
              skew_left <- c(gaussian_rv[gaussian_rv < 0]*2.5, gaussian_rv)
              
              hist_(skew_left, main = "Assimetria Negativa", ylim = c(0, max(normal_density)))
              
              qqnorm(skew_left, col="dodgerblue3")
              qqline(skew_left, col = "violetred1", lwd = 2)
   
}                   
                    #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                    #                                                         #
                    #                 ANÁLISE DAS VARIAVEIS                   #
                    #                                                         #
                    #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#

# PIBR
{
hist(PibR, nclass=30,labels=T, col="dodgerblue3", main="Histograma do PIB Acum. 12 Meses,", xlab='PIB', ylab="Frequencia")

graficopibr<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=PibR), color="dodgerblue3")+
  labs(title = "PIB Real", x= "Data",y="PIB Real (R$ Milhões)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficopibr

dickey1<-ur.df(PibR, lags=12, type="trend", selectlags="AIC")
summary(dickey1)

}




# IPCA
{
  #GRAFICO#
  graficoipca<-ggplot(data=PhillipsGraph)+
    geom_line(size=1.3, aes(x= Data, y=ipca), color="dodgerblue3")+
    labs(title = "IPCA Acumulado 12 Meses (%)", x= "Data",y="IPCA Ac. 12 (%)",subtitle = "2003.1 - 2021.12")+
    theme(axis.title.x= element_text(size=13),
          axis.title.y=element_text(size=13),
          axis.text.x=element_text(size=12),
          axis.text.y = element_text(size=12),
          plot.title = element_text(size=16, hjust = 0.5, face="bold"),
          plot.subtitle = element_text(size=12, hjust=0.5),
          legend.text = element_text(size=12))
  graficoipca
  #HISTOGRAMA#
  hist(ipca, nclass=30,labels=T, col="dodgerblue3", main="Histograma do IPCA", xlab='IPCA', ylab="Frequencia")
  
  #BOXPLOT#
  boxplot(ipca)
  BoxplotipcaGorvenos<-ggplot(data=PhillipsGraph)+geom_boxplot(aes(x= Governo, y=ipca, color=Governo),size=1)+
    theme(legend.text = element_text(size=13),
          axis.title.y=element_text(size=13),
          axis.title.x=element_text(size=13),
          axis.text.x=element_text(size=12),
          axis.text.y = el=ement_text(size=12))
  BoxplotipcaGorvenos
  
  boxplot(ipca, col='dodgerblue3',horizontal=TRUE) #BoxPlot lembrar Mediana e nao media / Quartil e nao quantil
  mtext("IPCA Acumulado 12 Meses (%)", side=3, line=1, font=2, cex=1)
  #QQPLOT#
  qqnorm(ipca, col="dodgerblue3", main= "Q-Q Plot IPCA")
  qqline(ipca, col="violetred1", lwd="2")  
  
  # Estatistica descritiva
  summary(ipca)
  sd(ipca)
  Coef.Var.ipca=sd(ipca)/mean(ipca)*100
  Coef.Var.ipca
  Statipca<-basicStats(ipca);Statipca
  skewness(ipca)
  kurtosis(ipca)
  jarqueberaTest(ipca)
  
  #TAXAS DE CRESCIMENTO
  {
    #Calculo da Taxa de Crescimento (Por programacao/Automatizado)
    
    n=length(ipca) # Atribuindo a 'n' o numero de observacoes
    n
    
    #Taxa de crescimento
    
    #PIBac12
    txipca1AUT<-((ipca[n]/ipca[1])-1)*100
    txipca1AUT
    
    #Taxa media mensal - Aritm?tica
    
    #PIBac12
    txipca2AUT<-(ipca[n]-ipca[1])/(ipca[1]*n)*100
    txipca2AUT
    
    
    #Taxa media mensal - geometrica
    
    #PIBAc12
    txipca3AUT<-(((ipca[n]/ipca[1])^(1/n))-1)*100
    txipca3AUT
    
    #Previsao com Matematica Financeira
    VFipcaL=ipca[n]*(1+(txipca2AUT/100))
    VFipcaL
    ipca[n]
    VFipcaG=ipca[n]*(1+(txipca3AUT/100))
    VFipcaG
    
    
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    
    #Gerando a Serie de Tendencia (t)
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(ipca)~t) #Variavel que eu estou  interessado eh o pib em funcao dessa sequencia (no caso t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo
  }
  dickey1<-ur.df(ipca, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}



# EIPCA
{
      #GRAFICO#
graficoeipca<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=eipca), color="dodgerblue3")+
  labs(title = "Expectativa da Inflação (EIPCA)", x= "Data",y="EIPCA (%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoeipca
      #HISTOGRAMA#
hist(eipca, nclass=30,labels=T, col="dodgerblue3", main="Histograma do eipca", xlab='eipca', ylab="Frequencia")

      #BOXPLOT#
boxplot(eipca)
BoxploteipcaGorvenos<-ggplot(data=PhillipsGraph)+geom_boxplot(aes(x= Governo, y=eipca, color=Governo),size=1)+
  theme(legend.text = element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))
BoxploteipcaGorvenos

boxplot(eipca, col='dodgerblue3',horizontal=TRUE) #BoxPlot lembrar Mediana e nao media / Quartil e nao quantil
mtext("Expectativa de inflação (EIPCA) (2003.1 - 2021.12)", side=3, line=1, font=2, cex=1.4)
      #QQPLOT#
qqnorm(eipca, col="dodgerblue3", main='')
mtext("Expectativa de inflação (EIPCA) (2003.1 - 2021.12)", side=3, line=1, font=2, cex=1.4)
qqline(eipca, col="violetred1", lwd="2")

  # Estatistica descritiva
    summary(eipca)
    sd(eipca)
    Coef.Var.eipca=sd(eipca)/mean(eipca)*100
    Coef.Var.eipca
    Stateipca<-basicStats(eipca);Stateipca
    skewness(eipca)
    kurtosis(eipca)
    jarqueberaTest(eipca)
        #TAXAS DE CRESCIMENTO
    {
      #Calculo da Taxa de Crescimento (Por programacao/Automatizado)
      
      n=length(eipca) # Atribuindo a 'n' o numero de observacoes
      n
      
      #Taxa de crescimento
      
      #PIBac12
      txeipca1AUT<-((eipca[n]/eipca[1])-1)*100
      txeipca1AUT
      
      #Taxa media mensal - Aritm?tica
      
      #PIBac12
      txeipca2AUT<-(eipca[n]-eipca[1])/(eipca[1]*n)*100
      txeipca2AUT
      
      
      #Taxa media mensal - geometrica
      
      #PIBAc12
      txeipca3AUT<-(((eipca[n]/eipca[1])^(1/n))-1)*100
      txeipca3AUT
      
      #Previsao com Matematica Financeira
      VFeipcaL=eipca[n]*(1+(txeipca2AUT/100))
      VFeipcaL
      eipca[n]
      VFeipcaG=eipca[n]*(1+(txeipca3AUT/100))
      VFeipcaG
      
      
      #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
      
      #Gerando a Serie de Tendencia (t)
      t=seq(1,n)
      t
      #Calculando a taxa de Crescimento
      Taxa6=lm(log(eipca)~t) #Variavel que eu estou  interessado eh o pib em funcao dessa sequencia (no caso t)
      Taxa6
      #Taxa Crescimento instantanea - em um ponto no tempo
      Cresc=(Taxa6$coeff[2])
      Cresc
      Cresc.Med=Cresc*100
      Cresc.Med
      #Taxa Crescimento Composta - ao longo de um periodo de tempo
      Cresc.G=(exp(Cresc))-1
      Cresc.Geo=Cresc.G*100
      Cresc.G
      Cresc.Geo
      
    }
    {
      dickey1<-ur.df(eipca, lags=12, type="trend", selectlags="AIC")
      summary(dickey1)
    }

}



# CAMBIO
{ 
  #GRAFICO CAMBIO NOMINAL #
graficoCambio<-ggplot(data=PhillipsGraph)+
  geom_line(size=1.3, aes(x= Data, y=Cambio), color="dodgerblue3")+
  labs(title = "Taxa de Câmbio", x= "Data",y="Câmbio (%)",subtitle = "2003.1 - 2021.12")+
  theme(axis.title.x= element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12),
        plot.title = element_text(size=16, hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size=12, hjust=0.5),
        legend.text = element_text(size=12))
graficoCambio
    #HISTOGRAMA#
hist(Cambio, nclass=30,labels=T, col="dodgerblue3", main="Histograma do Cambio", xlab='Cambio', ylab="Frequencia")

    #BOXPLOT#
boxplot(Cambio)
BoxplotCambioGorvenos<-ggplot(data=PhillipsGraph)+geom_boxplot(aes(x= Governo, y=Cambio, color=Governo),size=1)+
  theme(legend.text = element_text(size=13),
        axis.title.y=element_text(size=13),
        axis.title.x=element_text(size=13),
        axis.text.x=element_text(size=12),
        axis.text.y = element_text(size=12))
BoxplotCambioGorvenos

boxplot(Cambio, col='dodgerblue3',horizontal=TRUE) #BoxPlot lembrar Mediana e nao media / Quartil e nao quantil
mtext("Taxa de Câmbio (2003.1 - 2021.12)", side=3, line=1, font=2, cex=1)
    #QQPLOT#
qqnorm(Cambio, col="dodgerblue3", main= "Q-Q Plot Câmbio")
qqline(Cambio, col="violetred1", lwd="2")

  # Estatistica descritiva
    summary(Cambio)
    sd(Cambio)
    Coef.Var.Cambio=sd(Cambio)/mean(Cambio)*100
    Coef.Var.Cambio
    StatCambio<-basicStats(Cambio);StatCambio
    skewness(Cambio)
    kurtosis(Cambio)
    jarqueberaTest(Cambio)
    {
      #Calculo da Taxa de Crescimento (Por programacao/Automatizado)
      
      n=length(Cambio) # Atribuindo a 'n' o numero de observacoes
      n
      
      #Taxa de crescimento
      
      #PIBac12
      txCambio1AUT<-((Cambio[n]/Cambio[1])-1)*100
      txCambio1AUT
      
      #Taxa media mensal - Aritm?tica
      
      #PIBac12
      txCambio2AUT<-(Cambio[n]-Cambio[1])/(Cambio[1]*n)*100
      txCambio2AUT
      
      
      #Taxa media mensal - geometrica
      
      #PIBAc12
      txCambio3AUT<-(((Cambio[n]/Cambio[1])^(1/n))-1)*100
      txCambio3AUT
      
      #Previsao com Matematica Financeira
      VFCambioL=Cambio[n]*(1+(txCambio2AUT/100))
      VFCambioL
      Cambio[n]
      VFCambioG=Cambio[n]*(1+(txCambio3AUT/100))
      VFCambioG
      
      
      #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
      
      #Gerando a Serie de Tendencia (t)
      t=seq(1,n)
      t
      #Calculando a taxa de Crescimento
      Taxa6=lm(log(Cambio)~t) #Variavel que eu estou  interessado eh o pib em funcao dessa sequencia (no caso t)
      Taxa6
      #Taxa Crescimento instantanea - em um ponto no tempo
      Cresc=(Taxa6$coeff[2])
      Cresc
      Cresc.Med=Cresc*100
      Cresc.Med
      #Taxa Crescimento Composta - ao longo de um periodo de tempo
      Cresc.G=(exp(Cresc))-1
      Cresc.Geo=Cresc.G*100
      Cresc.G
      Cresc.Geo
    }
    dickey1<-ur.df(Cambio, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  
}

# HIATO
{
    #GRAFICO#
    graficohiato<-ggplot(data=hiatodf2)+
      geom_line(size=1.3, aes(x= PhillipsGraph.Data, y= hiatodf$value), color="dodgerblue3")+
      labs(title = "Hiato do Produto (HP-Filter)", x= "Data",y="Hiato (R$ Milhões)",subtitle = "2003.1 - 2021.12")+
      theme(axis.title.x= element_text(size=13),
            axis.title.y=element_text(size=13),
            axis.text.x=element_text(size=12),
            axis.text.y = element_text(size=12),
            plot.title = element_text(size=16, hjust = 0.5),
            plot.subtitle = element_text(size=12, hjust=0.5),
            legend.text = element_text(size=12))
    graficohiato

    #HISTOGRAMA#
    hist(hiatodf$value, nclass=30,labels=T, col="dodgerblue3", main="Histograma do Hiato (HP-Filter)", xlab='Hiato', ylab="Frequencia")
    #BOXPLOT#
    boxplot(hiatodf$value, col='dodgerblue3',horizontal=TRUE) #BoxPlot lembrar Mediana e nao media / Quartil e nao quantil
    mtext("Hiato (HP-Filter) (2003.1 - 2021.12)", side=3, line=1, font=2, cex=1)
    #QQPLOT#
    qqnorm(hiatodf$value, col="dodgerblue3", main= "Q-Q Plot Hiato (HP-Filter)")
    qqline(hiatodf$value, col="violetred1", lwd="2")
    
    # Estatistica descritiva
    summary(hiatodf$value)
    sd(hiato)
    Coef.Var.hiato=sd(hiato)/mean(hiato)*100
    Coef.Var.hiato
    Stathiato<-basicStats(hiato);Stathiato #CUIDADO COM A KURTOSE
    skewness(hiato)
    kurtosis(hiato)
    jarqueberaTest(hiato)
    {
      #Calculo da Taxa de Crescimento (Por programacao/Automatizado)
      
      n=length(hiato) # Atribuindo a 'n' o numero de observacoes
      n
      
      #Taxa de crescimento
      
      #PIBac12
      txhiato1AUT<-((hiato[n]/hiato[1])-1)*100
      txhiato1AUT
      
      #Taxa media mensal - Aritm?tica
      
      #PIBac12
      txhiato2AUT<-(hiato[n]-hiato[1])/(hiato[1]*n)*100
      txhiato2AUT
      
      
      #Taxa media mensal - geometrica
      
      #PIBAc12
      txhiato3AUT<-(((hiato[n]/hiato[1])^(1/n))-1)*100
      txhiato3AUT
      
      #Previsao com Matematica Financeira
      VFhiatoL=hiato[n]*(1+(txhiato2AUT/100))
      VFhiatoL
      hiato[n]
      VFhiatoG=hiato[n]*(1+(txhiato3AUT/100))
      VFhiatoG
      
      
      #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
      
      #Gerando a Serie de Tendencia (t)
      t=seq(1,n)
      t
      #Calculando a taxa de Crescimento
      Taxa6=lm(log(hiato)~t) #Variavel que eu estou  interessado eh o pib em funcao dessa sequencia (no caso t)
      Taxa6
      #Taxa Crescimento instantanea - em um ponto no tempo
      Cresc=(Taxa6$coeff[2])
      Cresc
      Cresc.Med=Cresc*100
      Cresc.Med
      #Taxa Crescimento Composta - ao longo de um periodo de tempo
      Cresc.G=(exp(Cresc))-1
      Cresc.Geo=Cresc.G*100
      Cresc.G
      Cresc.Geo
    }
    
    dickey1<-ur.df(hiato, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
}





                          #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#
                          #                                                #
                          #              ANALISE DOS PERIODOS              #
                          #                                                #
                          #IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII#



#PERIODO LULA
{
  #PIBR
  {
    aux=PibRLULA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
  
      n=length(aux)
      n
      
      #Taxa de crescimento
      txaux1AUT<-((aux[n]/aux[1])-1)*100
      txaux1AUT
      #Taxa media mensal - Aritm?tica
      txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
      txaux2AUT
      #Taxa media mensal - geometrica
      txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
      txaux3AUT
      #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
      t=seq(1,n)
      t
      #Calculando a taxa de Crescimento
      Taxa6=lm(log(aux)~t)
      Taxa6
      #Taxa Crescimento instantanea - em um ponto no tempo
      Cresc=(Taxa6$coeff[2])
      Cresc
      Cresc.Med=Cresc*100
      Cresc.Med
      #Taxa Crescimento Composta - ao longo de um periodo de tempo
      Cresc.G=(exp(Cresc))-1
      Cresc.Geo=Cresc.G*100
      Cresc.G
      Cresc.Geo
      dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
      summary(dickey1)
  }
  #IPCA
  {
    aux=ipcaLULA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)

    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo 
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #EIPCA
  {
    aux=eipcaLULA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo  
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #CAMBIO
  {
    aux=CambioLULA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
}

#PERIODO DILMA
{
  #PIBR
  {
    aux=PibRDILMA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #IPCA
  {
    aux=ipcaDILMA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo   
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #EIPCA
  {
    aux=eipcaDILMA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo  
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #CAMBIO
  {
    aux=CambioDILMA
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
    #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
    t=seq(1,n)
    t
    #Calculando a taxa de Crescimento
    Taxa6=lm(log(aux)~t)
    Taxa6
    #Taxa Crescimento instantanea - em um ponto no tempo
    Cresc=(Taxa6$coeff[2])
    Cresc
    Cresc.Med=Cresc*100
    Cresc.Med
    #Taxa Crescimento Composta - ao longo de um periodo de tempo
    Cresc.G=(exp(Cresc))-1
    Cresc.Geo=Cresc.G*100
    Cresc.G
    Cresc.Geo   
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
} 
  
#PERIODO TEMER
{
  #PIBR
  {
    aux=PibRTEMER
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #IPCA
  {
    aux=ipcaTEMER
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #EIPCA
  {
    aux=eipcaTEMER
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #CAMBIO
  {
    aux=CambioTEMER
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
} 

#PERIODO BOLSONARO
{
  #PIBR
  {
    aux=PibRBOLSONARO
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #IPCA
  {
    aux=ipcaBOLSONARO
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #EIPCA
  {
    aux=eipcaBOLSONARO
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT
  
    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
  #CAMBIO
  {
    aux=CambioBOLSONARO
    summary(aux)
    sd(aux)
    Coef.Var.aux=sd(aux)/mean(aux)*100
    Coef.Var.aux
    Stataux<-basicStats(aux);Stataux
    skewness(aux)
    kurtosis(aux)
    jarqueberaTest(aux)
    
    n=length(aux)
    n
    
    #Taxa de crescimento
    txaux1AUT<-((aux[n]/aux[1])-1)*100
    txaux1AUT
    #Taxa media mensal - Aritm?tica
    txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
    txaux2AUT
    #Taxa media mensal - geometrica
    txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
    txaux3AUT

    dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
    summary(dickey1)
  }
} 

#RECESSAO ECONOMICA 2008.10.01 - 2009-09-01 (FINAL DE 2008 até meio/final 2009)
{
  recessao1<-window(pib,frequency=12, start=c(2008,12), end=c(2009,9))
  aux=recessao1
  summary(aux)
  sd(aux)
  Coef.Var.aux=sd(aux)/mean(aux)*100
  Coef.Var.aux
  Stataux<-basicStats(aux);Stataux
  skewness(aux)
  kurtosis(aux)
  jarqueberaTest(aux)
  
  n=length(aux)
  n
  
  #Taxa de crescimento
  txaux1AUT<-((aux[n]/aux[1])-1)*100
  txaux1AUT
  #Taxa media mensal - Aritm?tica
  txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
  txaux2AUT
  #Taxa media mensal - geometrica
  txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
  txaux3AUT
  #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
  t=seq(1,n)
  t
  #Calculando a taxa de Crescimento
  Taxa6=lm(log(aux)~t)
  Taxa6
  #Taxa Crescimento instantanea - em um ponto no tempo
  Cresc=(Taxa6$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med
  #Taxa Crescimento Composta - ao longo de um periodo de tempo
  Cresc.G=(exp(Cresc))-1
  Cresc.Geo=Cresc.G*100
  Cresc.G
  Cresc.Geo
  dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}
#RECESSAO ECONOMICA 2014-11-15 - 2016-08-01 (final de 2014 até quando temer assumiu o governo)
{
  recessao2<-window(pib,frequency=12, start=c(2014,11), end=c(2016,8))
  aux=recessao2
  summary(aux)
  sd(aux)
  Coef.Var.aux=sd(aux)/mean(aux)*100
  Coef.Var.aux
  Stataux<-basicStats(aux);Stataux
  skewness(aux)
  kurtosis(aux)
  jarqueberaTest(aux)
  
  n=length(aux)
  n
  
  #Taxa de crescimento
  txaux1AUT<-((aux[n]/aux[1])-1)*100
  txaux1AUT
  #Taxa media mensal - Aritm?tica
  txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
  txaux2AUT
  #Taxa media mensal - geometrica
  txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
  txaux3AUT
  #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
  t=seq(1,n)
  t
  #Calculando a taxa de Crescimento
  Taxa6=lm(log(aux)~t)
  Taxa6
  #Taxa Crescimento instantanea - em um ponto no tempo
  Cresc=(Taxa6$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med
  #Taxa Crescimento Composta - ao longo de um periodo de tempo
  Cresc.G=(exp(Cresc))-1
  Cresc.Geo=Cresc.G*100
  Cresc.G
  Cresc.Geo
  dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}
#RECESSSAO ECONOMICA  2019-10-15 - 2021-01-01 (final de 2019 até começo de 2021)
{
  recessao3<-window(pib,frequency=12, start=c(2019,11), end=c(2021,1))
  aux=recessao3
  summary(aux)
  sd(aux)
  Coef.Var.aux=sd(aux)/mean(aux)*100
  Coef.Var.aux
  Stataux<-basicStats(aux);Stataux
  skewness(aux)
  kurtosis(aux)
  jarqueberaTest(aux)
  
  n=length(aux)
  n
  
  #Taxa de crescimento
  txaux1AUT<-((aux[n]/aux[1])-1)*100
  txaux1AUT
  #Taxa media mensal - Aritm?tica
  txaux2AUT<-(aux[n]-aux[1])/(aux[1]*n)*100
  txaux2AUT
  #Taxa media mensal - geometrica
  txaux3AUT<-(((aux[n]/aux[1])^(1/n))-1)*100
  txaux3AUT
  #Taxa de Crescimento Modelo Semilogaritmicos / Log-Lin
  t=seq(1,n)
  t
  #Calculando a taxa de Crescimento
  Taxa6=lm(log(aux)~t)
  Taxa6
  #Taxa Crescimento instantanea - em um ponto no tempo
  Cresc=(Taxa6$coeff[2])
  Cresc
  Cresc.Med=Cresc*100
  Cresc.Med
  #Taxa Crescimento Composta - ao longo de um periodo de tempo
  Cresc.G=(exp(Cresc))-1
  Cresc.Geo=Cresc.G*100
  Cresc.G
  Cresc.Geo
  dickey1<-ur.df(aux, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
}






#TESTE MELHOR MODELO - ALISSON

  #FORWARD LOOKING LINEAR (1a DIFERENCA)
  {
  Dipca=diff(ipca)
  Dhiato=diff(hiato)
  DCambio=diff(Cambio)
  Deipca=diff(eipca)
  
  Dipca2017_2021<-window(Dipca, frequency=12, start=c(2017,1), end=c(2021,12))
  Deipca2017_2021<-window(Deipca, frequency=12, start=c(2017,1), end=c(2021,12))
  Dhiato2017_2021<-window(Dhiato, frequency=12, start=c(2017,1), end=c(2021,12))
  DCambio2017_2021<-window(DCambio, frequency=12, start=c(2017,1), end=c(2021,12))
  
  PhillipsBLD2017_2021=dynlm(Dipca2017_2021~lag(Dipca2017_2021,-1)+Deipca2017_2021+lag(Dhiato2017_2021,-1)+DCambio2017_2021)
  summary(PhillipsBLD2017_2021)
  regressao=PhillipsBLD2017_2021
  residuos=residuals(regressao);plot(residuos)
  
  #homocedasticidade -> hipotese nula que os residuos nao são heterocedasticos
  gqtest(regressao, fraction=15, alternative = "greater")
  bptest(regressao)
  white_test(regressao)
  #autocorrelacao -> hipotese nula que os residuos não são autocorrelacionados
  dwtest(regressao)
  bgtest(regressao, order=4)
  ArchTest(residuos, lags=4)
  #normalidade -> hipotese nula que os residuos apresentam distribuição normal
  jarqueberaTest(residuos)
  shapiro.test(residuos)
  #estacionaridade -> hipotese nula que os residuos são estacionarios
  Box.test(residuos, lag=12, type="Box-Pierce")
  #adf -> hipotese nula que há raiz unitária (ruim)
  dickey1<-ur.df(ipca2017_2021, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  dickey1<-ur.df(lag(ipca2017_2021,-1), lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  dickey1<-ur.df(eipca2017_2021, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  dickey1<-ur.df(lag(hiato2017_2021,-1), lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  dickey1<-ur.df(Cambio2017_2021, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  dickey1<-ur.df(residuos, lags=12, type="trend", selectlags="AIC")
  summary(dickey1)
  # Apesar das variáveis não apresentaram estacionaridade pelo teste de dickey-fuller
  # os resíduos passaram no teste, não apresentando raiz unitária, portanto, estacionario)
  
  #multicolinearidade
  vif(regressao) #fator de incremento da variancia (ACIMA DE 10 = RUIM, ou seja, tem multicolinearidade)
 
   }
  
  
#VARIAVEL DEPENDENTE 
    #IPCA

#VARIAVEIS INDEPENDENTES
    #IPCA DEFASADO 1
    #EXPECTATIVA DA INFLAÇÃO
    #HIATO DEFASADO 1
    #CAMBIO 
