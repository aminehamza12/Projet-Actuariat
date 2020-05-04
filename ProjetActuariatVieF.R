## Encore un essai 
library(forecast) 
library(demography)


setwd("C:/Users/LEGION/Desktop/Projet Actuariat/ok")

## 1. Chargement des données HMD:

demoUK <- read.demogdata(file="Mx_1x1.txt", 
                           popfile="Exposures_1x1.txt",
                           type="mortality", label="United Kingdom Total Population")
class(demoUK)
names(demoUK)


# l'annee 1988 pour les femmes
head(demoUK$rate$female[, "1988"])

# 6 premieres annees de la population totale
head(demoUK$rate$total[, 1:6])

print(demoUK)
summary(demoUK)


#taux de mortalité pour l'année 1988
par(mfrow=c(1,1))
plot(demoUK$age, log(demoUK$rate$total[,"1988"]), main ='log mortality rates (demoUK, 1988)',
     xlab = "Ages x", ylab = "log mortality rates", type = "l")


## 2. Affichage des objets demogdata: 

plot(demoUK, series = 'total')
plot(demoUK, series = 'male')
plot(demoUK, series = 'female')

## avec légendes des années  
legend("bottomright",legend=unique(demoUK$year),
       col=rainbow(length(demoUK$year)*1.25), ncol=5, pch=3, 
       title="Year", cex=0.2)

## avec légendes d'âges
legend("bottomright",legend=unique(demoUK$age),
       col=rainbow(length(demoUK$age)*1.25), ncol=5, pch=3, 
       title="Âges", cex=0.3)


#Il est possible de sélectionner des années
UK_years = c(1945,1962,1979,1996,2013)
plot(demoUK, series = 'total', years = UK_years ,
     main = "demoUKtotal death rates for years (1945, 1962, 1979, 1996, 2013)"
     )
legend(x="bottomright", legend = UK_years,
       col = rainbow(length(UK_years)*1.25),
       lty = 1,
       cex=0.7,
       box.lwd = 0.3
       ) 

#Il est possible de sélectionner les âges en fonction des années:
par(mfrow=c(1,1))   #pour créer 3 sous-plots

UK_ages = c(0,27,55,82,110)
plot(demoUK,series="male",datatype="rate", plot.type="time", 
     main="Male rates",xlab="Years")
legend(x="bottomleft", legend = UK_ages,
       col = rainbow(length(UK_ages)*1.25),
       lty = 1,
       cex=0.6,
       box.lwd = 0.3
) 

plot(demoUK,series="female",datatype="rate", plot.type="time", main="Female rates",xlab="Years")
plot(demoUK,series="total",datatype="rate", plot.type="time", main="Total rates",xlab="Years")



# il est possible de séléctionner des âges pendant une période d'années
UK.mort_age = extract.ages(demoUK,0:60,F)    
UK.mort_age_yr = extract.years(UK.mort_age,1918:1950)    
plot(UK.mort_age_yr,series='male')   
legend("bottomright",legend=unique(UK.mort_age_yr$year),
       col=rainbow(length(UK.mort_age_yr$year)*1.25), ncol=5, pch=19, 
       title="Year", cex=0.5)



## Lissage de la table 

plot(demoUK, series ='male',years=2003, type="p", pch=1 )
# (voir smoothing 2 et 3)


smus = smooth.demogdata(demoUK)
plot(demoUK, series ='male',years=2003, type="p", pch=1, col='gray' )
lines(smus, series = 'male', years = 2003)


##2 types de lissages:
demoUK1914 <- extract.years(demoUK, years=1914:2013)
demoUK.smth.m <- smooth.demogdata(demoUK1914, method="mspline") #spline monotone
demoUK.smth.u <- smooth.demogdata(demoUK1914, method="spline")  #spline standard
par(mfrow=c(1,2))

plot(demoUK, years=1945, type="p", pch=21, ylim=c(-7, 1))
lines(demoUK.smth.m, years=1945)
lines(demoUK.smth.u, years=1945, lty=3)


library(lifecontingencies)

# Calculs actuariels d'après les taux de 2016
qx_2016 = demoUK$rate$total[0:108,"2016"]

lifetable_2016 <- probs2lifetable(probs= qx_2016,
                 type = "qx"
                 )

exn(lifetable_2016,x=65)

qxt(lifetable_2016, 60, 1)
pxt(lifetable_2016, 60, 1)

#pour le calcul des VAP on transforme la lifetable en table actuarielle:
actuarialtable2016 <- new("actuarialtable", x=lifetable_2016@x, lx=lifetable_2016@lx,
                interest=2.5/100)   #interest rate = 2.5%


i = 0.025  #25%
#1er contrat:
R = 1000
VAP_retraite = R * axn(actuarialtable2016, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraite



## 4 Fitting Lee Carter Model:
library(StMoMo)
library(gnm)

#conversion des données en StMoMo objet
#approximation centrale de l'exposition (exposures)
demoUK.momo<-StMoMoData(data=demoUK, series = "total",type="central")

names(demoUK.momo)
head(demoUK.momo$Ext[,'2016']) #???Exposition au risque de l'année 2016

summary(demoUK)

#plage d'âges
ages.fit = 0:105
#période de calibration
years.fit = 1955:2016

#Fitting the model
LC <- lc(link = "logit")
LCfit <- fit(LC, data = central2initial(demoUK.momo), 
                                        ages.fit = ages.fit,
                                        years.fit = years.fit)

#affichage des paramètres estimés:
par(mar=c(1,1,1,1))
plot(LCfit)
LCfit

#pendant 1955 jusqu'à 2016
LCfit2 <- fit(LC, data = central2initial(demoUK.momo), 
             ages.fit = ages.fit,
             years.fit = 1955:2016)

plot(LCfit2) 


## analyse des résidus
LCres <- residuals(LCfit)
plot(LCres)
LCres2 <- residuals(LCfit2)
plot(LCres2)

#On peut afficher la moyenne des résidus par âge ou par année:
mean(LCres$residuals[1,]) #moyenne des résidus de l'âge 1

## Projecting and simulating mortality models
horizon=50
LCfor <- forecast(LCfit, h = horizon)
plot(LCfor)

#on peut changer la période de calibration et voir regarder à 
#chaque fois l'intervalle de confiance'
LCfor$rates[,'2040']


demoUK$year[1:90]
LCfor$years

LCfor$rates[,"2016"]

#Past and forecasted rates are here binded in the same matrix.
rates<-cbind(demoUK$rate$total[0:50,1:50],LCfor$rate[0:50,])
head(rates)

plot(seq(min(demoUK$year[1:180]),max(demoUK$year[1:180])+horizon),
      rates[65,],
      xlab="Years",ylab="Death Rates",type="l",
     main="Taux observés et projetés à un horizon de 50 ans pour x = 65 ans")

##methode autre que forecast
par(mfrow=c(1,1))
nsims = 100
LCsim <- simulate(LCfit, nsim = nsims, h = 50)

plot(LCfit$years, LCfit$kt[1, ],
     xlim = range(LCfit$years, LCsim$kt.s$years),
     ylim = range(LCfit$kt, LCsim$kt.s$sim[1, , 1:20]), type = "l",
     xlab = "year", ylab = "kt", main = "LC model simulations")

matlines(LCsim$kt.s$years, LCsim$kt.s$sim[1, , 1:20],
         type = "l", lty = 1)

                  
#plotting historical fitted rates, until max(years.fit) =2014
chosen_cohort=1950         #doit appartenir aux years de LCfit
plot(0:64, extractCohort(fitted(LCfit, type = "rates"),  #x = 0: max(years.fit)-chosen_cohort
       cohort = chosen_cohort),
       type = "l", log = "y", xlab = "age", ylab = "q(x)",
       main = paste(c("Cohort",toString(chosen_cohort),"mortality rates"), collapse = " "),
       xlim = c(0,103), ylim = c(0.0005, 0.07))

#adding fitted projections
lines(65:99, extractCohort(LCfor$rates, cohort = chosen_cohort), # x = l'âge suivant:max(years.fit)+h - chosen_cohort 
      lty = 2, lwd=2, col="red")
               
chosen_cohort=1980        
plot(0:34, extractCohort(fitted(LCfit, type = "rates"),  
                         cohort = chosen_cohort),
     type = "l", log = "y", xlab = "age", ylab = "q(x)",
     main = paste(c("Cohort",toString(chosen_cohort),"mortality rates"), collapse = " "),
     xlim = c(0,103))
lines(35:84, extractCohort(LCfor$rates, cohort = chosen_cohort),
      lty = 2, lwd=2, col="red")

chosen_cohort=2016       
plot(0:2, extractCohort(fitted(LCfit, type = "rates"),  
                         cohort = chosen_cohort),
     type = "l", log = "y", xlab = "age", ylab = "q(x)",
     main = paste(c("Cohort",toString(chosen_cohort),"mortality rates"), collapse = " "),
     xlim = c(0,103))
lines(3:51, extractCohort(LCfor$rates, cohort = chosen_cohort),
      lty = 2, lwd=2, col="red")

#Remarque: Pour vérifier la plage d'âges des taux estimés de la cohorte
#cohort(2016, LCfor$rates)
extractCohort(LCfor$rates, cohort =2016 )

chosen_cohort=2016
#les taux de 2016 en un seul vecteur:
lc_historical_rates <- extractCohort(fitted(LCfit, type = "rates"),
                        cohort = chosen_cohort)
lc_forecasted_rates <- extractCohort(LCfor$rates,
                        cohort = chosen_cohort)
lc_rates_2016 <- c(lc_historical_rates,lc_forecasted_rates)

chosen_cohort=1976
#les taux de 1976 en un seul vecteur:
lc_historical_rates1976 <- extractCohort(fitted(LCfit, type = "rates"),
                                     cohort = chosen_cohort)
lc_forecasted_rates1976 <- extractCohort(LCfor$rates,
                                     cohort = chosen_cohort)
lc_rates_1976 <- c(lc_historical_rates1976,lc_forecasted_rates1976)


library(lifecontingencies)
lc_qx_2016<-mx2qx(lc_rates_2016)
lc_qx_1976<-mx2qx(lc_rates_1976)

#transformation en lifetable:
lc_lifetable_2016<-probs2lifetable(probs=lc_qx_2016,type = "qx",
                       name = paste("LC","2016","lt",sep="_"))
lc_lifetable_1976<-probs2lifetable(probs=lc_qx_1976,type = "qx",
                                   name = paste("LC","1976","lt",sep="_"))
exn(lc_lifetable_2016,x=40)
exn(lc_lifetable_1976,x=40)

#création d'une table actuarielle:

lc_acttbl_2016<-new("actuarialtable",x=lc_lifetable_2016@x,
                     lx=lc_lifetable_2016@lx,
                     interest = 0.025)
lc_acttbl_1976<-new("actuarialtable",x=lc_lifetable_1976@x,
                    lx=lc_lifetable_1976@lx,
                    interest = 0.025)



#VAP en utilisant les taux projetés
i = 0.025  #25%

R = 1000
VAP_retraiteP = R * axn(lc_acttbl_2016, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraiteP_1976 = R * axn(lc_acttbl_1976, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraiteP
VAP_retraiteP_1976
VAP_retraite


#Tarification

Prime_Retraite1=VAP_retraiteP_1976/axn(lc_acttbl_1976, x=40, n=25, i=i)
Prime_Retraite1
Prime_Retraite=(1000*axn(lc_acttbl_1976, x=40, m=25, i=i))/axn(lc_acttbl_1976, x=40, n=25, i=i)
Prime_Retraite














