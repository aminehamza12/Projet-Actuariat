## Encore un essai 
library(forecast) 
library(demography)

help(hmd.mx)

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
     main = "U.K total death rates for years (1945, 1962, 1979, 1996, 2013)"
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
UK1922 <- extract.years(demoUK, years=1922:2013)
UK.smth.m <- smooth.demogdata(UK1922, method="mspline") #spline monotone
UK.smth.u <- smooth.demogdata(UK1922, method="spline")  #spline standard
par(mfrow=c(1,2))

plot(demoUK, years=1945, type="p", pch=21, ylim=c(-7, 1))
lines(demoUK.smth.m, years=1945)
lines(demoUK.smth.u, years=1945, lty=3)


library(lifecontingencies)

# Calculs actuariels d'après les taux de 2013
qx_2016 = demoUK$rate$total[0:109,"2016"]

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

#2éme contrat:
K = 100000
VAP_capdec = K * Axn(actuarialtable2016, x=40)
VAP_capdec


## 4 Fitting Lee Carter Model:
library(StMoMo)

#conversion des données en StMoMo objet
#approximation centrale de l'exposition (exposures)
demoUK.momo<-StMoMoData(data=demoUK , series = "total",type="central")

names(demoUK.momo)
head(demoUK.momo$Ext[,'2013']) #???Exposition au risque de l'année 2013

#plage d'âges
ages.fit = 0:99
#période de calibration
years.fit = 1920:2014

#Fitting the model
LC <- lc(link = "logit")
LCfit <- fit(LC, data = central2initial(demoUK.momo), 
                                        ages.fit = ages.fit,
                                        years.fit = years.fit)
#affichage des paramètres estimés:
plot(LCfit)

#pendant 1835 jusqu'à 2003
LCfit2 <- fit(LC, data = central2initial(demoUK.momo), 
             ages.fit = ages.fit,
             years.fit = 1835:2003)

plot(LCfit2) 


## analyse des résidus
LCres <- residuals(LCfit)
plot(LCres)

plot(LCres)

#On peut afficher la moyenne des résidus par âge ou par année:
mean(LCres$residuals[1,]) #moyenne des résidus de l'âge 1

## Projecting and simulating mortality models

horizon=50
LCfor <- forecast(LCfit, h = horizon)
plot(LCfor)
#on peut changer la période de calibration et voir regarder à 
#chaque fois l'intervalle de confiance'
LCfor$rates[,'2040']


demoUK$year[1:180]
LCfor$years

LCfor$rates[,"2015"]

#Past and forecasted rates are here binded in the same matrix.
rates<-cbind(demoUK$rate$male[0:99,1:180],LCfor$rate[0:99,])
head(rates)

plot(seq(min(demoUK$year[1:180]),max(demoUK$year[1:180])+horizon),
      rates[65,],
      xlab="Years",ylab="Death Rates",type="l",
     main="Taux observés et projetés à un horizon de 50 ans pour x = 65 ans")


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


#Remarque: Pour vérifier la plage d'âges des taux estimés de la cohorte
#cohort(2016, LCfor$rates)
extractCohort(LCfor$rates, cohort = 1980)

chosen_cohort=1980
#les taux de 1980 en un seul vecteur:
lc_historical_rates <- extractCohort(fitted(LCfit, type = "rates"),
                        cohort = chosen_cohort)
lc_forecasted_rates <- extractCohort(LCfor$rates,
                        cohort = chosen_cohort)
lc_rates_1980 <- c(lc_historical_rates,lc_forecasted_rates)

library(lifecontingencies)
lc_qx_1980<-mx2qx(lc_rates_1950)

#transformation en lifetable:
lc_lifetable_1980<-probs2lifetable(probs=lc_qx_1980,type = "qx",
                       name = paste("LC","1980","lt",sep="_"))
exn(lc_lifetable_1980,x=65)


#création d'une table actuarielle:

lc_acttbl_1980<-new("actuarialtable",x=lc_lifetable_1980@x,
                     lx=lc_lifetable_1980@lx,
                     interest = 0.025)

axn(lc_acttbl_1950,x=65)












