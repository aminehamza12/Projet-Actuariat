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


#taux de mortalité pour une année exemple : l'année 1988
plot(demoUK$age, log(demoUK$rate$total[,"1988"]), main ='log mortality rates (United Kingdom, 1988)',
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

-------------------------------------------------------------------------
#Il est possible de sélectionner des années
UK_years = c(2015,2016)
plot(demoUK, series = 'total', years = UK_years ,
     main = "United Kingdom total death rates for years (2015, 2016)"
     )
legend(x="bottomright", legend = UK_years,
       col = rainbow(length(UK_years)*1.25),
       lty = 1,
       cex=0.7,
       box.lwd = 0.3
       ) 
------------------------------------------------------------------------
++++tracer les taux de mortalité en fonction de l’âge pour les femmes pour les 
âges à partir de 2015:
UK_years = c(2015,2016)
plot(demoUK, series = 'female', years = UK_years ,
     main = "United Kingdom ++female++ death rates for years (2015, 2016)"
     )
legend(x="bottomright", legend = UK_years,
       col = rainbow(length(UK_years)*1.25),
       lty = 1,
       cex=0.7,
       box.lwd = 0.3
       )
------------------------------------------------------------------------
++++tracer les taux de mortalité en fonction de l’âge pour les hommes pour les 
âges à partir de 2015:
UK_years = c(2015,2016)
plot(demoUK, series = 'male', years = UK_years ,
     main = "United Kingdom ++male++ death rates for years (2015, 2016)"
     )
legend(x="bottomright", legend = UK_years,
       col = rainbow(length(UK_years)*1.25),
       lty = 1,
       cex=0.7,
       box.lwd = 0.3
       )
------------------------------------------------------------------------

#Il est possible de sélectionner des âges:
UK_ages = c(0,27,55,82,110)
plot(demoUK, series = 'total', ages = UK_ages,
     main = "United Kingdom total death rates for ages (0, 27, 55, 82, 110)",
)
legend(x="bottomright", legend = UK_ages,
       col = rainbow(length(UK_ages)*1.25),
       lty = 1,
       cex=0.7,
       box.lwd = 0.3
) 

# il est possible de séléctionner des âges pendant une période d'années
UK.mort_age = extract.ages(demoUK,0:60,F)    
UK.mort_age_yr = extract.years(UK.mort_age,2000:2016)    
plot(UK.mort_age_yr,series='male')   
legend("bottomright",legend=unique(UK.mort_age_yr$year),
       col=rainbow(length(UK.mort_age_yr$year)*1.25), ncol=5, pch=19, 
       title="Year", cex=0.5)


## sélection des âges en fonction des années

par(mfrow=c(1,3))   #pour créer 3 sous-plots
plot(demoUK,series="male",datatype="rate", plot.type="time", main="Male rates",xlab="Years")
plot(demoUK,series="female",datatype="rate", plot.type="time", main="Female rates",xlab="Years")
plot(demoUK,series="total",datatype="rate", plot.type="time", main="Total rates",xlab="Years")




## Lissage de la table 

plot(demoUK, series ='male',years=2003, type="p", pch=1 )
# (voir smoothing 2 et 3)


smus = smooth.demogdata(demoUK)
plot(demoUK, series ='male',years=2003, type="p", pch=1, col='gray' )
lines(smus, series = 'male', years = 2003)


##2 types de lissages:
demoUK1955<- extract.years(demoUK, years=1955:2016)
demoUK.smth.m <- smooth.demogdata(demoUK1955, method="mspline") #spline monotone
demoUK.smth.u <- smooth.demogdata(demoUK1955, method="spline")  #spline standard
par(mfrow=c(1,2))

plot(demoUK, years=1945, type="p", pch=21, ylim=c(-7, 1))
lines(demoUK.smth.m, years=1955)
lines(demoUK.smth.u, years=1955, lty=3)


#install.packages("fmsb")
library(fmsb)

library(lifecontingencies)

# Calculs actuariels d'après les taux de 2013
qx_2016 = demoUK$rate$total[0:109,"2016"] #ici on choisi female / male

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
#Un contrat de retraite pour lequel l’assuré recevra un rentre de R = 1000 euros à partir de l’âge de 65 ans, et paiera une prime annuelle jusqu’à 65 ans. :
R = 100
VAP_retraite = R * axn(actuarialtable2016, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraite

#Un contrat de capital décès pour lequel les béné?ciaires de l’assuré recevront un capital de K = 100 000 euros en échange du paiement d’une prime annuelle par l’assuré. :
K = 100000
VAP_capdec = K * Axn(actuarialtable2016, x=40)
VAP_capdec

----------------------------------------------------------------------

# Calculs actuariels d'après les taux de 2013
qx_2016 = demoUK$rate$female[0:109,"2016"] #ici on choisi female 
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
#Un contrat de retraite pour lequel l’assuré recevra un rentre de R = 1000 euros à partir de l’âge de 65 ans, et paiera une prime annuelle jusqu’à 65 ans. :
R = 100
VAP_retraite = R * axn(actuarialtable2016, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraite


----------------------------------------------------------------------
qx_2016 = demoUK$rate$male[0:109,"2016"] #ici on choisi female 
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
#Un contrat de retraite pour lequel l’assuré recevra un rentre de R = 1000 euros à partir de l’âge de 65 ans, et paiera une prime annuelle jusqu’à 65 ans. :
R = 100
VAP_retraite = R * axn(actuarialtable2016, x=40, n=25, i=i)  #différé de 25 ans
VAP_retraite













