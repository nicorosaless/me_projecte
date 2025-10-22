################################################################################
#
# ME - GIA. Introducció a serie temporals
#
#--------------------------------------
# Conceptes
#--------------------------------------
#
# Estacionalitat
################################################################################

#-------------------------------------------------------------------------------
# Serie Air Passengers
#' 144 observacions amb els totals mensuals, en milers, dels passatgers de les 
#' línies aèries internacionals des de gener de 1949 a desembre de 1960 
#-------------------------------------------------------------------------------

##-- Legim la sèrie
airpass <- read.table("../Dades/airpass.dat")
airpass

##-- Creem l'objecte "ts" indicant inici i freqüència i el dibuixem
airpass <- ts(AirPassengers, start=1949, frequency=12)
class(airpass)
plot(airpass)

##-- Fem la transformació logaritme --> variancia constant
lnairpass <- log(airpass)
plot(lnairpass)

##-- Dibuixem les components de la sèrie
plot(decompose(lnairpass)) 

##-- ACF i PACF
par(mfrow=c(1,2))
 acf(lnairpass, ylim=c(-1,1), lagmax = 40)
pacf(lnairpass, ylim=c(-1,1), lagmax = 40)

##-- Diferenciem d'ordre 12 --> Treure estacionalitat
par(mfrow=c(1,1))
d12lnairpass <- diff(lnairpass, lag=12)
plot(d12lnairpass)

##-- ACF i PACF
par(mfrow=c(1,2))
 acf(d12lnairpass, ylim = c(-1,1), lagmax = 40)
pacf(d12lnairpass, ylim = c(-1,1), lagmax = 40)

##-- Diferenciem d'ordre 1 --> Eliminar tendencia
  d1d12lnairpass <- diff(  d12lnairpass)
d1d1d12lnairpass <- diff(d1d12lnairpass)
var(    d12lnairpass)
var(  d1d12lnairpass)
var(d1d1d12lnairpass)

##-- ACF i PACF
par(mfrow=c(1,2))
 acf(d1d12lnairpass, ylim=c(-1,1), lagmax = 40)
pacf(d1d12lnairpass, ylim=c(-1,1), lagmax = 40)


#-------------------------------------------------------------------------------
#	Serie: Gnpsh
#' Índex quatrimestral del Producte Nacional Brut (PNB) a USA, amb les dades 
#' **desestacionalitzades** des de 1947 fins a 1991
#-------------------------------------------------------------------------------


##-- Lectura de la serie
gnpsh <- read.table("../Dades/gnpsh.dat")
gnpsh

##-- Indicacio d'any d'inici i periode i plot de la serie
gnpsh <- ts(gnpsh, start = 1947, frequency = 4)
plot(gnpsh)

##-- Dibuixem les components de la sèrie
plot(decompose(gnpsh)) 

##-- La variancia sembla no constant: fem la transformacio logaritme?

# Transf. Log
lngnpsh <- log(gnpsh)

# Transf. Boxcox
library(forecast)
lambda  <- BoxCox.lambda(gnpsh)
bcgnpsh <- BoxCox(gnpsh, lambda = lambda)

# Comparar
par(mfrow=c(1,2))
plot(lngnpsh, main='Log')
plot(bcgnpsh, main='BoxCox')

##-- Representacio en la mateixa finestra l'ACF i PACF de la serie: encara no 
##   es estacionaria
par(mfrow=c(1,2))
 acf(lngnpsh,ylim=c(-1,1))
pacf(lngnpsh,ylim=c(-1,1))

##-- Diferenciacio d'ordre 1 per eliminar la tendencia.
  d1lngnpsh <- diff(  lngnpsh, lag=1)
d1d1lngnpsh <- diff(d1lngnpsh, lag=1)
var(  d1lngnpsh)
var(d1d1lngnpsh)

##-- Representacio de l'ACF i PACF: podem identificar possibles models
 acf(d1lngnpsh,ylim=c(-1,1))
pacf(d1lngnpsh,ylim=c(-1,1))


