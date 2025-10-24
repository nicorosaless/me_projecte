################################################################################
#
# ME - GIA. Estimacio, validacio i prediccio series temporals
#
#--------------------------------------
# Conceptes
#--------------------------------------
#
# Estimacio
# Validacio
# Prediccio
################################################################################

#-------------------------------------------------------------------------------
# Exploració
#-------------------------------------------------------------------------------

##-- Legim la sèrie
airpass <- read.table("../Dades/airpass.dat")
airpass

##-- Creem l'objecte "ts" indicant inici i freqüència i el dibuixem
airpass <- ts(AirPassengers, start=1949, frequency=12)

##-- Fem la transformació logaritme per fer la variància constant i les diferenciacions
## per fer-la estacionaria
lnairpass      <- log(airpass)
d12lnairpass   <- diff(lnairpass,lag=12)
d1d12lnairpass <- diff(  d12lnairpass)

par(mfrow=c(2,2))
plot(airpass,        main = 'airpass')
plot(lnairpass,      main = 'lnairpass')
plot(d12lnairpass,   main = 'd12lnairpass')
plot(d1d12lnairpass, main = 'd1d12lnairpass')

##-- ACF i PACF
par(mfrow=c(1,2))
 acf(d1d12lnairpass, ylim=c(-1,1), lag.max = 40)
pacf(d1d12lnairpass, ylim=c(-1,1), lag.max = 40)

#-------------------------------------------------------------------------------
# Models
#-------------------------------------------------------------------------------


##-- Model 1 -------------------------------------------------------------------
airpass.arima1a <- arima(lnairpass,
                        order    = c(3,1,3), 
                        seasonal = list(order = c(1,1,1), 
                                        period = 12), include.mean = FALSE)
airpass.arima1b <- arima(d1d12lnairpass,
                        order    = c(3,0,3), 
                        seasonal = list(order = c(1,0,1), 
                                        period = 12), include.mean = FALSE)

##-- Equivalents
airpass.arima1a
airpass.arima1b

##-- Significatius?
ratios <- round(abs(airpass.arima1a$coef/sqrt(diag(airpass.arima1a$var.coef))),2)
ratios
ratios>2

##-- Model 2 -------------------------------------------------------------------

airpass.arima2 <- arima(lnairpass,
                        order    = c(3,1,3), 
                        seasonal = list(order = c(0,1,1), 
                                        period = 12), include.mean = FALSE)
##-- Significatius?
ratios <- round(abs(airpass.arima2$coef/sqrt(diag(airpass.arima2$var.coef))),2)
ratios
ratios>2

##-- Model 3 -------------------------------------------------------------------
airpass.arima3 <- arima(lnairpass,
                        order    = c(2,1,3), 
                        seasonal = list(order = c(0,1,1), 
                                        period = 12), include.mean = FALSE)

##-- Significatius?
ratios <- round(abs(airpass.arima3$coef/sqrt(diag(airpass.arima3$var.coef))),2)
ratios
ratios>2

##-- Model definitiu -----------------------------------------------------------
AIC(airpass.arima1a)
AIC(airpass.arima2)
AIC(airpass.arima3)

mod_def <- airpass.arima3


#-------------------------------------------------------------------------------
##-- Validacio
#-------------------------------------------------------------------------------
##-- Homoscedasticitat ---------------------------------------------------------
resid <- mod_def$residuals
par(mfrow=c(1,2), mar=c(3,3,3,3))
plot(resid, main="Residuals")
abline(h = c(0 , -3*sd(resid), 3*sd(resid)), lty = c(1,3,3), col=c(1,4,4))
scatter.smooth(sqrt(abs(resid)), 
               main="Square Root of Absolute residuals",
               lpars = list(col=2))

#-- Normalitat  ----------------------------------------------------------------
par(mfrow=c(1,2), mar=c(3,3,3,3))
qqnorm(resid)
qqline(resid,col=2,lwd=2)
hist(resid, breaks = 10, freq=F)
curve(dnorm(x, mean = mean(resid), sd = sd(resid)), col=2, add=T)

##-- Independencia  ------------------------------------------------------------
tsdiag(mod_def, gof.lag = 20)

##-- Altarnativa: all in one ---------------------------------------------------
library(forecast)
checkresiduals(mod_def)

#-------------------------------------------------------------------------------
# Prediccio
#-------------------------------------------------------------------------------
# Calcul de les prediccions i de l'error estandard
pred   <- predict(mod_def, n.ahead=24)
pr_log <- pred$pred
se_log <- pred$se

# Intervals de logs
li_log <- pr_log - 1.96 * se_log # limit inferior log
ls_log <- pr_log + 1.96 * se_log # limit superior log

# Desfer logaritmes
li <- ts(exp(li_log), start = 1961, freq=12)
pr <- ts(exp(pr_log), start = 1961, freq=12)
ls <- ts(exp(ls_log), start = 1961, freq=12)

# Grafic
par(mfrow=c(1,1))
ts.plot(airpass,
        li, ls, pr,
        lty  = c(1,2,2,1), 
        col  = c("black","blue","blue","red"),
        xlim = c(1949,1962))


