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

##-- Legim la sèrie (desestacionalitzada)
gnpsh <- read.table("../Dades/GNPSH.DAT")
gnpsh

##-- Creem l'objecte "ts" indicant inici i freqüència i el dibuixem
gnpsh <- ts(gnpsh, start = 1947, frequency = 4)


##-- Fem la transformació logaritme per fer la variància constant i les diferenciacions
## per fer-la estacionaria
lngnpsh      <- log(gnpsh)
d1lngnpsh    <- diff(lngnpsh)
d1d1lngnpsh  <- diff(d1lngnpsh)

par(mfrow=c(2,2))
plot(gnpsh,       main = 'gnpsh')
plot(lngnpsh,     main = 'lngnpsh')
plot(d1lngnpsh,   main = 'd1lngnpsh')
plot(d1d1lngnpsh, main = 'd1d1lngnpsh')

##-- ACF i PACF
par(mfrow=c(1,2))
 acf(d1lngnpsh, ylim=c(-1,1), lag.max = 40)
pacf(d1lngnpsh, ylim=c(-1,1), lag.max = 40)

#-------------------------------------------------------------------------------
# Models
#-------------------------------------------------------------------------------


##-- Model 1 -------------------------------------------------------------------
gnpsh.arima1 <- arima(lngnpsh, order = c(3,1,0))
gnpsh.arima1

##-- Significatius?
ratios <- round(abs(gnpsh.arima1$coef/sqrt(diag(gnpsh.arima1$var.coef))),2)
ratios
ratios>2

##-- Model 2 -------------------------------------------------------------------
gnpsh.arima2 <- arima(lngnpsh, order    = c(2,1,0))

##-- Significatius?
ratios <- round(abs(gnpsh.arima2$coef/sqrt(diag(gnpsh.arima2$var.coef))),2)
ratios
ratios>2

##-- Model definitiu -----------------------------------------------------------
AIC(gnpsh.arima1)
AIC(gnpsh.arima2)

mod_def <- gnpsh.arima2


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

##-- Alternativa: all in one ---------------------------------------------------
library(forecast)
checkresiduals(mod_def)

#-------------------------------------------------------------------------------
# Prediccio
#-------------------------------------------------------------------------------
# Calcul de les prediccions i de l'error estandard
pred   <- predict(mod_def, n.ahead=16)
pr_log <- pred$pred
se_log <- pred$se

# Intervals de logs
li_log <- pr_log - 1.96 * se_log # limit inferior log
ls_log <- pr_log + 1.96 * se_log # limit superior log

# Desfer logaritmes
li <- ts(exp(li_log), start = 1991, freq=4)
pr <- ts(exp(pr_log), start = 1991, freq=4)
ls <- ts(exp(ls_log), start = 1991, freq=4)

# Grafic
par(mfrow=c(1,1))
ts.plot(gnpsh,
        li, ls, pr,
        lty  = c(1,2,2,1), 
        col  = c("black","blue","blue","red"),
        xlim = c(1947,1993))

##-- Alternativa: with logs ----------------------------------------------------
autoplot(forecast(mod_def))
