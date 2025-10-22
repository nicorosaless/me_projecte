################################################################################
#
# ME - GIA. Introducció a serie temporals
#
#--------------------------------------
# Conceptes
#--------------------------------------
#
# Models ARMA
################################################################################

#-------------------------------------------------------------------------------
# AR(p)
#-------------------------------------------------------------------------------

##-- AR(p=1)
model.ar <- c(0.9)

##-- Simulacio del model AR
ser <- ts(arima.sim(list(ar = model.ar), 100))
plot(ser)

##-- ACF teoric versus mostral -------------------------------------------------
par(mfrow=c(1,2))

# ACF teoric del model
model.acf <- ARMAacf(ar = model.ar, lag.max = 40)
plot(model.acf, type="h", ylim=c(-1,1))
abline(h = 0)

# ACF mostral de la serie
acf(ser, lag.max=40, ylim=c(-1,1))

##-- PACF teoric versus mostral ------------------------------------------------
par(mfrow=c(1,2))

# PACF teoric del model
model.pacf <- ARMAacf(ar=model.ar, lag.max=40, pacf=T)
plot(model.pacf, type="h", ylim=c(-1,1))
abline(h=0)

# PACF mostral de la serie
pacf(ser, lag.max=40,ylim=c(-1,1))

# Expressio com a AR infinit
pis <- -ARMAtoMA(ma=-model.ar, lag.max=10)
pis

# Expressio com a MA infinit
psis <- ARMAtoMA(ar=model.ar, lag.max=10)
psis

# Arrels del polinomi carcteristic de la part AR
Mod(polyroot(c(1,-model.ar)))

#-------------------------------------------------------------------------------
# MA(q)
#-------------------------------------------------------------------------------

##-- MA(q=1)
model.ma <- c(-0.9)

##-- Simulacio del model
ser <- ts(arima.sim(list(ma = model.ma),200))
plot(ser)

##-- ACF teoric versus mostral -------------------------------------------------
par(mfrow=c(1,2))

##-- ACF teoric 
model.acf <- ARMAacf(ma = model.ma, lag.max = 40)
plot(model.acf, type = "h", ylim = c(-1,1), xlab = 'Lag')
abline(h=0)

# ACF mostral de la serie
acf(ser, lag.max = 40, ylim = c(-1,1))

##-- PACF teoric versus mostral ------------------------------------------------
par(mfrow=c(1,2))

# PACF teoric del model
model.pacf <- ARMAacf(ma = model.ma, lag.max=40, pacf=TRUE)
plot(model.pacf, type = "h", ylim = c(-1,1))
abline(h=0)

# PACF mostral de la serie
pacf(ser, lag.max = 40, ylim = c(-1,1))

##-- Expressio com a AR infinit ------------------------------------------------
pis <- -ARMAtoMA(ar = -model.ma, lag.max=10)
pis

##-- Expressio com a MA infinit ------------------------------------------------
psis <-  ARMAtoMA(ma = model.ma, lag.max=10)
psis

##-- Arrels del polinomi caracteristic de la part MA
Mod(polyroot(c(1,model.ma)))


#-------------------------------------------------------------------------------
# ARMA(p,q)
#-------------------------------------------------------------------------------

model.ar <- c(0.7)
model.ma <- c(0.6)

# Simulacio del model
ser <- ts(arima.sim(list(ar = model.ar,ma=model.ma),200))
plot(ser)

##-- ACF teoric versus mostral ------------------------------------------------
par(mfrow=c(1,2))

# ACF teoric
model.acf <- ARMAacf(ar = model.ar, ma = model.ma, lag.max=40)
plot(model.acf,type="h",ylim=c(-1,1))
abline(h=0)

# ACF mostral 
acf(ser,lag.max=40,ylim=c(-1,1))

##-- PACF teoric versus mostral ------------------------------------------------
par(mfrow=c(1,2))

# PACF teoric del model
model.pacf <- ARMAacf(ar=model.ar,ma=model.ma,lag.max=40,pacf=T)
plot(model.pacf, type="h", ylim=c(-1,1))
abline(h=0)

# PACF mostral de la serie
pacf(ser,lag.max=40,ylim=c(-1,1))

# Expressio com a AR infinit
pis <- -ARMAtoMA(ar=-model.ma, ma=-model.ar, lag.max=10)
pis

# Expressio com a MA infinit
psis <- ARMAtoMA(ar=model.ar,ma=model.ma,lag.max=10)
psis

# Arrels del polinomi caracteristic de la part AR
Mod(polyroot(c(1,-model.ar)))

# Arrels del polinomi caracteristic de la part MA
Mod(polyroot(c(1,model.ma)))
