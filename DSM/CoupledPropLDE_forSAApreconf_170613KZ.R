# ---------------------------------------------------------------------
# Program: CoupledPropLDEUnivariateExample.R
#  Author: Steve Boker
#    Date: Sun May 14 14:18:01 EDT 2017
#
#
# ---------------------------------------------------------------------

setwd("~/KZNB/DSM")


# ----------------------------------
# Read libraries and set options.

options(width=110)

library(OpenMx)
library(psych)

source("GLLAfunctions.R")
mxOption(NULL, 'Default optimizer', 'NPSOL')




##### with KZ data with 1 Dyad - Male Receive/Female Provide#####



mean.na <- function (x) {
  out <- mean(x, na.rm=T)
  out
}


kzData1 <- read.csv("RCC_168_10sec.csv")

# respiratory sinus arrhythmia (RSA), centered on baseline RSA
kzData1<- within(kzData1, {rsabase.mean = ave(rsa, phase[kzData1$phase=="baseline"], FUN = mean.na)})
kzData1<- within(kzData1, {b.rsabase.mean = ave(b.rsa, phase[kzData1$phase=="baseline"], FUN = mean.na)})


kzData1<- within(kzData1, {ibibase.mean = ave(ibi, phase[kzData1$phase=="baseline"], FUN = mean.na)})
kzData1<- within(kzData1, {b.ibibase.mean = ave(b.ibi, phase[kzData1$phase=="baseline"], FUN = mean.na)})

kzData1$rsa.basec <- kzData1$rsa - kzData1$rsabase.mean
kzData1$b.rsa.basec <- kzData1$b.rsa - kzData1$b.rsabase.mean
kzData1b <- subset(kzData1, phase =="femaleprovide")
kzData1b$time.r <- kzData1b$time - 90
kzData1b$base <- 0

tData2 <- as.data.frame(cbind(kzData1b$rsa.basec, kzData1b$b.rsa.basec))
colnames(tData2) <- c("x", "y")

acf(tData2)



#pdf("Raw_Data_168_femaleprovide.pdf", height = 6, width = 8)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Female Provide / Male Receive", col = "white")
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1b$time.r, kzData1b$base, type='l', lty=1, col=1)
#dev.off()


#pdf("Raw_Data_168_femaleprovide_withlines.pdf", height = 6, width = 8)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Female Provide / Male Receive", col = "white")
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1b$time.r, kzData1b$base, type='l', lty=1, col=1)
lines(kzData1b$time.r[order(kzData1b$time.r)], 
      kzData1b$rsa.basec[order(kzData1b$time)], 
      xlim=range(kzData1b$time), ylim=range(kzData1b$rsa.basec), 
      pch=16, col = "deeppink", lwd = 2)
lines(kzData1b$time.r[order(kzData1b$time.r)], 
      kzData1b$b.rsa.basec[order(kzData1b$time)], 
      xlim=range(kzData1b$time), ylim=range(kzData1b$b.rsa.basec), 
      pch=16, col = "blue", lwd = 2)
#dev.off()



# ----------------------------------
# Time-delay embed the data.
# originally used embed of 5
embedD2 <- 4
theTau2 <- 1
deltaT2 <- 1

numIndicators2 <- 2

# ----------------------------------
# Time-delay embed the data.

tEmbedded2 <- cbind(gllaEmbed(tData2[,1], embed=embedD2, tau=theTau2, label="x", idColumn=FALSE),
                    gllaEmbed(tData2[,2], embed=embedD2, tau=theTau2, label="y", idColumn=FALSE))

# ----------------------------------
# Create the fixed LDE loading matrix.

L12 <- rep(1,embedD2)
L22 <- c(1:embedD2)*theTau2*deltaT2-mean(c(1:embedD2)*theTau2*deltaT2)
L32 <-  (L22^2)/2
LMatrix2 <- cbind(L12,L22,L32)

# ----------------------------------
# Create a 2nd order Multivariate LDE model.

manifestVars2 <- dimnames(tEmbedded2)[[2]]
ldeModelCoupled1_2 <- mxModel("LDE_Coupled_Model_1_2",
                              mxMatrix("Iden", 2, name="I2"),
                              mxMatrix("Full",  
                                       values=LMatrix2, 
                                       free=FALSE, 
                                       name="LFixed", 
                                       byrow=TRUE
                              ),
                              mxMatrix("Zero", embedD2*numIndicators2, 2, name="Z"),
                              mxAlgebra(cbind(I2 %x% LFixed, Z), name="L"),
                              mxMatrix("Full", 8, 8, 
                                       values=c(  0,  0,  0,  0,  0,  0,  0,  0,
                                                  0,  0,  0,  0,  0,  0,  0,  0,
                                                  0,  0,  0,  0,  0,  0,  1,  .1,
                                                  0,  0,  0,  0,  0,  0,  0,  0,
                                                  0,  0,  0,  0,  0,  0,  0,  0,
                                                  0,  0,  0,  0,  0,  0,  .1,  1,
                                                  -.2,-.2,  0,  0,  0,  0,  0,  0,
                                                  0,  0,  0,-.2,-.2,  0,  0,  0), 
                                       labels=c(    NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                    NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                    NA,     NA,     NA,     NA,     NA,     NA,     NA,"gammaX",
                                                    NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                    NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                    NA,     NA,     NA,     NA,     NA,     NA,"gammaY",    NA,
                                                    "etaX","zetaX",     NA,     NA,     NA,     NA,     NA,     NA,
                                                    NA,     NA,     NA, "etaY","zetaY",     NA,     NA,     NA), 
                                       free=c( F,F,F,F,F,F,F,F,
                                               F,F,F,F,F,F,F,F,
                                               F,F,F,F,F,F,F,T,
                                               F,F,F,F,F,F,F,F,
                                               F,F,F,F,F,F,F,F,
                                               F,F,F,F,F,F,T,F,
                                               T,T,F,F,F,F,F,F,
                                               F,F,F,T,T,F,F,F), 
                                       ubound=c(   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     .4,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     .4,    NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA), 
                                       lbound=c(   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,    -.4,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,    -.4,    NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA,
                                                   NA,     NA,     NA,     NA,     NA,     NA,     NA,     NA), 
                                       name="A", 
                                       byrow=TRUE
                              ),
                              mxMatrix("Symm", 8, 8,
                                       values=c(  .8,
                                                  0, .8,
                                                  0, 0, .8,
                                                  -.1, -.1, 0, .8,
                                                  -.1, -.1, 0, 0, .8,
                                                  0, 0, 0, 0, 0, .8,
                                                  0, 0, 0, 0, 0, 0, 0,
                                                  0, 0, 0, 0, 0, 0, 0, 0), 
                                       free=c( T,
                                               F, T,
                                               F, F, T,
                                               T, T, F, T,
                                               T, T, F, F, T,
                                               F, F, F, F, F, T,
                                               F, F, F, F, F, F, F,
                                               F, F, F, F, F, F, F, F), 
                                       labels=c("VX",
                                                NA, "VdX",
                                                NA, NA, "Vd2X",
                                                NA, NA, NA, "VY",
                                                NA, NA, NA, NA, "VdY",
                                                NA, NA, NA, NA, NA, "Vd2Y", 
                                                NA, NA, NA, NA, NA, NA, NA, 
                                                NA, NA, NA, NA, NA, NA, NA, NA), 
                                       name="S", 
                                       byrow=TRUE,
                                       lbound=c(0.00000001,
                                                NA, 0.00000001,
                                                NA, NA, 0.00000001,
                                                NA, NA, NA, 0.00000001,
                                                NA, NA, NA, NA, 0.00000001,
                                                NA, NA, NA, NA, NA, 0.00000001,
                                                NA, NA, NA, NA, NA, NA, NA, 
                                                NA, NA, NA, NA, NA, NA, NA, NA), 
                              ),
                              mxMatrix("Diag", embedD2*numIndicators2, embedD2*numIndicators2, 
                                       values=.8, 
                                       free=TRUE, 
                                       labels=c(rep("uX", embedD2), rep("uY", embedD2)), 
                                       name="U",
                                       lbound=0.000001
                              ),
                              mxMatrix("Iden", 8, name="I"),
                              mxAlgebra(L %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(L) + U, 
                                        name="R", 
                                        dimnames = list(manifestVars2, manifestVars2)
                              ),
                              mxExpectationNormal(covariance="R"),
                              mxFitFunctionML(),
                              mxData(cov(tEmbedded2), 
                                     type="cov", 
                                     numObs=dim(tEmbedded2)[1]
                              )
)

# ----------------------------------
# Fit the LDE model and examine the summary results.

ldeModel1CoupledFit_2 <- mxRun(ldeModelCoupled1_2)

summary(ldeModel1CoupledFit_2)



##### KZ data Male Receive Prediction Plot #####

library(deSolve)

# ----------------------------------
# Set constants.

totalSamples <- 1000
totalInterval <- 30
deltaT <- totalInterval / totalSamples

theTimes  <- c(1:30)
theTimes2  <- seq(1, totalInterval, length=totalSamples)  # the measurement occasions


# ----------------------------------
# Define the damped linear oscillator model.
#   Note that y(t) = dx(t)/dt as in the Mathematica model
#   Thus d^2x(t)/dt = eta * x(t) + zeta * dx(t)/dt

DLOmodel <- function(t, prevState, parms) {
  x <- prevState[1] # x[t]
  y <- prevState[2] # dx[t]
  
  with(as.list(parms), {
    dx <- y
    dy <- parms[1]*x + parms[2]*y
    res<-c(dx,dy)
    list(res)
  }
  )
}

# ----------------------------------
# Simulate and plot one oscillator.

eta_malereceiveX <- -1.53
zeta_malereceiveX <- 0 # set to 0, but zeta from model is .22
xstart_malereceiveX <- c(x = 1, y = 0.25107549) # not sure this Y start val is correct


out1_malereceiveX <- as.data.frame(lsoda(xstart_malereceiveX, theTimes2, DLOmodel, parms=c(eta_malereceiveX, zeta_malereceiveX)))

#pdf("predictionplot_malereceive_femalepartner_dots.pdf", height=5, width=5)
plot(c(min(theTimes), max(theTimes)), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nFemale Partner")
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='p', lwd=2, col="gray")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')

#dev.off()



#pdf("predictionplot_malereceive_femalepartner_lines.pdf", height=6, width=5)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nFemale Partner")
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='l', lwd=5, col="pink")
lines(c(min(theTimes2), max(theTimes2)), c(-0, 0), type='l', col=1)
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
#dev.off()


#pdf("predictionplot_malereceive_femalepartner_noraw.pdf", height=6, width=5)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nFemale Partner")
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='l', lwd=5, col="pink")
lines(c(min(theTimes2), max(theTimes2)), c(-0, 0), type='l', col=1)
#dev.off()



## for Male Partner:
eta_malereceiveY <- -.77
zeta_malereceiveY <- 0
xstart_malereceiveY <- c(x = 1, y = -0.6503902)


out1_malereceiveY <- as.data.frame(lsoda(xstart_malereceiveY, theTimes2, DLOmodel, parms=c(eta_malereceiveY, zeta_malereceiveY)))

#pdf("predictionplot_malereceive_malepartner_dots.pdf", height=6, width=5)
plot(c(min(theTimes), max(theTimes)), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nMale Partner")
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='p', lwd=2, col="gray")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
#dev.off()


#pdf("predictionplot_malereceive_malepartner_lines.pdf", height=6, width=5)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nMale Partner")
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='l', lwd=5, col="lightblue")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
#dev.off()

#cor.test(out1_malereceiveY$x, kzData1b$b.rsa.basec)

#pdf("predictionplot_malereceive_malepartner_noraw.pdf", height=6, width=5)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Female Provide / Male Receive \nMale Partner")
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='l', lwd=5, col="lightblue")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
#dev.off()



## both partners on same plot
#pdf("predictionplot_malereceive_both.pdf", height=6, width=8)
plot(c(1, 30), c(-5, 5),
     xlab="Time",
     ylab="RSA (Baseline Centered)",
     type='n', main = "Male Receive / Female Provide")
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='l', lwd=5, col="lightblue")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='l', lwd=5, col="pink")
lines(c(min(theTimes2), max(theTimes2)), c(-0, 0), type='l', col=1)
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
#dev.off()







###### Models using "conventional" analyses ##########

dyaddata <- read.csv("RCC_168_10sec_dyadic.csv")

# respiratory sinus arrhythmia (RSA), centered on baseline RSA
dyaddata<- within(dyaddata, {rsabase.mean = ave(rsa, phase[dyaddata$phase=="baseline"], FUN = mean.na)})
dyaddata<- within(dyaddata, {b.rsabase.mean = ave(b.rsa, phase[dyaddata$phase=="baseline"], FUN = mean.na)})


dyaddata<- within(dyaddata, {ibibase.mean = ave(ibi, phase[dyaddata$phase=="baseline"], FUN = mean.na)})
dyaddata<- within(dyaddata, {b.ibibase.mean = ave(b.ibi, phase[dyaddata$phase=="baseline"], FUN = mean.na)})

dyaddata$rsa.basec <- dyaddata$rsa - dyaddata$rsabase.mean
dyaddata$b.rsa.basec <- dyaddata$b.rsa - dyaddata$b.rsabase.mean

dyaddata<- within(dyaddata, {rsa.mean = ave(rsa, partner, FUN = mean.na)})
dyaddata$rsa.wc <- dyaddata$rsa - dyaddata$rsa.mean
dyaddata <- within(dyaddata, {rsa.basec.lag = ave(rsa.wc, partner, phase, FUN = dplyr::lag)})

dyaddata<- within(dyaddata, {b.rsa.mean = ave(b.rsa,partner, FUN = mean.na)})
dyaddata$b.rsa.wc <- dyaddata$b.rsa - dyaddata$b.rsa.mean
dyaddata <- within(dyaddata, {b.rsa.basec.lag = ave(b.rsa.wc, partner, phase, FUN = dplyr::lag)})


#### rsa as predicted by prior rsa


### male provide/female receive
dyaddata$pa <- ifelse(dyaddata$partner == "female", 1, 0)
dyaddata$pb <- ifelse(dyaddata$partner == "male", 1, 0)
dyaddata$dyad <- 1

maleprov <- lm(b.rsa.basec ~ 0+ pa + pb + pa:rsa.basec + pa:rsa.basec.lag + pa:b.rsa.basec.lag +
                 pb:rsa.basec + pb:rsa.basec.lag + pb:b.rsa.basec.lag, data = 
                 subset(dyaddata, phase == "femalereceive"))
summary(maleprov)


#without cross partner lag
maleprov2 <- lm(b.rsa.basec ~ 0+ pa + pb + pa:rsa.basec + pa:b.rsa.basec.lag +
                  pb:rsa.basec + pb:b.rsa.basec.lag, data = 
                  subset(dyaddata, phase == "femalereceive"))
summary(maleprov2)


### female provide/male receive

femaleprov <- lm(b.rsa.basec ~ 0+ pa + pb + pa:rsa.basec + pa:rsa.basec.lag + pa:b.rsa.basec.lag +
                   pb:rsa.basec + pb:rsa.basec.lag + pb:b.rsa.basec.lag, data = 
                   subset(dyaddata, phase == "femaleprovide"))
summary(femaleprov)


femaleprov2 <- lm(b.rsa.basec ~ 0+ pa + pb + pa:rsa.basec + pa:b.rsa.basec.lag +
                    pb:rsa.basec + pb:b.rsa.basec.lag, data = 
                    subset(dyaddata, phase == "femaleprovide"))
summary(femaleprov2)