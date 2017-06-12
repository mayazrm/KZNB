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


##### Example with Simulated Data #####

# ----------------------------------
# Read the data.



tData <- read.csv("CoupledLDE.csv", header=TRUE)
describe(tData)

# ----------------------------------
# Plot the time series.

##pdf("CoupledTimeSeries.#pdf", height=5, width=6)
plot(c(1, dim(tData)[1]), c(-10, 10),
     xlab="Time",
     ylab="Score",
     type='n')
lines(c(1:dim(tData)[1]), tData[,1], type='p', lwd=2, col='red')
lines(c(1:dim(tData)[1]), tData[,2], type='p', lwd=2, col='blue')
lines(c(1, dim(tData)[1]), c(-0, 0), type='l', lty=1, col=1)
##dev.off()



# ----------------------------------
# Time-delay embed the data.

embedD <- 7
theTau <- 1
deltaT <- 1

numIndicators <- 2

# ----------------------------------
# Time-delay embed the data.

tEmbedded <- cbind(gllaEmbed(tData[,1], embed=embedD, tau=theTau, label="x", idColumn=FALSE),
                   gllaEmbed(tData[,2], embed=embedD, tau=theTau, label="y", idColumn=FALSE))

# ----------------------------------
# Create the fixed LDE loading matrix.

L1 <- rep(1,embedD)
L2 <- c(1:embedD)*theTau*deltaT-mean(c(1:embedD)*theTau*deltaT)
L3 <-  (L2^2)/2
LMatrix <- cbind(L1,L2,L3)

# ----------------------------------
# Create a 2nd order Multivariate LDE model.

manifestVars <- dimnames(tEmbedded)[[2]]

ldeModelCoupled1 <- mxModel("LDE_Coupled_Model_1",
    mxMatrix("Iden", 2, name="I2"),
    mxMatrix("Full",  
        values=LMatrix, 
        free=FALSE, 
        name="LFixed", 
        byrow=TRUE
    ),
    mxMatrix("Zero", embedD*numIndicators, 2, name="Z"),
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
    mxMatrix("Diag", embedD*numIndicators, embedD*numIndicators, 
        values=.8, 
        free=TRUE, 
        labels=c(rep("uX", embedD), rep("uY", embedD)), 
        name="U",
        lbound=0.000001
    ),
    mxMatrix("Iden", 8, name="I"),
    mxAlgebra(L %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(L) + U, 
        name="R", 
        dimnames = list(manifestVars, manifestVars)
    ),
    mxExpectationNormal(covariance="R"),
    mxFitFunctionML(),
    mxData(cov(tEmbedded), 
        type="cov", 
        numObs=dim(tEmbedded)[1]
    )
)

# ----------------------------------
# Fit the LDE model and examine the summary results.

ldeModel1CoupledFit <- mxRun(ldeModelCoupled1)

summary(ldeModel1CoupledFit)



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



##pdf("Raw_Data_178_femaleprovide.#pdf", height = 6, width = 5)
plot(c(1, 30), c(-3, 3),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Female Provide / Male Receive", col = "white")
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1b$time.r, kzData1b$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1b$time.r, kzData1b$base, type='l', lty=1, col=1)
##dev.off()


#pdf("Raw_Data_178_femaleprovide_withlines.#pdf", height = 6, width = 5)
  plot(c(1, 30), c(-3, 3),
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

embedD2 <- 5 # embed of 5 seems to work best for female receive/male provide, but 4 is better for male receive/female provide
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

totalSamples <- 30
totalInterval <- 10
deltaT <- totalInterval / totalSamples

#theTimes  <- seq(0, totalInterval, length=totalSamples)  # the measurement occasions
theTimes  <- c(1:30)


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

eta_malereceiveX <- -1.45
zeta_malereceiveX <- .2 # had to adjust this. Using est from model yielded enormous values
xstart_malereceiveX <- c(x = 0.25107549, y = -0.6503902) # not sure this Y start val is correct
# xstart_malereceiveX <- c(x = 1, y = 0.25107549) 


out1_malereceiveX <- as.data.frame(lsoda(xstart_malereceiveX, theTimes, DLOmodel, parms=c(eta_malereceiveX, zeta_malereceiveX)))

pdf("predictionplot_malereceive_femalepartner_dots.pdf", height=5, width=5)
plot(c(min(theTimes), max(theTimes)), c(-10, 11),
     xlab="Time",
     ylab="X",
     type='n')
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='p', lwd=2, col=2)
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', lty=2, col=1)
dev.off()



pdf("predictionplot_malereceive_femalepartner_lines.pdf", height=5, width=5)
plot(c(1, 30), c(-10, 11),
     xlab="X",
     ylab="dX/dt",
     type='n')
lines(out1_malereceiveX$time, out1_malereceiveX$x, type='l', lwd=3, col="deeppink")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
dev.off()




plot(c(1, 30), c(-3, 3),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Female Provide / Male Receive", col = "white")
lines(kzData1b$time.r, kzData1b$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1b$time.r, kzData1b$base, type='l', lty=1, col=1)
lines(kzData1b$time.r[order(kzData1b$time.r)], 
      kzData1b$rsa.basec[order(kzData1b$time)], 
      xlim=range(kzData1b$time), ylim=range(kzData1b$rsa.basec), 
      pch=16, col = "deeppink", lwd = 2)







## for Male Partner:


eta_malereceiveY <- -.71
zeta_malereceiveY <- .08
xstart_malereceiveY <- c(x = 0.25107549, y = -0.6503902)


out1_malereceiveY <- as.data.frame(lsoda(xstart_malereceiveY, theTimes, DLOmodel, parms=c(eta_malereceiveY, zeta_malereceiveY)))

pdf("predictionplot_malereceive_malepartner_dots.pdf", height=5, width=5)
plot(c(min(theTimes), max(theTimes)), c(-10, 11),
     xlab="Time",
     ylab="X",
     type='n')
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='p', lwd=2, col=2)
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', lty=2, col=1)
dev.off()



pdf("predictionplot_malereceive_malepartner_lines.pdf", height=5, width=5)
plot(c(1, 30), c(-10, 11),
     xlab="X",
     ylab="dX/dt",
     type='n')
lines(out1_malereceiveY$time, out1_malereceiveY$x, type='l', lwd=3, col="blue")
lines(c(min(theTimes), max(theTimes)), c(-0, 0), type='l', col=1)
dev.off()





##### with KZ data with 1 Dyad - Female Receive/Male Provide #####

kzData1c <- subset(kzData1, phase =="femalereceive")
kzData1c$base <- 0
kzData1c$time.r <- kzData1c$time - 60 

tData2b <- as.data.frame(cbind(kzData1c$rsa.basec, kzData1c$b.rsa.basec))
colnames(tData2b) <- c("x", "y")

acf(tData2b)






#pdf("Raw_Data_178_maleprovide.#pdf", height = 6, width = 5)
plot(c(1, 30), c(-3, 3),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Male Provide / Female Receive", col = "white")
lines(kzData1c$time.r, kzData1c$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1c$time.r, kzData1c$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1c$time.r, kzData1c$base, type='l', lty=1, col=1)
#dev.off()


#pdf("Raw_Data_178_maleprovide_withlines.#pdf", height = 6, width = 5)
plot(c(1, 30), c(-3, 3),
     xlab="Time",
     ylab="RSA (Baseline Centered)", main = "Male Provide / Female Receive", col = "white")
lines(kzData1c$time.r, kzData1c$rsa.basec, type='p', lwd=2, col='deeppink')
lines(kzData1c$time.r, kzData1c$b.rsa.basec, type='p', lwd=2, col='blue')
lines(kzData1c$time.r, kzData1c$base, type='l', lty=1, col=1)
lines(kzData1c$time.r[order(kzData1c$time.r)], 
      kzData1c$rsa.basec[order(kzData1c$time)], 
      xlim=range(kzData1c$time), ylim=range(kzData1c$rsa.basec), 
      pch=16, col = "deeppink", lwd = 2)
lines(kzData1c$time.r[order(kzData1c$time.r)], 
      kzData1c$b.rsa.basec[order(kzData1c$time)], 
      xlim=range(kzData1c$time), ylim=range(kzData1c$b.rsa.basec), 
      pch=16, col = "blue", lwd = 2)
#dev.off()



# ----------------------------------
# Time-delay embed the data.

tEmbedded2b <- cbind(gllaEmbed(tData2b[,1], embed=embedD2, tau=theTau2, label="x", idColumn=FALSE),
                    gllaEmbed(tData2b[,2], embed=embedD2, tau=theTau2, label="y", idColumn=FALSE))

# ----------------------------------
# Create the fixed LDE loading matrix.

L12b <- rep(1,embedD2)
L22b <- c(1:embedD2)*theTau2*deltaT2-mean(c(1:embedD2)*theTau2*deltaT2)
L32b <-  (L22^2)/2
LMatrix2b <- cbind(L12b,L22b,L32b)

# ----------------------------------
# Create a 2nd order Multivariate LDE model.

manifestVars2b <- dimnames(tEmbedded2b)[[2]]
ldeModelCoupled1_2b <- mxModel("LDE_Coupled_Model_1_2b",
                              mxMatrix("Iden", 2, name="I2"),
                              mxMatrix("Full",  
                                       values=LMatrix2b, 
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
                                        dimnames = list(manifestVars2b, manifestVars2b)
                              ),
                              mxExpectationNormal(covariance="R"),
                              mxFitFunctionML(),
                              mxData(cov(tEmbedded2b), 
                                     type="cov", 
                                     numObs=dim(tEmbedded2b)[1]
                              )
)

# ----------------------------------
# Fit the LDE model and examine the summary results.

ldeModel1CoupledFit_2b <- mxRun(ldeModelCoupled1_2b)

summary(ldeModel1CoupledFit_2b)


