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

#pdf("CoupledTimeSeries.pdf", height=5, width=6)
plot(c(1, dim(tData)[1]), c(-10, 10),
     xlab="Time",
     ylab="Score",
     type='n')
lines(c(1:dim(tData)[1]), tData[,1], type='p', lwd=2, col='red')
lines(c(1:dim(tData)[1]), tData[,2], type='p', lwd=2, col='blue')
lines(c(1, dim(tData)[1]), c(-0, 0), type='l', lty=1, col=1)
#dev.off()



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



##### with KZ data with 1 Dyad #####

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

#kzData1b <- subset(kzData1, phase =="femalereceive")


tData2 <- as.data.frame(cbind(kzData1b$rsa.basec, kzData1b$b.rsa.basec))
colnames(tData2) <- c("x", "y")

acf(tData2)



# with IBI data 

#kzData2 <- read.csv("/Users/zeekatherine/Desktop/R Files/RCC_Data/RCC_physio_shortintervals/RCC_168_5sec.csv")
#kzData2$ibi.wc <- kzData2$ibi - kzData2$ibibase
#kzData2$b.ibi.wc <- kzData2$b.ibi - kzData2$b.ibibase
#kzData2b <- subset (kzData2, phase == "femaleprovide")

#tData <- as.data.frame(cbind(kzData2b$ibi.wc, kzData2b$b.ibi.wc))
#colnames(tData) <- c("x", "y")
#acf(tData)

# ----------------------------------
# Time-delay embed the data.

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


##### with KZ data with multiple Dyads #####

kzData1 <- read.csv("rcc_physio_subset_TEMP_forDSM.csv")

#tData <- as.data.frame(cbind(kzData1$ibi.wc, kzData1$b.ibi.wc))
tData3 <- as.data.frame(cbind(kzData1$rsa.basec, kzData1$b.rsa.basec))
colnames(tData3) <- c("x", "y")

acf(tData3)
# ----------------------------------
# Time-delay embed the data.

embedD3 <- 12
theTau3 <- 1
deltaT3 <- 1

numIndicators3 <- 2

# ----------------------------------
# Time-delay embed the data.

tEmbedded3 <- cbind(gllaEmbed(tData3[,1], embed=embedD3, tau=theTau3, label="x", idColumn=FALSE),
                   gllaEmbed(tData3[,2], embed=embedD3, tau=theTau3, label="y", idColumn=FALSE))

# ----------------------------------
# Create the fixed LDE loading matrix.

L13 <- rep(1,embedD3)
L23 <- c(1:embedD3)*theTau3*deltaT3-mean(c(1:embedD3)*theTau3*deltaT3)
L33 <-  (L23^2)/2
LMatrix3 <- cbind(L13,L23,L33)

# ----------------------------------
# Create a 2nd order Multivariate LDE model.

manifestVars3 <- dimnames(tEmbedded3)[[2]]
ldeModelCoupled1_3 <- mxModel("LDE_Coupled_Model_1_3",
                            mxMatrix("Iden", 2, name="I2"),
                            mxMatrix("Full",  
                                     values=LMatrix3, 
                                     free=FALSE, 
                                     name="LFixed", 
                                     byrow=TRUE
                            ),
                            mxMatrix("Zero", embedD3*numIndicators3, 2, name="Z"),
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
                            mxMatrix("Diag", embedD3*numIndicators3, embedD3*numIndicators3, 
                                     values=.8, 
                                     free=TRUE, 
                                     labels=c(rep("uX", embedD3), rep("uY", embedD3)), 
                                     name="U",
                                     lbound=0.000001
                            ),
                            mxMatrix("Iden", 8, name="I"),
                            mxAlgebra(L %*% solve(I-A) %*% S %*% t(solve(I-A)) %*% t(L) + U, 
                                      name="R", 
                                      dimnames = list(manifestVars3, manifestVars3)
                            ),
                            mxExpectationNormal(covariance="R"),
                            mxFitFunctionML(),
                            mxData(cov(tEmbedded3), 
                                   type="cov", 
                                   numObs=dim(tEmbedded3)[1]
                            )
)

# ----------------------------------
# Fit the LDE model and examine the summary results.

ldeModel1CoupledFit_3 <- mxRun(ldeModelCoupled1_3)

summary(ldeModel1CoupledFit_3)

