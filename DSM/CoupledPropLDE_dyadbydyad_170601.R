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


##### Reading in Data  #####



tData_all <- read.csv("rcc_physio_subset_TEMP_forDSM.csv")
describe(tData_all)

# 14 dyads with complete HRV data
# 40 segments/couple (30 sec each):
## 1-10 = baseline
## 11-20 = goal discussion
## 21-30 = male receive/female provide support
## 31-40 = female receive/male provide support


tData <- data.frame(dyad=tData_all$dyad, rsa.basec = tData_all$rsa.basec, b.rsa.basec = tData_all$b.rsa.basec,
                    time = tData_all$time, base = 0)



# ----------------------------------
# Plot the time series for each dyad

# set df with unique dyad IDs

dyadids <- subset(tData_all, time ==1)
dyadids$dyad


pdf("Raw_RSA_Panel_Plots.pdf", height = 12, width = 12)
par(mfrow=c(4,4))
for (i in dyadids$dyad) {
plot(c(1, 40), c(-3, 3),
     xlab="Time",
     ylab="RSA", 
     main=paste(i), col = "white")
lines(tData$time[tData$dyad==i], tData$rsa.basec[tData$dyad==i], type='p', lwd=2, col='deeppink')
lines(tData$time[tData$dyad==i], tData$b.rsa.basec[tData$dyad==i], type='p', lwd=2, col='blue')
lines(tData$time[tData$dyad==i], tData$base[tData$dyad==i], type='l', lty=1, col=1)
}
dev.off()



pdf("ACF_Panel_Plots.pdf", height = 12, width = 12)
par(mfrow=c(4,4))
for (i in dyadids$dyad) {
  acf(tData$rsa.basec)[tData$dyad==i]
}
dev.off()



##### Setting Embedding #####


embedD <- 7
theTau <- 1
deltaT <- 1

numIndicators <- 2

# Create the fixed LDE loading matrix.

L1 <- rep(1,embedD)
L2 <- c(1:embedD)*theTau*deltaT-mean(c(1:embedD)*theTau*deltaT)
L3 <-  (L2^2)/2
LMatrix <- cbind(L1,L2,L3)



### Set up empty data frame to "catch" output from model ###
ldeout <- data.frame(matrix(ncol = 25, nrow = 14))
colnames(ldeout) <- c("dyad", "etaX.est", "etaX.se", "zetaX.est", 
                      "zetaX.se","etaY.est", "etaY.se", "zetaY.est",
                      "zetaY.se","gammaX.est", "gammaX.se", 
                      "gammaY.est", "gammaY.se", "VX.est", "VX.se", "VdX.est", 
                      "VdX.se", "Vd2X.est","Vd2X.se","VY.est","VY.se",
                      "VdY.est","VdY.se", "Vd2Y.est","Vd2Y.se")

### Run model for each dyad ####

#dyadids2 <- subset(dyadids, dyad > 164)

dyadids$rownum <-as.numeric(1:nrow(dyadids))

for (i in dyadids$dyad) {
  # emdbed the data
tEmbedded <- cbind(gllaEmbed(tData$rsa.basec[tData$dyad==i], embed=embedD, tau=theTau, label="x", idColumn=FALSE),
                   gllaEmbed(tData$b.rsa.basec[tData$dyad==i], embed=embedD, tau=theTau, label="y", idColumn=FALSE))

#Create a 2nd order Multivariate LDE model.

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

ldeModel1CoupledFit <- mxRun(ldeModelCoupled1)


ldeout[(dyadids$rownum[dyadids$dyad==i]), ] = c(dyad = i, etaX.est = ldeModel1CoupledFit$output$estimate["etaX"], 
                                                    zetaX.est =  ldeModel1CoupledFit$output$estimate["zetaX"], 
                                                    etaX.se = ldeModel1CoupledFit$output$standardErrors["etaX",1], 
                                                    zetaX.se = ldeModel1CoupledFit$output$standardErrors["zetaX",1], 
                                                    etaY.est= ldeModel1CoupledFit$output$estimate["etaY"], 
                                                    zetaY.est= ldeModel1CoupledFit$output$estimate["zetaY"],
                                                    etaY.se= ldeModel1CoupledFit$output$standardErrors["etaY",1], 
                                                    zetaY.se= ldeModel1CoupledFit$output$standardErrors["zetaY",1], 
                                                    gammaX.est=ldeModel1CoupledFit$output$estimate["gammaX"], 
                                                    gammaX.se= ldeModel1CoupledFit$output$standardErrors["gammaX", 1], 
                                                    gammaY.est=ldeModel1CoupledFit$output$estimate["gammaY"], 
                                                    gammaY.se=ldeModel1CoupledFit$output$standardErrors["gammaY", 1], 
                                                    VX.est=ldeModel1CoupledFit$output$estimate["VX"], 
                                                    VdX.est=ldeModel1CoupledFit$output$estimate["VdX"], 
                                                    Vd2X.est=ldeModel1CoupledFit$output$estimate["Vd2X"],
                                                    VX.se= ldeModel1CoupledFit$output$standardErrors["VX", 1],
                                                    VdX.se= ldeModel1CoupledFit$output$standardErrors["VdX", 1],
                                                    Vd2X.se=ldeModel1CoupledFit$output$standardErrors["Vd2X", 1],
                                                    VY.est=ldeModel1CoupledFit$output$estimate["VY"], 
                                                    VdY.est=ldeModel1CoupledFit$output$estimate["VdY"], 
                                                    Vd2Y.est=ldeModel1CoupledFit$output$estimate["Vd2Y"],
                                                    VY.se= ldeModel1CoupledFit$output$standardErrors["VY", 1],
                                                    VdY.se= ldeModel1CoupledFit$output$standardErrors["VdY", 1],
                                                    Vd2Y.se=ldeModel1CoupledFit$output$standardErrors["Vd2Y", 1])



}

# https://stat.ethz.ch/pipermail/r-help/2006-June/107734.html
View(ldeout)







ldeout[(dyadids2$rownum[dyadids2$dyad==178]), ] = c(dyad = 178, etaX.est = 1, 
                                                zetaX.est =  1, etaX.se = 1, 
                                                zetaX.se = 1, etaY.est=1, zetaY.est=1,etaY.se=1, 
                                                zetaY.se=1, gammaX.est=1, gammaX.se=1, 
                                                gammaY.est=1, gammaY.se=1, VX.est=1, VdX.est=1, 
                                                Vd2X.est=1,VX.se=1,VdX.se=1,Vd2X.se=1,VY.est=1,
                                                VdY.est=1,Vd2Y.est=1,VY.se=1,VdY.se=1,Vd2Y.se=1)




ldeout[dyadids2$rownum[dyadids2$dyad==i], ] = c(dyad = i, etaX.est = ldeModel1CoupledFit$output$estimate["etaX"], 
                                                zetaX.est =  ldeModel1CoupledFit$output$estimate["zetaX"])

















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

acf(tData1$rsa.basec)



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

