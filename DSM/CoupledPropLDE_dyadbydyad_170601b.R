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

pdf("Raw_RSA_Panel_Plots_withlines.pdf", height = 12, width = 12)
par(mfrow=c(4,4))
for (i in dyadids$dyad) {
  plot(c(1, 40), c(-3, 3),
       xlab="Time",
       ylab="RSA", 
       main=paste(i), col = "white")
  lines(tData$time[tData$dyad==i], tData$rsa.basec[tData$dyad==i], type='p', lwd=2, col='deeppink')
  lines(tData$time[tData$dyad==i], tData$b.rsa.basec[tData$dyad==i], type='p', lwd=2, col='blue')
  lines(tData$time[tData$dyad==i], tData$base[tData$dyad==i], type='l', lty=1, col=1)
  lines(tData$time[tData$dyad==i][order(tData$time[tData$dyad==i])], 
        tData$rsa.basec[tData$dyad==i][order(tData$time[tData$dyad==i])], 
        xlim=range(tData$time[tData$dyad==i]), ylim=range(tData$rsa.basec[tData$dyad==i]), 
        pch=16, col = "deeppink", lwd = 2)
  lines(tData$time[tData$dyad==i][order(tData$time[tData$dyad==i])], 
        tData$b.rsa.basec[tData$dyad==i][order(tData$time[tData$dyad==i])], 
        xlim=range(tData$time[tData$dyad==i]), ylim=range(tData$b.rsa.basec[tData$dyad==i]), 
        pch=16, col = "blue", lwd = 2)
  }
dev.off()





#####
#ACF for rsa of female partners
#####
pdf("ACF_Panel_Plots.pdf", height = 12, width = 12)
par(mfrow=c(4,4))
for (i in dyadids$dyad) {
  acf(tData$rsa.basec)[tData$dyad==i]
}
dev.off()



#####
#ACF for rsa of male partners
#####
pdf("ACF_Panel_Plots_Male.pdf", height = 12, width = 12)
par(mfrow=c(4,4))
for (i in dyadids$dyad) {
  acf(tData$b.rsa.basec)[tData$dyad==i]
}
dev.off()


##### Setting Embedding #####


embedD <- 8
# ^ with value of 5, major issues estimating zeta. 
# with value of 7, gammas go to outer bounds

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
#View(ldeout)
print(ldeout)
summarize(ldeout)
#write.csv(ldeout, "CoupledPropLDE_dyadbydyad_modeloutput_170602.csv")






