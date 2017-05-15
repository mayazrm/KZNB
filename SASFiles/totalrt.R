############################################################################################
#95% Confidence Ellipse for Time1 and Time2 Random Effects (base on what results?)
############################################################################################

library(ellipse)
muII=c(-0.08284, -0.1128)
sigmaII=matrix(c(0.005934, 0.01359, 0.01359, 0.01642), nrow=2, byrow=TRUE)
sigmaII=matrix(c(0.005934, 0.00986, 0.00986, 0.01642), nrow=2, byrow=TRUE)

plot(ellipse(sigmaII, centre = muII, level=0.95, npoints=1000), type="l", lwd=4)

abline(a=0, b=1, lwd=4, col="grey")
text(7.2, 7.8, expression(45^{"o"}), cex=1.5)


############################################################################################
#Read the SAS EB estimates for the slope: they are wider than the R ones (which SAS run?)
############################################################################################

rand1 <- read.csv('C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk/rand1.csv')
colnames(rand1) <- tolower(colnames(rand1))
stripchart(rand1$slope, pch=21, bg="skyblue", cex=3.5, lwd=2, xlim=c(-.25, .06))
abline(v=c(-.10), lwd=5, col="skyblue")
mtext("-0.10", 1, line=2.7, adj=.48, cex=2)
mtext("Average", 3, line=.5, adj=.48, cex=2)


############################################################################################
#Read in Trait_Vale Random Effects for Time 1 and Time 2 from sesp2015-1.sas
############################################################################################

setwd("C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk/SESP2015")
library(foreign, pos=15)
total <- 
  read.spss("C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk/SESP2015/total.sav",
            use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(total) <- tolower(colnames(total))
save("total", file="C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk/SESP2015/total.RData")

summary(total$posminusneg1)
summary(total$posminusneg2)
cor(total$posminusneg1, total$posminusneg2, use="pairwise.complete.obs")
cov(total$posminusneg1, total$posminusneg2, use="pairwise.complete.obs")


windows()
par(mfrow=c(2,1)) 
stripchart(total$posminusneg1, pch=21, bg="skyblue", cex=3.5, lwd=2,
           xlim=c(-.60, .22), xlab="Trait Valence Effect: Time 1", cex.axis=1.5, cex.lab=1.5)
#abline(v=c(-.146), lwd=5, col="skyblue")
#mtext("-0.25", 1, line=2.7, adj=.48, cex=2)
#mtext("Average", 3, line=.5, adj=.48, cex=2)

stripchart(total$posminusneg2, pch=21, bg="skyblue", cex=3.5, lwd=2, xlim=c(-.60, .22),
           xlab="Trait Valence Effect: Time 2", cex.axis=1.5, cex.lab=1.5)
#abline(v=c(-.254), lwd=5, col="skyblue")
#mtext("-0.25", 1, line=2.7, adj=.48, cex=2)
#mtext("Average", 3, line=.5, adj=.48, cex=2)


muII=c(-0.14545, -0.25251)
sigmaII=matrix(c(0.20967^2, 0.02882221, 0.02882221, 0.20832^2), nrow=2, byrow=TRUE)

windows()
par(mfrow=c(1,1)) 
plot(total$posminusneg1, total$posminusneg2, pch=21, bg="skyblue",
     cex=3.5, lwd=2, xlim=c(-.60, .22), ylim=c(-.60, .22),
     xlab="Trait Valence Effect: Time 1", ylab="Trait Valence Effect: Time 2",
     cex.lab=1.5, cex.axis=1.5)
par(new=T)
plot(ellipse(sigmaII, centre = muII, level=0.95, npoints=1000), type="l", lwd=4,
     xlim=c(-.60, .22), ylim=c(-.60, .22), xlab="", ylab="",
     cex.lab=1.5, cex.axis=1.5)





