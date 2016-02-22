#-------------------------------------------------------------------------------------------------------------
kzaf<-read.csv("/Users/zeekatherine/Desktop/Work:School/Graduate School/Columbia PhD/2016_5Spring/Psychophysiological_Methods/ACQ/AF KZ SCR data LONG v2.csv")

#-------------------------------------------------------------------------------------------------------------

#Run phase analysis using lm
kzafout<-lm(MeanSC~subj + meditation + action + subj*meditation + subj*action, data=kzaf)
summary(kzafout)


#-------------------------------------------------------------------------------------------------------------
#Plot time course data for AF and KZ

kz <- subset(kzaf, subj=="KZ")
par(mfrow=c(1,1), lwd=2) 
plot(kz$segment, kz$MeanSC, type="l", col="blue", xlim=c(1,13), ylim=c(7, 16), xlab="Segment", ylab="Skin Conductance")
points(kz$segment, kz$MeanSC, col="red")

par(new=TRUE)
af<-subset(kzaf, subj=="AF")
plot(af$segment, af$MeanSC, type="l", col="blue", pch=19, xlim=c(1,13), ylim=c(7, 16), xlab="Segment",
     ylab="Skin Conductance")
points(af$segment, af$MeanSC, col="red")
#-------------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------------
#Plot phase results against data points
#windows()
par(mfrow=c(1,2), lwd=2) 
af<-subset(kzaf, subj=="AF")
plot(af$segment, af$MeanSC, type="p", col="red", pch=19, xlim=c(1,13), ylim=c(7, 16), xlab="Segment",
     ylab="Skin Conductance")
segments(0, 8.3, x1 = 6, y1 = 8.3)
segments(6, (8.3 + 2.6), x1 = 10, y1 = (8.3 + 2.6))
segments(10, (8.3 + 3.4), x1 = 14, y1 = (8.3 + 3.4))
abline(v=c(6, 10))


kz<-subset(kzaf, subj=="KZ")
plot(kz$segment, kz$MeanSC, type="p", col="red", pch=19, xlim=c(1,13), ylim=c(7, 16), xlab="Segment",
     ylab="Skin Conductance")
segments(0, (8.3 + 2.2298), x1 = 6, y1 = (8.3 + 2.2298))
segments(6, (8.3 + 2.2298 + 2.6 + 0.21), x1 = 10, y1 = (8.3 + 2.2298 + 2.6 + 0.21))
segments(10, (8.3 + 2.2298 + 3.4 + 0.37), x1 = 14, y1 = (8.3 + 2.2298 + 3.4 + 0.37))
abline(v=c(6, 10))
#-------------------------------------------------------------------------------------------------------------

#plot phase results against time course
par(mfrow=c(1,1), lwd=2) 
plot(kz$segment, kz$MeanSC, type="l", col="blue", xlim=c(1,13), ylim=c(7, 16), xlab="Segment",
     ylab="Skin Conductance")
points(kz$segment, kz$MeanSC, col="red")
segments(0, (8.3 + 2.2298), x1 = 6, y1 = (8.3 + 2.2298))
segments(6, (8.3 + 2.2298 + 2.6 + 0.21), x1 = 10, y1 = (8.3 + 2.2298 + 2.6 + 0.21))
segments(10, (8.3 + 2.2298 + 3.4 + 0.37), x1 = 14, y1 = (8.3 + 2.2298 + 3.4 + 0.37))
abline(v=c(6, 10))

par(new=TRUE)
af<-subset(kzaf, subj=="AF")
plot(af$segment, af$MeanSC, type="l", col="blue", pch=19, xlim=c(1,13), ylim=c(7, 16), xlab="Segment",
     ylab="Skin Conductance")
points(af$segment, af$MeanSC, col="red")
segments(0, 8.3, x1 = 6, y1 = 8.3)
segments(6, (8.3 + 2.6), x1 = 10, y1 = (8.3 + 2.6))
segments(10, (8.3 + 3.4), x1 = 15, y1 = (8.3 + 3.4))
abline(v=c(6, 10))


#-------------------------------------------------------------------------------------------------------------
#To add predicted values from fitted model to the above plot, add
points(kzaf$segment, predict(kzafout))






