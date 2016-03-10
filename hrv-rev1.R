#-------------------------------------------------------------------------------------------------------------
#Psychophysiological Methods and Analysis: Module 2: Heart Rate Variability
#Version with Time 6 as separate phase
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
setwd("C:/Users/Niall/Dropbox/NIALL1/MET/ppmc");
hrv <- na.omit(read.csv("ISP_HR_partialdata_long.csv"))

hrv <- within(hrv, {
  phase6 <- Recode(time, 
                    ' 1:5 = "baseline"; 6 = "time6"; 7:10 = "practice"; 11:13 = "recovery" ',
                   as.factor.result=TRUE)
}
)

hrv$phase6a <- NULL

hrv$phase6 <- with(hrv, factor(phase6, levels=c('baseline','time6','practice','recovery')))


#Model with random intercept and random slope
rsaout2p6<- lmer(RSA ~ Condition*phase6 + (1 + phase6 | subj), data = hrv)
summary(rsaout2p6)
Anova(rsaout2p6, type="3")


#Model with random intercepts for each phase 
rsaout2p6<- lmer(RSA ~ phase6*Condition + (phase6 | subj), data = hrv)
summary(rsaout2p6)
Anova(rsaout2p6, type="3")

#Pull random effects of time6 drop; add to fixed effect of drop (averaged over Inv, Vis)
#Look at its distribution
time6drop<- - 1.114 + random.effects(rsaout2p6)$subj[, 2]
hist(time6drop)
densityPlot( ~ time6drop, bw="SJ", adjust=1.8, kernel="gaussian")

#-------------------------------------------------------------------------------------------------------------
#Phase6 Version: Panel Plots HRV vs. Time for subjects ordered by time 6 drop
#-------------------------------------------------------------------------------------------------------------
pdf(file="Phase6-hrv-panel-HRV-vs-time-for-time6-drop.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv$time[hrv$subj==i], hrv$RSA[hrv$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13))
#  points(hrv$time[hrv$subj==i], predict(rsaout2p6)[hrv$subj==i], pch="-", cex=3, col="red")
  abline(v=c(5, 6, 10))
}
dev.off()



#-------------------------------------------------------------------------------------------------------------
#Phase6 Version: Panel Plots HRV actual and HRV predicted vs. Time for subjects ordered by time 6 drop
#-------------------------------------------------------------------------------------------------------------
pdf(file="Phase6-hrv-panel-HRV-pred-vs-time-for-time6-drop.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv$time[hrv$subj==i], hrv$RSA[hrv$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13))
  points(hrv$time[hrv$subj==i], predict(rsaout2p6)[hrv$subj==i], pch="-", cex=3, col="red")
  abline(v=c(5, 6, 10))
}
dev.off()


#-------------------------------------------------------------------------------------------------------------
#Prepare for Spaghetti Plot
#-------------------------------------------------------------------------------------------------------------

#Create phase means for plotting: First inv
minv<-matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 
               1, 0, 1, 0, 0, 0, 0, 0, 
               1, 0, 0, 1, 0, 0, 0, 0,
               1, 0, 0, 0, 1, 0, 0, 0), nrow=4, byrow=TRUE)
phasemeansinv<-minv%*%fixed.effects(rsaout2p6)
#Set up  (13, 1) vector of phase means
vinv<-c(rep(phasemeansinv[1,1], 5), phasemeansinv[2, 1], rep(phasemeansinv[3,1], 4), rep(phasemeansinv[4,1], 3))
#Set up time
time<-seq(1:13)
#Bind in a matrix
predinv<-cbind(time, vinv)

#Create phase means for plotting: First inv
mvis<-matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 
               1, 1, 1, 0, 0, 1, 0, 0, 
               1, 1, 0, 1, 0, 0, 1, 0,
               1, 1, 0, 0, 1, 0, 0, 1), nrow=4, byrow=TRUE)

phasemeansvis<-mvis%*%fixed.effects(rsaout2p6)
#Set up  (13, 1) vector of phase means
vvis<-c(rep(phasemeansvis[1,1], 5), phasemeansvis[2, 1], rep(phasemeansvis[3,1], 4), rep(phasemeansvis[4,1], 3))
#Set up time
time<-seq(1:13)
#Bind in a matrix
predvis<-cbind(time, vvis)


#Phase6 spaghetti plot
pdf(file="phase6-lmer-hrv-spaghetti-plot.pdf", width=14, height=10)
par(mfcol=c(1,2))
plot(hrv$time[hrv$Condition=="Invisible"], hrv$RSA[hrv$Condition=="Invisible"], 
     ylab="HR Variability", xlab="Time", type="n", pch=4, xlim=c(0,13), ylim=c(0,10), main="Invisible Condition")
for (i in unique(hrv$subj[hrv$Condition=="Invisible"]))
{
  lines(hrv$time[hrv$Condition=="Invisible" & hrv$subj==i],
        predict(rsaout2p6)[hrv$Condition=="Invisible" & hrv$subj==i], lwd=2)
}
#Add fixed effects: phasemeansinv
lines(predinv[, 1], predinv[, 2], lwd=5, col="red")
abline(v=c(5, 6, 10))

plot(hrv$time[hrv$Condition=="Visible"], hrv$RSA[hrv$Condition=="Visible"], ylab="HR Variability",
     xlab="Time", type="n", pch=4, xlim=c(0,13), ylim=c(0,10), main="Visible Condition")
for (i in unique(hrv$subj[hrv$Condition=="Visible"]))
{
  lines(hrv$time[hrv$Condition=="Visible" & hrv$subj==i],
        predict(rsaout2p6)[hrv$Condition=="Visible" & hrv$subj==i], lwd=2)
}
#Add fixed effects: phasemeansvis
lines(predvis[, 1], predvis[, 2], lwd=5, col="red")
abline(v=c(5, 6, 10))
dev.off()


