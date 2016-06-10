#-------------------------------------------------------------------------------------------------------------
#Psychophysiological Methods and Analysis: Module 2: Heart Rate Variability
#Version with Time 6 as separate phase
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#Set working directory: replace with your directory path
setwd("C:/Users/Niall/Dropbox/NIALL1/MET/ppmc");

#Read in the hrv dataset prepared by Katherine; Note the use of na.omit function
hrv <- na.omit(read.csv("ISP_HR_partialdata_long.csv"))

#Source Tal Galili's improved version of merge function
source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/merge.data.frame.r")

#-------------------------------------------------------------------------------------------------------------

library(car) #For Recode function
library(lme4) #For multilevel modeling
library(lmerTest) #To get Satterthwaite df and p values from lmer
library(pbkrtest) #To get K-R df and p values from lmer

#Create phase6, a version of phase with time6 as a separate phase
hrv <- within(hrv, {
  phase6 <- Recode(time, 
                    ' 1:5 = "baseline"; 6 = "time6"; 7:10 = "practice"; 11:13 = "recovery" ',
                   as.factor.result=TRUE)
}
)

#Make the factor phase6 have levels in the right order,
#i.e., with "baseline" as the reference category
hrv$phase6 <- with(hrv, factor(phase6, levels=c('baseline','time6','practice','recovery')))


#-------------------------------------------------------------------------------------------------------------
#Sort the hrv dataset by the size of the change in RSA between time 5 and 6

#Create variable that is the difference in RSA between time 5 and 6
# using subset function

# First, subset RSA for time=5 (1 dataline per subj)
time5 <- subset(hrv, time==5, select=c(subj, time, RSA))
names(time5) <- c("subj", "time5", "RSA5")

#Next, subset time=5
time6 <- subset(hrv, time == 6, select=c(subj, time, RSA))
names(time6) <- c("subj", "time6", "RSA6")

#Bind the datasets by column, so there is still just 1 dataline per subj
time56 <- merge.data.frame(time5, time6, by = "subj", keep_order=1)
time56$difRSA <- time56$RSA6 - time56$RSA5

#Merge and order by RSA difference between time 5 and 6 
hrv1 <- merge.data.frame(hrv, time56, by = "subj", keep_order=1)
ordhrv1 <- hrv1[order(hrv1$difRSA), ]
ordtime56 <- time56[order(time56$difRSA), ]


#-------------------------------------------------------------------------------------------------------------
#Raw Data: Panel plots, sorted by RSA change between time 5 at time 6
#-------------------------------------------------------------------------------------------------------------

#pdf(file="hrv-drop-ordered-panel-time-course.pdf", width=14, height=28)
svg(file="hrv-drop-ordered-panel-time-course.svg", width=14, height=28)

par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv1$time[hrv1$subj==i], hrv1$RSA[hrv1$subj==i],  ylab="HR Variability", xlab="Time",
       type="l", ylim=c(0,10), xlim=c(0, 13)) 
  abline(v=c(5, 6, 10))
}
dev.off()


#-------------------------------------------------------------------------------------------------------------
#MODELS
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
#Model with random intercept and random slope
#-------------------------------------------------------------------------------------------------------------
rsaout2p6<- lmer(RSA ~ Condition*phase6 + (1 + phase6 | subj), data = hrv1)
summary(rsaout2p6, ddf = "Kenward-Roger")
anova(rsaout2p6, type = 3, ddf = "Kenward-Roger")

#Pull random effects of time6 drop; add to fixed effect of drop (averaged over Inv, Vis)
#Look at its distribution

time6drop<- - 1.114 + ranef(rsaout2p6)$subj[, 2]
densityPlot( ~ time6drop, bw="SJ", adjust=1.8, kernel="gaussian")


#Panel Plots HRV actual and HRV predicted vs. Time for subjects sorted by time 6 drop

pdf(file="hrv-panel-HRV-pred-vs-time-for-time6-drop.pdf", width=14, height=28)
#svg(file="hrv-panel-HRV-pred-vs-time-for-time6-drop.svg", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv1$time[hrv1$subj==i], hrv1$RSA[hrv1$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13),
       main=paste("id", i, "(", round(ordtime56$difRSA[ordtime56$subj==i], digits=1),")"))
  points(hrv1$time[hrv1$subj==i], predict(rsaout2p6)[hrv1$subj==i], pch="-", cex=3, col="red")
  abline(v=c(5, 6, 10))
}
dev.off()

#-------------------------------------------------------------------------------------------------------------
#Misspecified Model: Fixed intercept and slope
#-------------------------------------------------------------------------------------------------------------
rsaout0p6<- lm(RSA ~ Condition*phase6, data = hrv1)
summary(rsaout0p6)
Anova(rsaout0p6, type="3")

#Misspec (rasout0) Panel Plots HRV actual and HRV predicted vs. Time for subjects sorted by time 6 drop

pdf(file="rsaout0-hrv-panel-HRV-pred-vs-time-for-time6-drop.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv1$time[hrv1$subj==i], hrv1$RSA[hrv1$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13),
       main=paste("id", i, "(", round(ordtime56$difRSA[ordtime56$subj==i], digits=1),")"))
  points(hrv1$time[hrv1$subj==i], predict(rsaout0p6)[hrv1$subj==i], pch="-", cex=3, col="red")
  abline(v=c(5, 6, 10))
}
dev.off()


#-------------------------------------------------------------------------------------------------------------
#Misspecified Model: Random intercept and fixed slope
#-------------------------------------------------------------------------------------------------------------
rsaout1p6<- lmer(RSA ~ Condition*phase6 + (1 | subj), data = hrv1)
summary(rsaout1p6, ddf = "Kenward-Roger")
anova(rsaout1p6, type="3", ddf = "Kenward-Roger")

#Misspec (rsaout1): Panel Plots HRV actual and HRV predicted vs. Time for subjects sorted by time 6 drop

pdf(file="rsaout1-hrv-panel-HRV-pred-vs-time-for-time6-drop.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv1$time[hrv1$subj==i], hrv1$RSA[hrv1$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13),
       main=paste("id", i, "(", round(ordtime56$difRSA[ordtime56$subj==i], digits=1),")"))
       points(hrv1$time[hrv1$subj==i], predict(rsaout1p6)[hrv1$subj==i], pch="-", cex=3, col="red")
       abline(v=c(5, 6, 10))
}
dev.off()

#-------------------------------------------------------------------------------------------------------------
#Prepare for Spaghetti Plot
#-------------------------------------------------------------------------------------------------------------

#Create phase means for plotting: First, Condition = "Invisible"
minv<-matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 
               1, 0, 1, 0, 0, 0, 0, 0, 
               1, 0, 0, 1, 0, 0, 0, 0,
               1, 0, 0, 0, 1, 0, 0, 0), nrow=4, byrow=TRUE)
library(lme4)
phasemeansinv<-minv%*%fixef(rsaout2p6)
#Set up  (13, 1) vector of phase means
vinv<-c(rep(phasemeansinv[1,1], 5), phasemeansinv[2, 1], rep(phasemeansinv[3,1], 4), rep(phasemeansinv[4,1], 3))
#Set up time
time<-seq(1:13)
#Bind in a matrix
predinv<-cbind(time, vinv)

#Create phase means for plotting: Second, Condition = "Visible"
mvis<-matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 
               1, 1, 1, 0, 0, 1, 0, 0, 
               1, 1, 0, 1, 0, 0, 1, 0,
               1, 1, 0, 0, 1, 0, 0, 1), nrow=4, byrow=TRUE)

phasemeansvis<-mvis%*%fixef(rsaout2p6)
#Set up  (13, 1) vector of phase means
vvis<-c(rep(phasemeansvis[1,1], 5), phasemeansvis[2, 1], rep(phasemeansvis[3,1], 4), rep(phasemeansvis[4,1], 3))
#Set up time
time<-seq(1:13)
#Bind in a matrix
predvis<-cbind(time, vvis)


#Phase6 spaghetti plot
pdf(file="phase6-lmer-hrv-spaghetti-plot.pdf", width=14, height=10)
par(mfcol=c(1,2))
plot(hrv1$time[hrv1$Condition=="Invisible"], hrv1$RSA[hrv1$Condition=="Invisible"], 
     ylab="HR Variability", xlab="Time", type="n", pch=4, xlim=c(0,13), ylim=c(0,10), main="Invisible Condition")
for (i in unique(hrv1$subj[hrv1$Condition=="Invisible"]))
{
  lines(hrv1$time[hrv1$Condition=="Invisible" & hrv1$subj==i],
        predict(rsaout2p6)[hrv1$Condition=="Invisible" & hrv1$subj==i], lwd=2)
}
#Add fixed effects: phasemeansinv
lines(predinv[, 1], predinv[, 2], lwd=5, col="red")
abline(v=c(5, 6, 10))

plot(hrv1$time[hrv1$Condition=="Visible"], hrv1$RSA[hrv1$Condition=="Visible"], ylab="HR Variability",
     xlab="Time", type="n", pch=4, xlim=c(0,13), ylim=c(0,10), main="Visible Condition")
for (i in unique(hrv1$subj[hrv1$Condition=="Visible"]))
{
  lines(hrv1$time[hrv1$Condition=="Visible" & hrv1$subj==i],
        predict(rsaout2p6)[hrv1$Condition=="Visible" & hrv1$subj==i], lwd=2)
}
#Add fixed effects: phasemeansvis
lines(predvis[, 1], predvis[, 2], lwd=5, col="red")
abline(v=c(5, 6, 10))
dev.off()
