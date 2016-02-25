#-------------------------------------------------------------------------------------------------------------
#Psychophysiological Methods and Analysis: Module 2: Heart Rate Variability
#-------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
setwd("C:/Users/Niall/Dropbox/NIALL1/MET/ppmc");
hrv <- read.csv("ISP_HR_partialdata_long.csv")
#-------------------------------------------------------------------------------------------------------------
library(lme4)
library(brms)
library(car)

#Linear model with fixed intercept and slope
rsaout0<- lm(RSA ~ Condition*phase, data = hrv)
summary(rsaout0)
Anova(rsaout0, type="3")

#Model with random intercept and fixed slope
rsaout1<- lmer(RSA ~ Condition*phase + (1 | subj), data = hrv)
summary(rsaout1)
Anova(rsaout1, type="3")

#Model with random intercept and random slope
rsaout2<- lmer(RSA ~ Condition*phase + (1 + phase | subj), data = hrv)
summary(rsaout2)
Anova(rsaout2, type="3")


#Linear model with fixed intercept and slope using Bayesian estimation
rsabrm0<- brm(RSA ~ Condition*phase, data = hrv)
summary(rsabrm0)
plot(rsabrm0)



#-------------------------------------------------------------------------------------------------------------
#Order the hrv dataset by the size of the change in RSA between time 5 and 6

#Create variable that is the difference in RSA between time 5 and 6
# using subset function

# First, subset RSA for time=5 (1 dataline per subj)
time5 <- subset(hrv, time==5, select=c(subj, time, RSA))
names(time5) <- c("subj", "time", "RSA5")

#Next, subset time=5
time6 <- subset(hrv, time == 6, select=c(subj, time, RSA))
names(time6) <- c("vsubj", "vtime", "RSA6")

#Bind the datasets by column, so there is still just 1 dataline per subj
time56 <- cbind(time5, time6)
time56$difRSA <- time56$RSA6 - time56$RSA5

#Merge and order by RSA difference between time 5 and 6 
hrv2 <- merge(hrv1, time56, by = "subj")
ordhrv2 <- hrv2[order(hrv2$difRSA), ]
ordtime56 <- time56[order(time56$difRSA), ]


#-------------------------------------------------------------------------------------------------------------
#Panel plots, ordered by RSA change between time 5 at time 6
#-------------------------------------------------------------------------------------------------------------

pdf(file="hrv-drop-ordered-panel-time-course.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv1$time[hrv1$subj==i], hrv1$RSA[hrv1$subj==i],  ylab="HR Variability", xlab="Time",
       type="l", ylim=c(0,10), xlim=c(0, 13)) 
}
dev.off()


#-------------------------------------------------------------------------------------------------------------
#Panel Plots HRV actual and HRV predicted vs. Time for subjects ordered by time 6 drop
#-------------------------------------------------------------------------------------------------------------
pdf(file="hrv-panel-HRV-pred-vs-time-for-time6-drop.pdf", width=14, height=28)
par(mfrow=c(11,7))
for (i in ordtime56$subj) {
  plot(hrv$time[hrv$subj==i], hrv$RSA[hrv$subj==i],  ylab="HR Variability", xlab="Time",
       type="o", pch=19, col="blue", ylim=c(.8,10), xlim=c(0, 13))
  points(hrv$time[hrv$subj==i], predict(rsaout2)[hrv$subj==i], pch="-", cex=3, col="red")
  abline(v=c(5, 10))
  
}
dev.off()

