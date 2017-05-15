
setwd("C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk")

#First, read in an SPSS dataset with the reaction time data


library(foreign)

RT <- read.spss("C:/Users/Niall/Dropbox/NIALL1/MET/Suppes Talk/RT-expt1.sav", 
                use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(RT) <- tolower(colnames(RT))


#Select Session 2 (when subjects were primed), results for non-practice items, and RTs less than 5 sec
#Note: This corresponds to SAS sample
RTexpttwo <- subset(RT, subset=time==2 & filter_.=="Selected" & rt <5000 & subj != 9 & subj != 15)
summary(RTexpttwo$rt)
RTexpttwo<-RTsmall[order(RTexpttwo$subj),]
subj<-unique(RTexpttwo$subj)

#Not sure this is the version I need; there is not RTsmall1
#load("C:/Users/Niall/Dropbox/NIALL1/MET/Analysis of Change Course/Spring 2013/Labs/RT.RData")

library(nlme)
#Run mixed effects model with AR(1) errors: Raw Reaction Times
RTmodel <- lme(fixed=rt ~ trait_vale*regfocus, control=list(maxIter=1000), data=RTexpttwo, random=~trait_vale | subj, correlation = corAR1())
summary(RTmodel)

#Run mixed effects model with AR(1) errors: Logged Raw Reaction Times
logRTmodel <- lme(fixed=log(rt) ~ trait_vale*regfocus, control=list(maxIter=1000), data=RTexpttwo, random=~trait_vale | subj, correlation = corAR1())
summary(logRTmodel)

#Key comparison run to SAS;
#Run mixed effects model with no AR(1) errors, no regfocus: Logged Raw Reaction Times
logRTmodel <- lme(fixed=log(rt) ~ trait_vale, control=list(maxIter=1000),
                  data=RTexpttwo, random=~trait_vale | subj)
summary(logRTmodel)

##############################################
#May 15, 2017 Analyses for heterogeneity paper
##############################################

library(lme4)
logRTmod <- lmer(rt_log ~ trait_vale + (trait_vale | subj), 
                  data=RTexpttwo)
summary(logRTmod)


exp(7.0726+2.08*(0.03053)) # 1106.655 -1043.257
exp(6.8866-2.08*(0.03053)) # 1043.257-1008.377= 136.557

1256.521-918.8268

exp(6.9796-2*( 0.2214272)) # 1008.377
exp(6.9796+2*( 0.2214272)) # 1008.377

exp(6.9796-2*( 0.2214272)) # 1008.377
exp(6.9796+2*( 0.2214272)) # 1008.377

exp(7.0726-0.22) 946.3382
exp(6.8866+0.22) 1219.993 - 946.3382

exp(7.0726+ 0.22) 1469.386 - 785.7196
exp(6.8866-0.22)  785.7196

exp(6.9796-0.3127) #785.9554
exp(6.9796+0.3127) # 1468.945 - 785.9554 = 682.9896
exp(6.9796-0.3127) #

0.3127

6.9796-0.09302

6.8866

7.0726
#Add predicted values from the raw reaction time analysis to the dataset
RTsmall$pred<-fitted(RTmodel)
#For graphing: Sort the dataset by regulatory focus (prevention, promotion), ID, and predicted RT
RTord<-RTsmall[order(RTsmall$regfocus, RTsmall$subj, RTsmall$pred),]

#Calculate the random effects
tv_eff<-ranef(logRTmodel)
tv_eff$rint<-tv_eff[,1] + 6.984452
tv_eff$rslope<-tv_eff[,2] + -0.103283
tv_eff$subj<-row.names(tv_eff)
tv_eff1<-tv_eff[order(tv_eff$rslope),]
tv_eff4 <- subset(tv_eff1, subset=subj != c(9, 15))


RTsmall2 <- merge(RTsmall1, tv_eff, all=TRUE, by="subj")
RTsmall3<-RTsmall2[order(RTsmall2$rslope),]
unique(RTsmall3$rslope)
names(RTsmall3)[c(18,27)] <- c("trait_vale","eb_slope")
RTsmall3$pred<-(RTsmall3$rint + RTsmall3$rslope*RTsmall3$trait_vale)
RTsmall3<-RTsmall3[order(RTsmall3$rslope, RTsmall3$trait_vale),]

#Whole sample
windows(12,12)
par(mfrow=c(5,5))
for (i in tv_eff1$rslope){
  plot(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$rt[RTsmall3$rslope==i], 
       ylab="Log(Reaction Time)", xlab="Trait Valence", type="p", pch=21, bg="skyblue", ylim=c(400,4000), xlim=c(-1.1, 1.1), 
       main=paste("slope =", round(i, digits=3), sep = " "))
  lines(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$pred[RTsmall3$rslope==i], type="l", lty=1)
}
mtext("Whole Sample", side=3, outer=TRUE, line=-1.2)

#Whole sample
windows(12,12)
par(mfrow=c(5,5))
for (i in tv_eff1$rslope){
  par(new=F)
  plot(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$rt_log[RTsmall3$rslope==i], 
       ylab="Log(Reaction Time)", xlab="Trait Valence", type="p", pch=21, bg="skyblue", ylim=c(6,8.2), xlim=c(-1.1, 1.1), 
       main=paste("slope =", round(i, digits=3), sep = " "))
  par(new=T)
  plot(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$pred[RTsmall3$rslope==i], axes=F, type="l", lty=1,  ylab=" ", xlab=" ",)
}
mtext("Whole Sample", side=3, outer=TRUE, line=-1.2)

#Whole sample
windows(12,12)
par(mfrow=c(5,5))
for (i in tv_eff1$rslope){
  plot(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$rt_log[RTsmall3$rslope==i], 
       ylab="Log(Reaction Time)", xlab="Trait Valence", type="p", pch=21, bg="skyblue", ylim=c(6.2,8.3), xlim=c(-1.1, 1.1), 
       main=paste("slope =", round(i, digits=3), sep = " "))
  lines(RTsmall3$trait_vale[RTsmall3$rslope==i], RTsmall3$pred[RTsmall3$rslope==i], type="l", lty=1, col="red", lwd=2)
  
  abline(lm(RTsmall3$rt_log[RTsmall3$rslope==i] ~ RTsmall3$trait_vale[RTsmall3$rslope==i]), col="blue", lwd=2)
}
mtext("Whole Sample", side=3, outer=TRUE, line=-1.2)

RTsmall4 <- subset(RTsmall3, subset=subj != c(9, 15))

#Whole sample
windows(12,12)
par(mfrow=c(5,5))
for (i in tv_eff4$rslope){
  plot(RTsmall4$trait_vale[RTsmall4$rslope==i], RTsmall4$rt_log[RTsmall4$rslope==i], 
       ylab="Log(Reaction Time)", xlab="Trait Valence", type="p", pch=21, bg="skyblue", ylim=c(6.2,8.3), xlim=c(-1.1, 1.1), 
       main=paste("slope =", round(i, digits=3), sep = " "))
  lines(RTsmall4$trait_vale[RTsmall4$rslope==i], RTsmall4$pred[RTsmall4$rslope==i], type="l", lty=1, col="red", lwd=2)
  
  abline(lm(RTsmall4$rt_log[RTsmall4$rslope==i] ~ RTsmall4$trait_vale[RTsmall4$rslope==i]), col="blue", lwd=2)
}
mtext("Whole Sample", side=3, outer=TRUE, line=-1.2)


