#Heterogeneity1.R

#Github shared directory
setwd("~/KZNB")

#Read in full dataset
rnd <- read.csv("~/KZNB/random.proj_160526c.csv")

library(car)
library(nlme)
library(lme4)
library(brms)

#Look at full rt distribution
mean(rnd$rt) # M = 1.29
sd(rnd$rt) # SD = 0.80
quantile(rnd$rt, c(.025, .25, .5, .75, .975, 1))
plot(density(rnd$rt), xlim=c(0, 3))

#Rescale trial number so that one unit (-.5, .5) = complete range
rnd$trial40 <- (rnd$trial_num + 1) 
rnd$trial40c <- (rnd$trial40 - 20)/40

#Create effect-coded valence;
rnd$valenceE <-recode(rnd$valence, as.factor.result=F, '"negative" = -0.5; "positive" = 0.5')


#What is the effect of self-relevance and valence
#Regression
summary(lm(rt~key + valence, data = rnd))
summary(lm(rt~key*valence, data = rnd))
#Means only model
summary(lm(rt~key*valence - key - valence -1, data = rnd))

#Remove trials the rts < 100 ms or > 2 SD above mean
rndtb <- subset(rnd, rt >= .01 & rt < 2)

rndtb2 <- subset(rndtb, key == "up")
mean(rndtb2$rt)
median(rndtb2$rt)
mean(rndtb2$rt[rndtb2$valence=="positive"])
mean(rndtb2$rt[rndtb2$valence=="negative"])
quantile(rndtb2$rt, c(.025, .25, .5, .75, .975, 1))
plot(density(rndtb2$rt), xlim=c(0, 3))


pdf(file="rt-by-valence.pdf", width=14, height=28)

par(mfrow=c(10,7))
for (i in unique(rndtb2$id)) {
  plot(rndtb2$valenceE[rndtb2$id==i], rndtb2$rt[rndtb2$id==i],  
       ylab="Reaction Time (seconds)", xlab="Valence (-.5,+.5)",
       type="p", pch=21, cex=2, lwd=2, bg="skyblue", ylim=c(0,2), xlim=c(-0.7, 0.7),
       main=paste("id ", round(i, digits=3), sep = " "))
  #abline(v=c(5, 6, 10))
}

dev.off()

rndbrm0<- brm(rt ~ trial40c + valenceE , data = rndtb2, chains = 2, cores = 4)
summary(rndbrm0)
rndsamp0<-posterior.samples(rndbrm0, nsamples=5)
plot(marginal_effects(rndbrm0))

rndbrm2 <- brm(rt ~ valenceE + trial40c + (1 + valenceE + trial40c | id), 
                        data=rndtb2, chains=2, cores=4)
summary(rndbrm2)



##order by size of valence slope
rebrm2 <- ranef(rndbrm2)
mean(rebrm2$id[,2])
reval.brm2 <- (rebrm2$id[,2] + fixef(rndbrm2)[2,1]) #Create predicted re's 
REbrm2 <- cbind(unique(rndtb2$id), reval.brm2) 
ordrebrm2 <- REbrm2[order(REbrm2[, 2]), ] #Ordered by reval.brm2 (second column of REbrm2)

plot(marginal_effects(rndbrm2, effects = "valenceE",
                      conditions = data.frame(id = unique(rndtb2$id), trial40c = 0),
                      re_formula = NULL, points=TRUE)) 

mebrm2 <- marginal_effects(rndbrm2, effects = "valenceE",
                      conditions = data.frame(id = unique(rndtb2$id), trial40c = 0),
                      re_formula = NULL, points=TRUE)


#Plot predictions:

pdf(file="predrt-by-valence.pdf", width=30, height=22)

par(mfrow=c(5,14))
for (i in ordrebrm2[,1]) {
  plot(mebrm2$valenceE$valenceE[mebrm2$valenceE$id==i],
       mebrm2$valenceE$Estimate[mebrm2$valenceE$id==i], 
       ylab="Predicted RT (seconds)", xlab="Valence (-.5,+.5)",
       type="l", cex=2, lwd=2.5, bg="skyblue", ylim=c(0,2), xlim=c(-0.7, 0.7),
       main=paste("id ", round(i, digits=3), sep = " "))
  lines(mebrm2$valenceE$valenceE[mebrm2$valenceE$id==i],
        mebrm2$valenceE$lowerCI[mebrm2$valenceE$id==i], lty=1, lwd=1.5, col="black")
  lines(mebrm2$valenceE$valenceE[mebrm2$valenceE$id==i],
        mebrm2$valenceE$upperCI[mebrm2$valenceE$id==i], lty=1, lwd=1.5, col="black")
  points(rndtb2$valenceE[rndtb2$id==i], rndtb2$rt[rndtb2$id==i], pch=21, cex=1.2, bg="skyblue")
  #abline(v=c(5, 6, 10))
}

dev.off()


##Spaghetti Plot

#Fixed effect predictions
predrt.fixed <- fixef(rndbrm2)[1] +  
  fixef(rndbrm2)[2]*mebrm2$valenceE$valenceE[mebrm2$valenceE$id==101]


pdf(file="predrt-spaghetti-plot.pdf", width=4, height=8)
plot(rndtb2$valenceE, rndtb2$rt, ylab="Predicted RT (seconds)", xlab="Valence (-.5,+.5)",
     type="n", cex=2, lwd=2.5, bg="skyblue", ylim=c(.7,1.4), xlim=c(-0.5, 0.5))
for (i in ordrebrm2[,1]) 
  {
  lines(mebrm2$valenceE$valenceE[mebrm2$valenceE$id==i],
        mebrm2$valenceE$Estimate[mebrm2$valenceE$id==i], lty=1, lwd=1.5, col="black")
}

#Add in fixed effects line
 lines(mebrm2$valenceE$valenceE[mebrm2$valenceE$id==101], predrt.fixed, lwd=5, col="red")

dev.off()
