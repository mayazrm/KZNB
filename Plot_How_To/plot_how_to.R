### With the face orientation dataset

library(lme4)
library(lmerTest)

#Read
ranface <- read.csv("experiment7_facesorient.csv")

#Transform
ranface$fo1 <- ifelse(ranface$Face_orientation == 1, 1, 0)
ranface$fo2 <- ifelse(ranface$Face_orientation == 2, 1, 0)
ranface$trial.r <- ranface$trial -1

#Run Model
ranfacem1 <- lmer(RT ~ fo1 + fo2 + (fo1 + fo2 | Participant), data = ranface)
summary(ranfacem1)

#Get Ranefs for f01
#Add ID Column
#Order by Ranef Size
ranfacem1.ranef<-ranef(ranfacem1)$Participant
ranfacem1.ranef$Participant <- as.numeric(row.names(ranfacem1.ranef))
ranfacem1.ranef$fo1 <- ranfacem1.ranef$fo1 -5.0919e-01
ordranfacem1.ranef <- ranfacem1.ranef[order(ranfacem1.ranef$fo1), ]

#Add Predicted Values to ranface
ranface$pred <- fitted(ranfacem1)

#Order ranface by Participant, Face-orientation, predicted value
ordranface <- ranface[order(ranface$Participant, 
                            ranface$Face_orientation, ranface$pred), ]

#Select Upright and Upside-Down trials
ordranfacefo1 <-ordranface[ordranface$Face_orientation!=2, ]


#Panel plots ordered by size of fo1 random effect
windows()
par(mfrow=c(5,5))
for (i in ordranfacem1.ranef$Participant) {
  plot(jitter(ordranfacefo1$fo1[ordranfacefo1$Participant==i], .2), 
       ordranfacefo1$RT[ordranfacefo1$Participant==i],
       ylab="RT (ms)", xlab="Upright",
       type="p", pch=19, col="blue", ylim=c(.2, 8), xlim=c(0, 1),
       main=paste(round(ordranfacem1.ranef$fo1[ordranfacem1.ranef$Participant==i], digits=2), sep = " "))
  lines(ordranfacefo1$fo1[ordranfacefo1$Participant==i], 
  ordranfacefo1$pred[ordranfacefo1$Participant==i], lwd=2, col="red")
}


#data for id=4
ordranfacefo1[ordranfacefo1$Participant==4, ]$fo1
ordranfacefo1$fo1[ordranfacefo1$Participant==4]

ordranfacefo1[ordranfacefo1$Participant==4, ]$pred
ordranfacefo1$pred[ordranfacefo1$Participant==4]

#Try with id=4
plot(ordranfacefo1$fo1[ordranfacefo1$Participant==4], 
     ordranfacefo1$RT[ordranfacefo1$Participant==4],
     ylab="Reaction Time (ms)", xlab="Face Orientation (Upright v. Upside-down)",
     type="p", pch=19, col="blue", ylim=c(.2,10), xlim=c(0, 1),
     main=paste("id =", 4, sep = " "))
lines(ordranfacefo1$fo1[ordranfacefo1$Participant==4], 
      ordranfacefo1$pred[ordranfacefo1$Participant==4], lwd=2, col="red")
