

# RANDOM PROJECT ANALYSES 161021

#setwd("/Users/zeekatherine")
setwd("~/KZNB")


rnd <- read.csv("randomproject_data_aggregated_161021.csv")
rndt <- rnd


library(nlme)
library(lme4)
library(ggplot2)
library(lmerTest)
theme_set(theme_bw(base_size = 14))
#For Bayesian analysis:
library(brms)

#For dotplots
library(Rcmdr)



#### RECODING TIME ####

# so that one unit (-.5, .5) = duration of study
rndt$trial40 <- (rndt$trials.thisN + 1) 
rndt$trial40c <- (rndt$trial40 - 20)/40

rndt$rt <- rndt$response.rt
rndt$logrt <- log(rndt$response.rt)

rndt$id <- rndt$ID

rndt$valenceE <-recode(rndt$valence, as.factor.result=F, '"negative" = -0.5; "positive" = 0.5')

rndt$rtms <- rndt$rt*1000


##### Only looking at traits rated as self-descriptive ######


rndtb2 <- subset(rndt, response.keys == "up")


selfrelplot <- ggplot(data=rndtb2, aes(x=valence, y=logrt)) +
  geom_jitter(aes(color=as.factor(valence)), position = position_jitter(width = .2), size=1.5) +
  labs(title = "Log RT by Valence - Self-Relevant Traits Only", x="id", y="Log RT") +
  stat_summary(aes(group=valence), fun.y = mean, geom="point", colour="black", size=2) +
  stat_summary(fun.data=mean_cl_boot, geom='errorbar', size = .5) +
  scale_color_manual(values=c("orangered3", "green4")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position='none',
        plot.title = element_text(lineheight=.8, face="bold"),
        axis.title.y = element_text(face="bold", vjust=0.5),
        axis.title.x = element_text(face="bold", vjust=-0.35)) +
  facet_wrap(~id) 
selfrelplot
#ggsave(selfrelplot, file = "selfrelplot_N62.pdf", height = 40/2.54, width = 64/2.54)


####### Analyses for Paper ########
## Updates - Stricter Exclusion Cutoff 2 ######
rndtb2 <- subset(rndt, response.keys == "up")
rndtb <- subset(rndtb2, rt >= .01 & rt < 1.45) #90th percentile or below
rndtb2$rt.z <- scale(rndtb2$rt)
sort(rndtb2$rt.z)
rndtz <- subset(rndtb2,  rt.z < 3) #within 3 SD of the mean RT

# standard model -- Significant main effect of valence
summary(lm(logrt~valenceE + trial40c, data = rndtz))
summary(lm(rtms~valenceE + trial40c, data = rndtz)) #run on raw rt

#random intercept model
summary(lmer(logrt ~ valenceE + trial40c + (1 | id),  data=rndtz))
summary(lmer(rtms ~ valenceE + trial40c + (1 | id),  data=rndtz))


#random intercept & slope of valence
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtz))
summary(lmer(rtms ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtz))


#random intercept & slope of valence & random effects of time 
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE + trial40c | id) ,  data=rndtz))
summary(lmer(rtms ~ valenceE + trial40c + (1 + valenceE + trial40c| id) ,  data=rndtz))


##
#random intercept & slope of valence & random effects of time and word
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE + trial40c | id) + (1 | word),  data=rndtz))
summary(lmer(rtms ~ valenceE + trial40c + (1 + valenceE + trial40c| id) + (1 | word),  data=rndtz))





#random intercept & slope of valence and trial
#valenceslope <- (lme(fixed=logrt ~ valenceE + trial40c,  data=rndtb, random= ~ 1 + valenceE + trial40c |id))
#summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence + trial40c | id)) #raw rt
#random intercept & slope of valence and trial: Raw RT
#valenceslope.raw <- (lme(fixed=rt ~ valenceE + trial40c,  data=rndtb, random= ~ 1 + valenceE + trial40c | id))
valenceslope.raw2 <- (lmer(rt ~ valenceE + trial40c + (1 + valenceE + trial40c| id) + (1 | word),  data=rndtz))
#summary(valenceslope.raw)
summary(valenceslope.raw2)
#vposranef.raw<- -0.1270142 + ranef(valenceslope.raw)[2]
vposranef.raw2<- -0.174426 + ranef(valenceslope.raw2)$id[2]


quantile(vposranef.raw2$valenceE, probs=c(.025, .975))
1000*quantile(vposranef.raw2$valenceE, probs=c(.025, .5, .975))

#pdf("random_wave2_dotplot_N62_172016.pdf", width = 16, height = 8) 
stripchart(1000*vposranef.raw2, pch=21, bg="skyblue", cex=3.5, lwd=2,
           xlim=c(-500, 150), xlab="Trait Valence Effect (in Milliseconds)", cex.axis=1.5, cex.lab=1.5)
abline(v=c(-408, -161, 32), col = c("red", "black", "red"),
       lwd = 3)
#dev.off()

