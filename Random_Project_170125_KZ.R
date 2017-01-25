

# RANDOM PROJECT ANALYSES 161021

setwd("/Users/zeekatherine")
setwd("~/KZNB")


rnd <- read.csv("randomproject_data_aggregated_161021.csv")


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
rnd$trial40 <- (rnd$trials.thisN + 1) 
rnd$trial40c <- (rnd$trial40 - 20)/40


#### TRIMMING & TRANSFORMING RT ####

# rt density plot
ggplot(rnd, aes(x=response.rt)) + 
  geom_density()

hist(rnd$response.rt)

# trimmed data: removed any < 100 ms and any greater than 3 SD above mean (1.29+.8*3=3.69)
sd(rnd$response.rt, na.rm=T) # SD = 0.35
summary(rnd$response.rt, na.rm=T) # M = 1.02
#rndt <- subset(rnd, rt >= .01 & rt < 3.69)
rndt <- rnd


##log transform latency scores
rndt$rt <- rndt$response.rt
rndt$logrt <- log(rndt$response.rt)

# log rt density plot - normal !
ggplot(rndt, aes(x=logrt)) + 
  geom_density()

rndt$id <- rndt$ID

rndt$valenceE <-recode(rndt$valence, as.factor.result=F, '"negative" = -0.5; "positive" = 0.5')



#### ANALYSES without Self-Relevance ####

### 
#random intercept & slope for trial number (centered -.5, .5)
summary(lmer(rt ~ trial40c + (trial40c | id),  data=rndt))


### VALENCE EFFECT (objective valence of words)
# simple regression: no main effect of valence on rt
summary(lm(rt ~ valence, rndt)) # significant main effect


#random intercept model - ns fixed
summary(lmer(rt ~ valence + trial40c + (1 | id),  data=rndt)) # ME of valence

#random intercept & slope  - ns fixed
summary(lmer(rt ~ valence + trial40c + (valence + trial40c | id),  data=rndt)) # ME of valence



# plotted: 
ggplot(data=rndt, aes(x=valence, y=logrt)) +
  geom_jitter(aes(color=as.factor(valence)), position = position_jitter(width = .2), size=1.5) +
  labs(title = "Log RT by Valence", x="id", y="Log RT") +
  stat_summary(aes(group=id), fun.y = mean, geom="point", colour="black", size=2) +
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



##### Only looking at traits rated as self-descriptive ######


rndtb2 <- subset(rndt, response.keys == "up")

# standard model 
summary(lm(rt~valenceE + trial40c, data = rndtb2))

#random intercept models
intmod <- (lmer(rt ~ valenceE + trial40c + (1 | ID),  data=rndtb2)) # ME valence
summary(lmer(logrt ~ valenceE + trial40c + (1 | ID),  data=rndtb2)) # ME valence


#random intercept & slope
summary(lmer(rt ~ valenceE + trial40c + (valenceE | ID),  data=rndtb2)) # ME valence
summary(lmer(logrt ~ valenceE + trial40c + (valenceE | ID),  data=rndtb2)) # ME valence
anova(intmod, slopemod)


#random intercept & slope of valence and word
summary(lmer(rt ~ valenceE + trial40c + (valenceE | ID) + (1 | word),  data=rndtb2)) # ME valence
summary(lmer(logrt ~ valenceE + trial40c + (valenceE | ID) + (1 | word),  data=rndtb2)) # ME valence






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
#ggsave(selfrelplot, file = "selfrelplot_N62.pdf", height = 40/2.54, width = 64/2.54)



####### Updates - Stricter Exclusion Cutoff ######


# trimmed data: removed any < 100 ms and any greater than 2 SD above mean (1.29+.8*2=2.89)
sd(rndtb2$rt) # SD = 0.34
summary(rndtb2$rt) # M = .98

quantile(rndtb2$rt, c(.90, .95, .99))

rndtb <- subset(rndtb2, rt >= .01 & rt < 1.45) #90th percentile or below


mean(rndtb$rt)
median(rndtb$rt)
mean(rndtb$rt[rndtb$valence=="positive"])
mean(rndtb$rt[rndtb$valence=="negative"])

# standard model -- Significant main effect of valence
summary(lm(logrt~valenceE + trial40c, data = rndtb))
summary(lm(rt~valenceE + trial40c, data = rndtb)) 

#random intercept model
summary(lmer(logrt ~ valenceE + trial40c + (1 | id),  data=rndtb))
summary(lmer(rt ~ valenceE + trial40c + (1 | id),  data=rndtb))


#random intercept & slope of valence
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtb))
summary(lmer(rt ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtb))

#random intercept & slope of valence plus random intercept of word
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE| id) + (1 | word),  data=rndtb))
summary(lmer(rt ~ valenceE + trial40c + (1 + valenceE| id) + (1 | word),  data=rndtb))




#random intercept & slope of trial
noslope <- (lme(fixed=logrt ~ valenceE + trial40c,  data=rndtb, random= ~ 1 + trial40c | id))
summary(noslope)


####### Updates - Stricter Exclusion Cutoff 2 ######
rndtb2$rt.z <- scale(rndtb2$rt)
sort(rndtb2$rt.z)
rndtz <- subset(rndtb2,  rt.z < 3) #within 3 SD of the mean RT

# standard model -- Significant main effect of valence
summary(lm(logrt~valenceE + trial40c, data = rndtz))
summary(lm(rt~valenceE + trial40c, data = rndtz)) #run on raw rt

#random intercept model
summary(lmer(logrt ~ valenceE + trial40c + (1 | id),  data=rndtz))
summary(lmer(rt ~ valenceE + trial40c + (1 | id),  data=rndtz))


#random intercept & slope of valence
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtz))
summary(lmer(rt ~ valenceE + trial40c + (1 + valenceE| id),  data=rndtz))


#random intercept & slope of valence & random effects of time 
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE + trial40c | id) ,  data=rndtz))
summary(lmer(rt ~ valenceE + trial40c + (1 + valenceE + trial40c| id) ,  data=rndtz))


#random intercept & slope of valence & random effects of time and word
summary(lmer(logrt ~ valenceE + trial40c + (1 + valenceE + trial40c | id) + (1 | word),  data=rndtz))
summary(lmer(rt ~ valenceE + trial40c + (1 + valenceE + trial40c| id) + (1 | word),  data=rndtz))





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

#pdf("random_wave2_dotplot_N62_161021.pdf", width = 16, height = 8) 
stripchart(1000*vposranef.raw2, pch=21, bg="skyblue", cex=3.5, lwd=2,
           xlim=c(-500, 150), xlab="Trait Valence Effect (in Milliseconds)", cex.axis=1.5, cex.lab=1.5)
abline(v=c(-408, -161, 32), col = c("red", "black", "red"),
       lwd = 3)
#dev.off()

#Run analysis using brms
#valenceslope.brm <- brm(logrt ~ valence + trial40c + (1 + valence + trial40c | id), 
                         data=rndtb2, chains=2, cores=4)
summary(valenceslope.brm)
valslopesamp<-posterior.samples(valenceslope.brm)
par(mfrow=c(1,1))
plot(density(valslopesamp$sd_id_valenceT.positive))
abline(v=c(.04, .13))
pp<-predict(valenceslope.brm, nsamples=6, summary=F)
par(mfrow=c(4,1))
plot(density(rndtb2$logrt), xlim=c(-1, 1), ylim=c(0, 1.5))
plot(density(pp[1,]), xlim=c(-1, 1), ylim=c(0, 1.5))
plot(density(pp[2,]), xlim=c(-1, 1), ylim=c(0, 1.5))
plot(density(pp[3,]), xlim=c(-1, 1), ylim=c(0, 1.5))





summary(valenceslope)
vposranef<- -0.05165573 + ranef(valenceslope)[2]
mean(vposranef$valence)
plot(density(vposranef$valence))
quantile(vposranef$valence, probs=c(.025, .975))
qnorm(c(.025, .25, .75, .975), mean=-.05, sd=.09) #96% prediction interval for ranefs
Dotplot(vposranef$valence)

stripchart(vposranef$valence, pch=21, bg="skyblue", cex=3.5, lwd=2,
           xlim=c(-.20, .10), xlab="Trait Valence Effect", cex.axis=1.5, cex.lab=1.5)
abline(v=c(.04, .13))


# look at change in loglikelihood (multiply by -2?)
# obtain degrees of freedom: 
(-2*-101.7602) # this means that there is a need for random effects!
(-2*-107.9709) # with random slope of valence, this means that there is a need for random effects!

anova(valenceslope, noslope)
# for tests on random effects: 


# Examine distribution of random effects



####### With Data from Ran Hassin PNAS ######
setwd("/Users/zeekatherine/KZNB/Ran Hassin PNAS Data")

##### Study 6 Analyses
ran6a <- read.csv("exp6_all_data_long.csv")
ran6 <- subset(ran6a, exclude_from_analysis == "FALSE" & operand == "S")

ran6$presentation_time_f <- as.factor(ran6$presentation_time)

ran6$congr <- ifelse(ran6$congruent == "no", -.5, .5)
ran6$ptimer <- ifelse(ran6$presentation_time == "1700", -.5, .5)


## random intercept only
# MLE model
ran6mod1 <- lmer(rt ~ congr*ptimer + (1 | subject), data = ran6)
summary(ran6mod1)
confint(ran6mod1, oldNames = FALSE)



# Bayesian model
ran6mod1b <- brm(rt ~ congr*ptimer + (1 | subject), data = ran6, 
                 cores = 1, chains = 1)
summary(ran6mod1b)


## random slopes
# MLE model
ran6mod2 <- lmer(rt ~ congr*ptimer + (1 + congr | subject), data = ran6)
summary(ran6mod2)
confint(ran6mod2, oldNames = FALSE)

# Bayesian model
ran6mod2b <- brm(rt ~ congr*ptimer + (1 + congr | subject), data = ran6,
                 cores = 1, chains = 1)
summary(ran6mod2b)


# Study 6 Dotplot (MLE model with random slopes)
summary(ran6mod2)
ran6mod2ranef <- -15.358 + ranef(ran6mod2)$subject[2]
quantile(ran6mod2ranef$congr, probs=c(.025, .5, .975))

#pdf("random_ran6_dotplot.pdf", width = 16, height = 8) 
stripchart(ran6mod2ranef, pch=21, bg="skyblue", cex=2, lwd=2,
           xlim=c(-20, -9), xlab="Difference in Reaction Times between Congruent and Incongruent Conditions", cex.axis=1.5, cex.lab=1.5)
abline(v=c(-18.6, -15.14, -12.21), col = c("red", "black", "red"),
       lwd = 3)
#dev.off()


##### Study 7 Analyses
ran7a <- read.csv("exp7_all_data_long.csv")
ran7 <- subset(ran7a, subjective == "1" & operation == "S" & objective.test.score <= 0.6)
# COMPUTE filter_$=(subjective = 1 & OTRate <= 0.6 & operation = 'S').

ran7$congr <- ifelse(ran7$congruent == "no", -.5, .5)
ran7$ptimer <- ifelse(ran7$presentation.duration == "1000", -.5, .5)


## random intercept only
# MLE model
ran7mod1 <- lmer(rt ~ congr*ptimer + (1 | subject), data = ran7)
summary(ran7mod1)
confint(ran7mod1, oldNames = FALSE)


# Bayesian model
ran7mod1b <- brm(rt ~ congr*ptimer + (1 | subject), data = ran7, 
                 cores = 1, chains = 1)
summary(ran7mod1b)


## random slopes 
# MLE model
ran7mod2 <- lmer(rt ~ congr*ptimer + (1 + congr | subject), data = ran7)
summary(ran7mod2)
confint(ran7mod2, oldNames = FALSE)

# Bayesian model
ran7mod2b <- brm(rt ~ congr*ptimer + (1 + congr | subject), data = ran7,
                 cores =1, chains = 1)
summary(ran7mod2b)


# Study 7 Dotplot (MLE model with random slopes)
summary(ran7mod2)
ran7mod2ranef <- -13.496 + ranef(ran7mod2)$subject[2]
quantile(ran7mod2ranef$congr, probs=c(.025, .5, .975))

#pdf("random_ran7_dotplot.pdf", width = 16, height = 8) 
stripchart(ran7mod2ranef, pch=21, bg="skyblue", cex=2, lwd=2,
           xlim=c(-27, -1), xlab="Difference in Reaction Times between Congruent and Incongruent Conditions", cex.axis=1.5, cex.lab=1.5)
abline(v=c(-24.64, -14.14, -3.16), col = c("red", "black", "red"),
       lwd = 3)
#dev.off()
