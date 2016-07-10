

# RANDOM PROJECT ANALYSES

setwd("/Users/zeekatherine")
setwd("~/KZNB")


rnd <- read.csv("~/KZNB/random.proj_160526c.csv")


library(nlme)
library(lme4)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

#For dotplots
library(Rcmdr)

#For Bayesian analysis:
library(brms)


#### RECODING TIME ####

# so that one unit (-.5, .5) = duration of study
rnd$trial40 <- (rnd$trial_num + 1) 
rnd$trial40c <- (rnd$trial40 - 20)/40


#### TRIMMING & TRANSFORMING RT ####

# rt density plot
ggplot(rnd, aes(x=rt)) + 
  geom_density()

# trimmed data: removed any < 100 ms and any greater than 3 SD above mean (1.29+.8*3=3.69)
sd(rnd$rt) # SD = 0.80
summary(rnd$rt) # M = 1.29
rndt <- subset(rnd, rt >= .01 & rt < 3.69)

##log transform latency scores
rndt$logrt <- log(rndt$rt)

# log rt density plot - normal !
ggplot(rndt, aes(x=logrt)) + 
  geom_density()



#### ANALYSES ####

### PRACTICE EFFECT? opposite - FATIGUE!
#random intercept & slope for trial number (centered -.5, .5)
summary(lme(fixed=rt ~ trial40c ,  data=rndt, random= ~ 1 + trial40c | id))


### VALENCE EFFECT (objective valence of words)
# simple regression: no main effect of valence on rt
summary(lm(rt ~ valence, rndt))


#random intercept model - ns fixed
summary(lme(fixed=rt ~ valence + trial40c,  data=rndt, random= ~ 1 | id))

#random intercept & slope  - ns fixed
summary(lme(fixed=rt ~ valence + trial40c,  data=rndt, random= ~ 1 + valence | id))


# plotted: no fixed effect because of heterogeneity! 
# failure to replicate main effect is case in point :)
ggplot(data=rndt, aes(x=valence, y=logrt)) +
  geom_jitter(aes(color=as.factor(valence)), position = position_jitter(width = .2), size=1.5) +
  labs(title = "Log RT by Valence", x="id", y="Log RT") +
  stat_summary(aes(group=key), fun.y = mean, geom="point", colour="black", size=2) +
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




### SELF-RELEVANCE EFFECT (endorsed as self-descriptive) - 
# respond sig faster to self-descriptive words

# simple regression: sig main effect of self-relevance on rt
summary(lm(rt ~ key, rndt))

#random intercept model
summary(lme(fixed=rt ~ key +trial40c ,  data=rndt, random= ~ 1 | id))

#random intercept & slope
summary(lme(fixed=rt ~ key +trial40c,  data=rndt, random= ~ 1 + key | id))



#vlabeller <- function(var, value){
#  value <- as.character(value)
#  if (var=="key") { 
#    value[value=="down"] <- "Not"
#    value[value=="up"]   <- "Self"
#  }
#  return(value)
#}


ggplot(data=rndt, aes(x=key, y=logrt)) +
  geom_jitter(aes(color=as.factor(key)), position = position_jitter(width = .2), size=1.5) +
  labs(title = "Log RT by Self-Relevance", x="id", y="Log RT") +
  stat_summary(aes(group=key), fun.y = mean, geom="point", colour="black", size=2) +
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



# VALENCE controlling for self-relevance (ns)

#random intercept model
summary(lme(fixed=rt ~ valence + key + trial40c,  data=rndt, random= ~ 1 | id))

#random intercept & slope
summary(lme(fixed=rt ~ valence + key + trial40c,  data=rndt, random= ~ 1 + valence | id))



##### Only looking at traits rated as self-descriptive ######


rndtb2 <- subset(rndt, key == "up")

# standard model 
summary(lm(rt~valence + trial40c, data = rndtb2))

#random intercept model
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 | id))

#random intercept & slope
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence | id))




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
ggsave(selfrelplot, file = "selfrelplot.pdf", height = 40/2.54, width = 64/2.54)


###### Time Course #######

# random slope and intercept for trial number
summary(lme(fixed=rt ~ valence * trial40c,  data=rndtb2, random= ~ trial40c | id))

summary(lme(fixed=rt ~ valence * trial40c,  data=rndtb2, random= ~ valence * trial40c | id))



####### Updates 160607 - Stricter Exclusion Cutoff ######


# trimmed data: removed any < 100 ms and any greater than 2 SD above mean (1.29+.8*2=2.89)
sd(rnd$rt) # SD = 0.80
summary(rnd$rt) # M = 1.29

quantile(rnd$rt, c(.90, .95, .99))

rndtb <- subset(rnd, rt >= .01 & rt < 2)
##log transform latency scores
rndtb$logrt <- log(rndtb$rt[rndtbvalence=="positive"])



rndtb2 <- subset(rndtb, key == "up")
mean(rndtb2$rt)
median(rndtb2$rt)
mean(rndtb2$rt[rndtb2$valence=="positive"])
mean(rndtb2$rt[rndtb2$valence=="negative"])

# standard model -- Significant main effect of valence
summary(lm(logrt~valence + trial40c, data = rndtb2))
summary(lm(rt~valence + trial40c, data = rndtb2)) #run on raw rt

#random intercept model
summary(lme(fixed=logrt ~ valence + trial40c,  data=rndtb2, random= ~ 1 | id))

#random intercept & slope of valence
summary(lme(fixed=logrt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence | id))

#random intercept & slope of trial
noslope <- (lme(fixed=logrt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + trial40c | id))
summary(noslope)

#random intercept & slope of valence and trial
valenceslope <- (lme(fixed=logrt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence + trial40c | id))
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence + trial40c | id)) #raw rt
#random intercept & slope of valence and trial: Raw RT
valenceslope.raw <- (lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence + trial40c | id))
summary(valenceslope.raw)
vposranef.raw<- -0.0501060 + ranef(valenceslope.raw)[2]
quantile(vposranef.raw$valence, probs=c(.025, .975))
1000*quantile(vposranef.raw$valence, probs=c(.025, .5, .975))

stripchart(1000*vposranef.raw$valencepositive, pch=21, bg="skyblue", cex=3.5, lwd=2,
           xlim=c(-200, 100), xlab="Trait Valence Effect (in Milliseconds)", cex.axis=1.5, cex.lab=1.5)
abline(v=c(-150, 60))


#Run analysis using brms
valenceslope.brm <- brm(logrt ~ valence + trial40c + (1 + valence + trial40c | id), 
                         data=rndtb2, chains=2, cores=4)
summary(valenceslope.brm)
valslopesamp<-posterior.samples(valenceslope.brm)
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


