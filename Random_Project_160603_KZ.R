

# RANDOM PROJECT ANALYSES

setwd("/Users/zeekatherine")



rnd <- read.csv("~/Desktop/R Files/Random_Project/random.proj_160526c.csv")


library(nlme)
library(lme4)
library(ggplot2)
theme_set(theme_bw(base_size = 14))

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
rndtb <- subset(rnd, rt >= .01 & rt < 2.89)

##log transform latency scores
rndtb$logrt <- log(rndtb$rt)



rndtb2 <- subset(rndtb, key == "up")

# standard model -- Significant main effect of valence
summary(lm(rt~valence + trial40c, data = rndtb2))

#random intercept model
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 | id))

#random intercept & slope of valence
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence | id))

#random intercept & slope of valence and timee
summary(lme(fixed=rt ~ valence + trial40c,  data=rndtb2, random= ~ 1 + valence + trial40c | id))




