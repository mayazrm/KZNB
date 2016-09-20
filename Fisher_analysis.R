# ANALYSES FOR:
# Fisher, C., Hahn, A. C., DeBruine, L. M. & Jones, B. C. (2015). Women's preference for attractive makeup tracks changes in their salivary testosterone. Psychological Science, 26, 1958–1964. doi:10.1177/0956797615609900

#####################################
# NOTE: The original paper reporting these data has been retracted (see http://pss.sagepub.com/content/early/2016/02/03/0956797616630941.full). Further analyses including random slopes for the term att.c do not show the key effect of the interaction between testosterone and makeup attractiveness.
#####################################

# RANDOM EFFECTS VARIABLES
# code      = 	participant ID code (85 participants, all white, female and heterosexual)
# session   =	participant's test session (1st through 5th)
# face      = 	stimulus face ID (50 faces with random makeup combinations)

# DEPENDENT VARIABLE
# pref      = 	makeup preference scores

# FIXED EFFECTS VARIABLES
# att           = 	attractiveness score for each face stimulus (average ratings by 3rd party raters)
# estradiol     =	estradiol in pg/mL
# progesterone  = 	progesterone in pg/mL
# testosterone  = 	testosterone in pg/mL

# FIXED EFFECTS VARIABLES USED IN ANALYSES (created below in code)
# att.c     = 	attractiveness score for each face stimulus (centered)
# est.c	    = 	estradiol (centered) in pg/mL
# prog.c    = 	progesterone (centered) in pg/mL
# test.c    = 	testosterone (centered) in pg/mL
# etop.c    =	estradiol to progesterone ratio (centered)
# est.ln.c  = 	natural log of estradiol (centered) in pg/mL
# prog.ln.c	= 	natural log of progesterone (centered) in pg/mL
# test.ln.c	= 	natural log of testosterone (centered) in pg/mL
# etop.ln.c	=	natural log of estradiol to progesterone ratio (centered)


#SET CURRENT DIRECTORY TO WHEREVER THE FILE IS STORED, OR DRAG FILE INTO SPACE BETWEEN QUOTES IN LINE BELOW:
makeup<-read.csv("Fisher_data.csv")

#create etop ratio, center all hormones on grand mean, AND center attractiveness ratings of faces
makeup$etop<-makeup$estradiol/makeup$progesterone
makeup$test.c <- scale(makeup$testosterone, center=T, scale=F)
makeup$est.c <- scale(makeup$estradiol, center=T, scale=F)
makeup$prog.c <- scale(makeup$progesterone, center=T, scale=F)
makeup$etop.c <- scale(makeup$etop, center=T, scale=F)
makeup$att.c <- scale(makeup$att, center=T, scale=F)

require(lme4)
require(lmerTest)

# Analysis 1: Initial analysis
m1<-lmer(pref ~ est.c * att.c + prog.c * att.c + test.c * att.c + etop.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m1)

#Analysis 2: Initial analysis excluding e-to-p ratio
m2<-lmer(pref ~ est.c * att.c + prog.c * att.c + test.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m2)

#Analysis 3: Initial analysis excluding estradiol and progesterone
m3<-lmer(pref ~ test.c * att.c + etop.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m3)

#log transform all hormone values (using natural log) and re-center
makeup$est.ln.c<-scale(log(makeup$estradiol), center=T, scale=F)
makeup$prog.ln.c<-scale(log(makeup$progesterone), center=T, scale=F)
makeup$test.ln.c<-scale(log(makeup$testosterone), center=T, scale=F)
makeup$etop.ln.c<-scale(log(makeup$etop), center=T, scale=F)

#Analysis 4: Alternative version of analysis 2 with all hormones log normalized
m4<-lmer(pref ~ est.ln.c * att.c + prog.ln.c * att.c + test.ln.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m4)

#Analysis 5: Alternative version of analysis 3 with all hormones log normalized
m5<-lmer(pref ~ test.ln.c * att.c + etop.ln.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m5)

#Analysis 6: Additional analysis testing for interaction between estradiol and progesterone
m6<-lmer(pref ~ est.c * prog.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m6)

#Analysis 7: Additional analysis testing for interaction between estradiol and testosterone
m7<-lmer(pref ~ est.c * test.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m7)

#Analysis 8: Additional analysis testing for interaction between progesterone and testosterone
m8<-lmer(pref ~ prog.c * test.c * att.c + (1 | code/session) + (1 | face) + (1 | code:face), data=makeup, REML=FALSE)
summary(m8)

#Calculate confidence intervals for all models
confint(m1)
confint(m2)
confint(m3)
confint(m4)
confint(m5)
confint(m6)
confint(m7)
confint(m8)