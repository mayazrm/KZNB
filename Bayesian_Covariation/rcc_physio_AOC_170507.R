##### Dyadic Physio Data updated 170507 #####


#rccfs <- data.frame(rcc$dyad, rcc$partner, rcc$firstsharer)
#rccfs$dyad <- rccfs$rcc.dyad
#rccfs$partner <- rccfs$rcc.partner
#rccfs$firstsharer <- rccfs$rcc.firstsharer

#rccphys <- read.csv("RCC_HRV_aggregated_161125.csv")

#rccphys2 <- merge(rccfs, rccphys, by = c("dyad", "partner"))
#write.csv(rccphys2, file = "rcc_physio_AOC_temp.csv")

###### Reshaping #######
setwd("/Users/zeekatherine/Desktop/R Files/RCC_Data")


aoc <- read.csv("RCC_HRV_aggregated_170417_forreshape.csv")
# reshaping data:

library(reshape)

#datalong.1 <- reshape(aoc, varying=c("hr_1", "hr_2", "hr_3", "hr_4", "hr_5", "hr_6", "hr_7", 
"hr_8", "hr_9", "hr_10", "hr_11", "hr_12", "hr_13", "hr_14", 
"hr_15", "hr_16", "hr_17", "hr_18", "hr_19", "hr_20", "hr_21", 
"hr_22", "hr_23", "hr_24", "hr_25", "hr_26", "hr_27", "hr_28", 
"hr_29", "hr_30", "hr_31", "hr_32", "hr_33", "hr_34", "hr_35", 
"hr_36", "hr_37", "hr_38", "hr_39", "hr_40", "rsa_1", "rsa_2", 
"rsa_3", "rsa_4", "rsa_5", "rsa_6", "rsa_7", "rsa_8", "rsa_9", 
"rsa_10", "rsa_11", "rsa_12", "rsa_13", "rsa_14", "rsa_15", 
"rsa_16", "rsa_17", "rsa_18", "rsa_19", "rsa_20", "rsa_21", 
"rsa_22", "rsa_23", "rsa_24", "rsa_25", "rsa_26", "rsa_27", 
"rsa_28", "rsa_29", "rsa_30", "rsa_31", "rsa_32", "rsa_33", 
"rsa_34", "rsa_35", "rsa_36", "rsa_37", "rsa_38", "rsa_39", "rsa_40", 
"ibi_1", "ibi_2", "ibi_3", "ibi_4", "ibi_5", "ibi_6", "ibi_7", "ibi_8", 
"ibi_9", "ibi_10", "ibi_11", "ibi_12", "ibi_13", "ibi_14", "ibi_15", 
"ibi_16", "ibi_17", "ibi_18", "ibi_19", "ibi_20", "ibi_21", "ibi_22", 
"ibi_23", "ibi_24", "ibi_25", "ibi_26", "ibi_27", "ibi_28", "ibi_29", 
"ibi_30", "ibi_31", "ibi_32", "ibi_33", "ibi_34", "ibi_35", "ibi_36", 
"ibi_37", "ibi_38", "ibi_39", "ibi_40", "rrate_1", "rrate_2", "rrate_3", 
"rrate_4", "rrate_5", "rrate_6", "rrate_7", "rrate_8", "rrate_9", 
"rrate_10", "rrate_11", "rrate_12", "rrate_13", "rrate_14", "rrate_15", 
"rrate_16", "rrate_17", "rrate_18", "rrate_19", "rrate_20", "rrate_21", 
"rrate_22", "rrate_23", "rrate_24", "rrate_25", "rrate_26", "rrate_27", 
"rrate_28", "rrate_29", "rrate_30", "rrate_31", "rrate_32", "rrate_33", 
"rrate_34", "rrate_35", "rrate_36", "rrate_37", "rrate_38", "rrate_39", 
"rrate_40", "ramp_1", "ramp_2", "ramp_3", "ramp_4", "ramp_5", "ramp_6", 
"ramp_7", "ramp_8", "ramp_9", "ramp_10", "ramp_11", "ramp_12", "ramp_13", 
"ramp_14", "ramp_15", "ramp_16", "ramp_17", "ramp_18", "ramp_19", "ramp_20", 
"ramp_21", "ramp_22", "ramp_23", "ramp_24", "ramp_25", "ramp_26", "ramp_27", 
"ramp_28", "ramp_29", "ramp_30", "ramp_31", "ramp_32", "ramp_33", "ramp_34", 
"ramp_35", "ramp_36", "ramp_37", "ramp_38", "ramp_39", "ramp_40", "b.hr_1", 
"b.hr_2", "b.hr_3", "b.hr_4", "b.hr_5", "b.hr_6", "b.hr_7", "b.hr_8", "b.hr_9", 
"b.hr_10", "b.hr_11", "b.hr_12", "b.hr_13", "b.hr_14", "b.hr_15", "b.hr_16", 
"b.hr_17", "b.hr_18", "b.hr_19", "b.hr_20", "b.hr_21", "b.hr_22", "b.hr_23", 
"b.hr_24", "b.hr_25", "b.hr_26", "b.hr_27", "b.hr_28", "b.hr_29", "b.hr_30", 
"b.hr_31", "b.hr_32", "b.hr_33", "b.hr_34", "b.hr_35", "b.hr_36", "b.hr_37", 
"b.hr_38", "b.hr_39", "b.hr_40", "b.rsa_1", "b.rsa_2", "b.rsa_3", "b.rsa_4", 
"b.rsa_5", "b.rsa_6", "b.rsa_7", "b.rsa_8", "b.rsa_9", "b.rsa_10", "b.rsa_11", 
"b.rsa_12", "b.rsa_13", "b.rsa_14", "b.rsa_15", "b.rsa_16", "b.rsa_17", 
"b.rsa_18", "b.rsa_19", "b.rsa_20", "b.rsa_21", "b.rsa_22", "b.rsa_23", "b.rsa_24", 
"b.rsa_25", "b.rsa_26", "b.rsa_27", "b.rsa_28", "b.rsa_29", "b.rsa_30", "b.rsa_31", 
"b.rsa_32", "b.rsa_33", "b.rsa_34", "b.rsa_35", "b.rsa_36", "b.rsa_37", "b.rsa_38", 
"b.rsa_39", "b.rsa_40", "b.ibi_1", "b.ibi_2", "b.ibi_3", "b.ibi_4", "b.ibi_5", 
"b.ibi_6", "b.ibi_7", "b.ibi_8", "b.ibi_9", "b.ibi_10", "b.ibi_11", "b.ibi_12", 
"b.ibi_13", "b.ibi_14", "b.ibi_15", "b.ibi_16", "b.ibi_17", "b.ibi_18", "b.ibi_19", 
"b.ibi_20", "b.ibi_21", "b.ibi_22", "b.ibi_23", "b.ibi_24", "b.ibi_25", "b.ibi_26", 
"b.ibi_27", "b.ibi_28", "b.ibi_29", "b.ibi_30", "b.ibi_31", "b.ibi_32", "b.ibi_33", 
"b.ibi_34", "b.ibi_35", "b.ibi_36", "b.ibi_37", "b.ibi_38", "b.ibi_39", "b.ibi_40", 
"b.rrate_1", "b.rrate_2", "b.rrate_3", "b.rrate_4", "b.rrate_5", "b.rrate_6", 
"b.rrate_7", "b.rrate_8", "b.rrate_9", "b.rrate_10", "b.rrate_11", "b.rrate_12", 
"b.rrate_13", "b.rrate_14", "b.rrate_15", "b.rrate_16", "b.rrate_17", "b.rrate_18", 
"b.rrate_19", "b.rrate_20", "b.rrate_21", "b.rrate_22", "b.rrate_23", "b.rrate_24", 
"b.rrate_25", "b.rrate_26", "b.rrate_27", "b.rrate_28", "b.rrate_29", "b.rrate_30", 
"b.rrate_31", "b.rrate_32", "b.rrate_33", "b.rrate_34", "b.rrate_35", "b.rrate_36", 
"b.rrate_37", "b.rrate_38", "b.rrate_39", "b.rrate_40", "b.ramp_1", "b.ramp_2", 
"b.ramp_3", "b.ramp_4", "b.ramp_5", "b.ramp_6", "b.ramp_7", "b.ramp_8", "b.ramp_9", 
"b.ramp_10", "b.ramp_11", "b.ramp_12", "b.ramp_13", "b.ramp_14", "b.ramp_15", 
"b.ramp_16", "b.ramp_17", "b.ramp_18", "b.ramp_19", "b.ramp_20", "b.ramp_21", 
"b.ramp_22", "b.ramp_23", "b.ramp_24", "b.ramp_25", "b.ramp_26", "b.ramp_27", 
"b.ramp_28", "b.ramp_29", "b.ramp_30", "b.ramp_31", "b.ramp_32", "b.ramp_33", 
"b.ramp_34", "b.ramp_35", "b.ramp_36", "b.ramp_37", "b.ramp_38", "b.ramp_39", "b.ramp_40"), 
direction = "long", idvar = c("dyad", "partner"), sep = "_")

#datalong <- datalong.1[order(datalong.1$dyad),]


write.csv(datalong, file = "rcc_physio_AOC_LONG_170417.csv")







##### With Long Data #####
#setwd("/Users/zeekatherine/Desktop/R Files/RCC_Data")

#aoc1 <- read.csv("rcc_physio_AOC_LONG_170417.csv")
#table(aoc1$dyad)
#sharer <- read.csv("RCC_firstsharerinfo.csv")
#sharer2 <- subset(sharer, firstsharer == "A" | firstsharer =="B")

#aoc <- merge(aoc1, sharer2, by = c("dyad", "partner"))
#aoc <- subset(aoc1b, firstsharer == "A" | firstsharer =="B")
#aoc1b <- as.data.frame(cbind(aoc1$dyad, aoc1$partner, aoc1$time, aoc1$rsa))
#colnames(aoc1b) <- c("dyad", "partner", "time", "rsa")
#table(aoc$dyad)

#aoc1c <- aoc1[order(-aoc1$rsa),] 


#aoc <- subset(aoc1, rsa < 10 & b.rsa < 10)

#library(car)

#aoc$phase <- (car::recode(aoc$time, "1:10 = 'Baseline'; 11:20 = 'GoalDisc'; 21:30 = 'Sup_A'; 31:40 = 'Sup_B'"))
#aoc$id <- (paste(aoc$dyad, aoc$partner, sep="."))



#aoc$suprole <- aoc$phase
#aoc$suprole[aoc$phase=="Sup_A" & aoc$firstsharer=="A" & aoc$partner=="A"] <- "receive"
#aoc$suprole[aoc$phase=="Sup_A" & aoc$firstsharer=="B" & aoc$partner=="B"] <- "receive"
#aoc$suprole[aoc$phase=="Sup_A" & aoc$firstsharer=="A" & aoc$partner=="B"] <- "provide"
#aoc$suprole[aoc$phase=="Sup_A" & aoc$firstsharer=="B" & aoc$partner=="A"] <- "provide"

#aoc$suprole[aoc$phase=="Sup_B" & aoc$firstsharer=="A" & aoc$partner=="A"] <- "provide"
#aoc$suprole[aoc$phase=="Sup_B" & aoc$firstsharer=="B" & aoc$partner=="B"] <- "provide"
#aoc$suprole[aoc$phase=="Sup_B" & aoc$firstsharer=="A" & aoc$partner=="B"] <- "receive"
#aoc$suprole[aoc$phase=="Sup_B" & aoc$firstsharer=="B" & aoc$partner=="A"] <- "receive"

#write.csv(aoc, file = "rcc_physio_withSharerInfo_170507.csv")




##### Reading in Data and Packages ########

mean.na <- function (x) {
  out <- mean(x, na.rm=T)
  out
}




library(dplyr)
library(car)
library(brms)
library(ggplot2)
library(ggthemes)
library(lme4)
library(lmerTest)
library(brms)


### Setting phase variables (Baseline, GoalDisc, provide, receive)
#aoc$suprole <- as.factor(aoc$suprole)

#aoc <- within(aoc, suprole2 <- relevel(suprole, ref = "provide")) # reference group = provide
#aoc$suprole2 <- as.factor(aoc$suprole2)


###### Creating Distinguising Variables for Two Intercepts #####
#aoc$pa <- ifelse(aoc$partner=="A", 1, 0) # A = female partner
#aoc$pb <- ifelse(aoc$partner=="B", 1, 0) # B = male partner


#aoc$prov <- ifelse(aoc$suprole=="provide", 0, 1)


###### Centering Variabls #####


# Note: b. prefix indicates partner variables

# respiratory sinus arrhythmia (RSA), centered on baseline RSA
#aoc<- within(aoc, {base.mean = ave(rsa, dyad, partner, phase[aoc$phase=="Baseline"], FUN = mean.na)})
#aoc<- within(aoc, {b.base.mean = ave(b.rsa, dyad, partner, phase[aoc$phase=="Baseline"], FUN = mean.na)})

#aoc$rsa.basec <- aoc$rsa - aoc$base.mean # within-person baseline centered
#aoc$b.rsa.basec <- aoc$b.rsa - aoc$b.base.mean

# lagged variables are lagged by 1 timepoint
# centered on baseline
#aoc <- within(aoc, {rsa.basec.lag = ave(rsa.basec, dyad, partner, suprole2, FUN = dplyr::lag)})
#aoc <- within(aoc, {b.rsa.basec.lag = ave(b.rsa.basec, dyad, partner, suprole2, FUN = dplyr::lag)})

# centered on mean across all study periods
#aoc<- within(aoc, {rsa.mean = ave(rsa, dyad, partner, FUN = mean.na)})
#aoc$rsa.wc <- aoc$rsa - aoc$rsa.mean
#aoc <- within(aoc, {rsa.lag = ave(rsa.wc, dyad, partner,suprole2, FUN = dplyr::lag)})

#aoc<- within(aoc, {b.rsa.mean = ave(b.rsa, dyad, partner, FUN = mean.na)})
#aoc$b.rsa.wc <- aoc$b.rsa - aoc$b.rsa.mean
#aoc <- within(aoc, {b.rsa.lag = ave(b.rsa.wc, dyad, partner, suprole2, FUN = dplyr::lag)})





# Inter-beat Interval 
#aoc<- within(aoc, {ibi.mean = ave(ibi, dyad, partner, FUN = mean.na)})
#aoc$ibi.wc <- aoc$ibi - aoc$ibi.mean
#aoc <- within(aoc, {ibi.lag = ave(ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})

#aoc<- within(aoc, {b.ibi.mean = ave(b.ibi, dyad, partner, FUN = mean.na)})
#aoc$b.ibi.wc <- aoc$b.ibi - aoc$b.ibi.mean
#aoc <- within(aoc, {b.ibi.lag = ave(b.ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})
#aoc<- within(aoc, {ibi.mean = ave(ibi, dyad, partner, FUN = mean.na)})
#aoc$ibi.wc <- aoc$ibi - aoc$ibi.mean
#aoc <- within(aoc, {ibi.lag = ave(ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


#aoc<- within(aoc, {b.ibi.mean = ave(b.ibi, dyad, partner, FUN = mean.na)})
#aoc$b.ibi.wc <- aoc$b.ibi - aoc$b.ibi.mean
#aoc <- within(aoc, {b.ibi.lag = ave(b.ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


#aoc<- within(aoc, {b.hr.mean = ave(b.hr, dyad, partner, FUN = mean.na)})
#aoc$b.hr.wc <- aoc$b.hr - aoc$b.hr.mean
#aoc <- within(aoc, {b.hr.lag = ave(b.hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})

#aoc<- within(aoc, {hr.mean = ave(hr, dyad, partner, FUN = mean.na)})
#aoc$hr.wc <- aoc$hr - aoc$hr.mean
#aoc <- within(aoc, {hr.lag = ave(hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


#aoc<- within(aoc, {ibi.phasemean = ave(ibi, dyad, partner, phase, FUN = mean.na)})


#aoc <- within(aoc, {b.hr.lag = ave(b.hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


###### Creating Within-Phase Time variable ######
#aoc$timephase <- (car::recode(aoc$time, "1 = 1; 2 = 2; 3=3; 4=4;5=5; 6=6; 7=7; 8=8; 9=9;
                             # 10=10; 11=1; 12=2; 13=3; 14=4; 15=5; 16=6; 17=7; 18=8; 19=9;
                              #20=10; 21=1; 22=2; 23=3; 24=4; 25=5; 26=6; 27=7; 28=8; 29=9;
                              #30=10; 31=1; 32=2; 33=3; 34=4; 35=5; 36=6; 37=7; 38=8; 39=9;
                              #40=10"))
#aoc$timephase.r <- aoc$timephase - 1 # for looking at effects at start of discussion
#aoc$timephase.end <- aoc$timephase - 10 # for looking at effects at the end of the  discussion



###### Coregulation Analyses ########
setwd("/Users/zeekatherine/Desktop/R Files/RCC_Data")
aoc <- read.csv("rcc_physio_AOC_LONG_170417_withsharerinfo.csv")


#### looking within the "provide" phase 
aoc2 <- subset(aoc, suprole2 =="provide")

# b.rsa.basec = partner's rsa, within-person baseline centered
# rsa.basec.lag = one's own rsa lagged by 1 time pt

modelprovwc.rsabrm <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                         + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                         rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                         (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                       data = aoc2, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelprovwc.rsabrm)
ranef(modelprovwc.rsabrm)



#### goal only 
aocgoalonly <- subset(aoc, suprole2 == "GoalDisc")
modelprovwc.rsabrm_goal <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                           + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                           rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                           (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                         data = aocgoalonly, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelprovwc.rsabrm_goal)




#### receive only 
aocreconly <- subset(aoc, suprole2 == "receive")
modelprovwc.rsabrm_rec <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                                + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                                rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                                (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                              data = aocreconly, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 1000)
summary(modelprovwc.rsabrm_rec)




### Provide vs. Receive 
aocsup <- subset(aoc, suprole2 =="provide" | suprole2 == "receive")
modtestsup <- brm(b.rsa.wc ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + pa:suprole2 + pb:suprole2 + 
                      timephase.r:pa + timephase.r:pb + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + 
                      rsa.basec.lag:pa:suprole2 + rsa.basec.lag:pb:suprole2 +
                      rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb +
                     suprole2:rsa.basec.lag:timephase.r:pa + suprole2:rsa.basec.lag:timephase.r:pb +
                      (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                    data = aocsup,
                  autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modtestsup)





###### RSA  With Random Dyad Pairings ######
#randpair <- read.csv("/Users/zeekatherine/Desktop/R Files/RCC_Data/RCC_randompairing.csv")

#aocrand <- merge(aoc, randpair, by = c("dyad", "partner"))
#write.csv(aocrand, file = "rcc_randompairings_full.csv")

aocrandp <- read.csv("RCC_randompairings_full.csv")



aocrandp<- within(aocrandp, {base.mean = ave(rsa, dyad, partner, phase[aocrandp$phase=="Baseline"], FUN = mean.na)})
aocrandp<- within(aocrandp, {b.base.mean = ave(b.rsa, dyad, partner, phase[aocrandp$phase=="Baseline"], FUN = mean.na)})



aocrandp$rsa.basec <- aocrandp$rsa - aocrandp$base.mean
aocrandp$b.rsa.basec <- aocrandp$b.rsa - aocrandp$b.base.mean


aocrandp<- within(aocrandp, {rsa.mean = ave(rsa, dyad, partner, FUN = mean.na)})
aocrandp$rsa.wc <- aocrandp$rsa - aocrandp$rsa.mean
aocrandp <- within(aocrandp, {rsa.basec.lag = ave(rsa.wc, dyad, partner,suprole2, FUN = dplyr::lag)})

aocrandp<- within(aocrandp, {b.rsa.mean = ave(b.rsa, dyad, partner, FUN = mean.na)})
aocrandp$b.rsa.wc <- aocrandp$b.rsa - aocrandp$b.rsa.mean
aocrandp <- within(aocrandp, {b.rsa.basec.lag = ave(b.rsa.wc, dyad, partner, suprole2, FUN = dplyr::lag)})



aocrandp<- within(aocrandp, {ibi.mean = ave(ibi, dyad, partner, FUN = mean.na)})
aocrandp$ibi.wc <- aocrandp$ibi - aocrandp$ibi.mean
aocrandp <- within(aocrandp, {ibi.lag = ave(ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


aocrandp<- within(aocrandp, {b.ibi.mean = ave(b.ibi, dyad, partner, FUN = mean.na)})
aocrandp$b.ibi.wc <- aocrandp$b.ibi - aocrandp$b.ibi.mean
aocrandp <- within(aocrandp, {b.ibi.lag = ave(b.ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})
aocrandp<- within(aocrandp, {ibi.mean = ave(ibi, dyad, partner, FUN = mean.na)})
aocrandp$ibi.wc <- aocrandp$ibi - aocrandp$ibi.mean
aocrandp <- within(aocrandp, {ibi.lag = ave(ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


aocrandp<- within(aocrandp, {b.ibi.mean = ave(b.ibi, dyad, partner, FUN = mean.na)})
aocrandp$b.ibi.wc <- aocrandp$b.ibi - aocrandp$b.ibi.mean
aocrandp <- within(aocrandp, {b.ibi.lag = ave(b.ibi.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


aocrandp<- within(aocrandp, {b.hr.mean = ave(b.hr, dyad, partner, FUN = mean.na)})
aocrandp$b.hr.wc <- aocrandp$b.hr - aocrandp$b.hr.mean
aocrandp <- within(aocrandp, {b.hr.lag = ave(b.hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})

aocrandp<- within(aocrandp, {hr.mean = ave(hr, dyad, partner, FUN = mean.na)})
aocrandp$hr.wc <- aocrandp$hr - aocrandp$hr.mean
aocrandp <- within(aocrandp, {hr.lag = ave(hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


aocrandp<- within(aocrandp, {ibi.phasemean = ave(ibi, dyad, partner, phase, FUN = mean.na)})

aocrandp <- within(aocrandp, {b.hr.lag = ave(b.hr.wc, dyad, partner, suprole2, FUN = dplyr::lag)})


aocrandp <- within(aocrandp, {rsa.basec.lag = ave(rsa.basec, dyad, partner, suprole2, FUN = dplyr::lag)})
aocrandp <- within(aocrandp, {b.rsa.basec.lag = ave(b.rsa.basec, dyad, partner, suprole2, FUN = dplyr::lag)})



### Provide Phase
aocrandprov <- subset(aocrandp, suprole2=="provide")

modelprovwc.rsabrm_rand <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                           + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                           rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                           (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | randomdyad), 
                         data = aocrandprov, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelprovwc.rsabrm_rand)
# no coregulation with randomly paired dyads



#### Receive Phase 
aocrandrec <- subset(aocrandp, suprole2=="receive")
modelrecwc.rsabrm_rand <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                                + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                                rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                                (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | randomdyad), 
                              data = aocrandrec, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelrecwc.rsabrm_rand)
# no coregulation with randomly paired dyads


####### Coregulation controlling for concurrent linkage (synchrony) #######
modelprovsyncreg <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                           + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                           rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                         rsa.basec:pa + rsa.basec:pb +
                         (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                         data = aoc2, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelprovsyncreg)


modelrecsyncreg <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                         + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.r:pa + timephase.r:pb +
                         rsa.basec.lag:timephase.r:pa + rsa.basec.lag:timephase.r:pb + 
                         rsa.basec:pa + rsa.basec:pb +
                         (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.r:pa + timephase.r:pb | dyad), 
                       data = aocreconly, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelrecsyncreg)


###### Concurrent Covariation Analyses #######


modelprovsync <-brm(b.rsa.basec ~ 0 + pa + pb + pa:rsa.basec + pb:rsa.basec +
                      timephase.r:pa + timephase.r:pb +
                      timephase.r:pa:rsa.basec + timephase.r:pb:rsa.basec +
                           (0 + pa + pb + pa:rsa.basec + pb:rsa.basec + timephase.r:pa + timephase.r:pb | dyad), 
                         data = aoc2, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelprovsync)



modelrecsync <-brm(b.rsa.basec ~ 0 + pa + pb + pa:rsa.basec + pb:rsa.basec +
                      timephase.r:pa + timephase.r:pb +
                      timephase.r:pa:rsa.basec + timephase.r:pb:rsa.basec +
                      (0 + pa + pb + pa:rsa.basec + pb:rsa.basec + timephase.r:pa + timephase.r:pb | dyad), 
                    data = aocreconly, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelrecsync)




modelgoalsync <-brm(b.rsa.basec ~ 0 + pa + pb + pa:rsa.basec + pb:rsa.basec +
                     timephase.r:pa + timephase.r:pb +
                     timephase.r:pa:rsa.basec + timephase.r:pb:rsa.basec +
                     (0 + pa + pb + pa:rsa.basec + pb:rsa.basec + timephase.r:pa + timephase.r:pb | dyad), 
                   data = aoc3, autocor = cor_ar(~timephase.r|id), cores = 4, chains = 1, iter = 100)
summary(modelgoalsync)


##### Example Analysis with individual dyad ##########
aoc156 <- subset(aoc, dyad=="156" & suprole2 =="provide")
model156prov <-lm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb, data = aoc156)
summary(model156prov)

library(car)
avPlots(model156prov)

###### Sync Plot ########
aoc$idshare <- (paste(aoc$firstsharer, sep=".", aoc$dyad))
aocf <- subset(aoc, partner =="B")
aoc$partner.lab <- ifelse(aoc$partner=="A", "Female", "Male")

aocsub1 <- subset(aoc, dyad == 161 |  dyad == 156 | dyad == 107 | dyad == 176)


aocsub2 <- subset(aocsub1, time > 30)
aocsub2$time2 <- aocsub2$time-30
aocsub2$partner.lab2 <- ifelse(aocsub2$partner.lab=="Female", "Provider", "Recipient")
syncplot2 <- ggplot(aocsub2, aes(time2, rsa.basec, color = partner.lab2)) + geom_line(size = 1, aes(linetype = partner.lab2)) + 
  scale_linetype_manual(values=c("Provider" = "longdash", "Recipient" = "solid"))+
  coord_cartesian(xlim = c(1, 10), ylim = c(-2.2, 2)) +
  theme_few() + 
  scale_color_manual(values=c("Provider" = "darkgreen", "Recipient" = "gray50"))+
  labs(x = "Time", y = "Heart Rate Variability") +
  labs(colour = "Partner", linetype = "Partner") +
  geom_hline(yintercept=0, color = 'gray', size = .5)+
  facet_wrap(~dyad) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
syncplot2
#ggsave(syncplot2, file = "syncplot2_170501.pdf", height = 6, width = 8)

## during female receive/male provide
aocsubfrec <- subset(aocsub1, time > 20 & time < 41)
aocsubfrec$time2 <- aocsubfrec$time-20
aocsubfrec$partner.lab2 <- ifelse(aocsubfrec$partner.lab=="Female", "Provider", "Recipient")
syncplotrec <- ggplot(aocsubfrec, aes(time2, rsa.basec, color = partner.lab2)) + geom_line(size = 1, aes(linetype = partner.lab2)) + 
  scale_linetype_manual(values=c("Provider" = "longdash", "Recipient" = "solid"))+
  coord_cartesian(xlim = c(1, 10), ylim = c(-2.2, 2)) +
  theme_few() + 
  scale_color_manual(values=c("Provider" = "darkgreen", "Recipient" = "gray30"))+
  labs(x = "Time", y = "Heart Rate Variability") +
  labs(colour = "Partner", linetype = "Partner") +
  geom_hline(yintercept=0, color = 'gray', size = .5)+
  facet_wrap(~dyad) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
syncplotrec
#ggsave(syncplot2, file = "syncplot2_170420.pdf", height = 6, width = 8)



## during goal
aocsubgoal <- subset(aocsub1, suprole2=="GoalDisc")
aocsubgoal$time2 <- aocsubgoal$time-10
aocsubgoal$partner.lab2 <- ifelse(aocsubgoal$partner.lab=="Female", "Provider", "Recipient")
syncplotgoal <- ggplot(aocsubgoal, aes(time2, rsa.basec, color = partner.lab2)) + geom_line(size = 1, aes(linetype = partner.lab2)) + 
  scale_linetype_manual(values=c("Provider" = "longdash", "Recipient" = "solid"))+
  coord_cartesian(xlim = c(1, 10), ylim = c(-2.2, 2)) +
  theme_few() + 
  scale_color_manual(values=c("Provider" = "deeppink", "Recipient" = "blue"))+
  labs(x = "Time", y = "Heart Rate Variability") +
  labs(colour = "Partner", linetype = "Partner") +
  geom_hline(yintercept=0, color = 'gray', size = .5)+
  facet_wrap(~dyad) +
  scale_x_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
syncplotgoal
#ggsave(syncplotgoal, file = "syncplotgoal_170420.pdf", height = 6, width = 8)


####### RSA Graph - Support ############

## Male Provider predicting Female Recipient
summary(modelprovwc.rsabrm)



dphysstart <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                             max(aoc2$rsa.basec.lag, na.rm=T), .1),
                         timephase.r = 1, b.rsa.basec.lag   = 0)



dphysmiddle <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                              max(aoc2$rsa.basec.lag, na.rm=T), .1),
                          timephase.r = 5, b.rsa.basec.lag   =0)


dphysend <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                           max(aoc2$rsa.basec.lag, na.rm=T), .1),
                       timephase.r = 8, b.rsa.basec.lag   = 0)


fitstartphys <- cbind(dphysstart, fitted(modelprovwc.rsabrm, newdata = dphysstart, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitstartphys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

fitmiddlephys <- cbind(dphysmiddle, fitted(modelprovwc.rsabrm, newdata = dphysmiddle, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitmiddlephys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

fitendphys  <- cbind(dphysend, fitted(modelprovwc.rsabrm, newdata = dphysend, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitendphys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")




plotendphys <- ggplot(fitendphys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotendphys
#ggsave(plotendphys, file = "plotendphys_170417.pdf", height = 6, width = 4, bg = "transparent")


plotmidphys <- ggplot(fitmiddlephys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotmidphys
#ggsave(plotmidphys, file = "plotmidphys_170417.pdf", height = 6, width = 4, bg = "transparent")

plotstartphys <- ggplot(fitstartphys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotstartphys
#ggsave(plotstartphys, file = "plotstartphys_170417.pdf", height = 6, width = 4, bg = "transparent")






### For female provider predicting male recipient
dphysstart_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                               max(aoc2$rsa.basec.lag, na.rm=T), .1),
                           timephase.r = 1, b.rsa.basec.lag   = 0)



dphysmiddle_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                max(aoc2$rsa.basec.lag, na.rm=T), .1),
                            timephase.r = 5, b.rsa.basec.lag   = 0)


dphysend_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                             max(aoc2$rsa.basec.lag, na.rm=T), .1),
                         timephase.r = 8, b.rsa.basec.lag   = 0)


fitstartphys_f <- cbind(dphysstart_f, fitted(modelprovwc.rsabrm, newdata = dphysstart_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitstartphys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

fitmiddlephys_f <- cbind(dphysmiddle_f, fitted(modelprovwc.rsabrm, newdata = dphysmiddle_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitmiddlephys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

fitendphys_f  <- cbind(dphysend_f, fitted(modelprovwc.rsabrm, newdata = dphysend_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(fitendphys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")





plotendphys_f <- ggplot(fitendphys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink") + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotendphys_f
#ggsave(plotendphys_f, file = "plotendphys_f_170425.pdf", height = 6, width = 4, bg = "transparent")



plotmidphys_f <- ggplot(fitmiddlephys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink") + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotmidphys_f
#ggsave(plotmidphys_f, file = "plotmidphys_f_170425.pdf", height = 6, width = 4, bg = "transparent")

plotstartphys_f <- ggplot(fitstartphys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink") + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
plotstartphys_f
#ggsave(plotstartphys_f, file = "plotstartphys_f_170425.pdf", height = 6, width = 4, bg = "transparent")



###### RSA Graphs Receive Phase ########
summary(modelprovwc.rsabrm_rec)

# male recipient predicting female provider
rec_physstart <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                max(aoc2$rsa.basec.lag, na.rm=T), .1),
                            timephase.r = 1, b.rsa.basec.lag   = 0)



rec_physmiddle <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                 max(aoc2$rsa.basec.lag, na.rm=T), .1),
                             timephase.r = 5, b.rsa.basec.lag   =0)


rec_physend <- data.frame(pb = 1, pa = 0, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                              max(aoc2$rsa.basec.lag, na.rm=T), .1),
                          timephase.r = 8, b.rsa.basec.lag   = 0)


rec_fitstartphys <- cbind(rec_physstart, fitted(modelprovwc.rsabrm_rec, newdata = rec_physstart, re_formula = NA, incl_autocor = FALSE)[,-2])
names(rec_fitstartphys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

rec_fitmiddlephys <- cbind(rec_physmiddle, fitted(modelprovwc.rsabrm_rec, newdata = rec_physmiddle, re_formula = NA, incl_autocor = FALSE)[,-2])
names(rec_fitmiddlephys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

rec_fitendphys  <- cbind(rec_physend, fitted(modelprovwc.rsabrm_rec, newdata = rec_physend, re_formula = NA, incl_autocor = FALSE)[,-2])
names(rec_fitendphys) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")




rec_plotendphys <- ggplot(rec_fitendphys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
rec_plotendphys
#ggsave(rec_plotendphys, file = "rec_plotendphys_170418.pdf", height = 6, width = 4, bg = "transparent")


rec_plotmidphys <- ggplot(rec_fitmiddlephys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
rec_plotmidphys
#ggsave(rec_plotmidphys, file = "rec_plotmidphys_170418.pdf", height = 6, width = 4, bg = "transparent")

rec_plotstartphys <- ggplot(rec_fitstartphys, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "blue") + 
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "blue") +
  geom_line(color = "darkblue", size = 3) +
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
rec_plotstartphys
#ggsave(rec_plotstartphys, file = "rec_plotstartphys_170418.pdf", height = 6, width = 4, bg = "transparent")



##### Spaghetti Plot - Female Provide at end of Discussion ########
modelprovwc.rsabrm_end <-brm(b.rsa.basec ~ 0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + 
                               + b.rsa.basec.lag:pa + b.rsa.basec.lag:pb  + timephase.end:pa + timephase.end:pb +
                               rsa.basec.lag:timephase.end:pa + rsa.basec.lag:timephase.end:pb + 
                               (0 + pa + pb + rsa.basec.lag:pa + rsa.basec.lag:pb + timephase.end:pa + timephase.end:pb | dyad), 
                             data = aoc2, autocor = cor_ar(~timephase.end|id), cores = 4, chains = 1, iter = 100)
summary(modelprovwc.rsabrm_end)


modeleffect.re <- ranef(modelprovwc.rsabrm_end)
res1<-data.frame(as.numeric(rownames(modeleffect.re$dyad)),modeleffect.re$dyad)
colnames(res1)[1]="dyad"


res1$newpa <- modeleffect.re$dyad[,"pa"] + fixef(modelprovwc.rsabrm_end)["pa",] 
res1$newparsa <- modeleffect.re$dyad[,"pa:rsa.basec.lag"] + fixef(modelprovwc.rsabrm_end)["pa:rsa.basec.lag",]



# female predicting male across all phases, random effects
aocf <- subset(aoc2, partner=="A")
femalespagh <- ggplot(aocf, aes(rsa.basec.lag, b.rsa.basec)) +
  geom_abline(res1, intercept = res1$newpa, slope = res1$newparsa, color="hotpink1", lwd = 1, alpha = .5) + 
  geom_abline(res1, intercept = (fixef(modelprovwc.rsabrm_end)["pa",]), slope = fixef(modelprovwc.rsabrm_end)[3], color="violetred4", lwd = 2, alpha = 1) + 
  theme_few() + 
  scale_y_continuous(limits = c(-1, 1)) + 
  scale_x_continuous(limits = c(min(aocf$rsa.basec.lag, na.rm=T), max(aocf$rsa.basec.lag, na.rm=T)))+
  labs(x = "Provider HRV\n(within-person baseline centered, lagged)", y = "Recipient HRV (within-person baseline centered)")+
  theme(text=element_text(size=20))
femalespagh
#ggsave(femalespagh, file = "femalespagh_provide_170425.pdf", height = 7, width = 6)






####### Plot during Goal Disc ###########
summary(modelprovwc.rsabrm_goal)



goalphysstart_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                  max(aoc2$rsa.basec.lag, na.rm=T), .1),
                              timephase.r = 1, b.rsa.basec.lag   = 0)



goalphysmiddle_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                   max(aoc2$rsa.basec.lag, na.rm=T), .1),
                               timephase.r = 5, b.rsa.basec.lag   = 0)


goalphysend_f <- data.frame(pb = 0, pa = 1, rsa.basec.lag = seq(min(aoc2$rsa.basec.lag, na.rm=T), 
                                                                max(aoc2$rsa.basec.lag, na.rm=T), .1),
                            timephase.r = 8, b.rsa.basec.lag   = 0)


goalfitstartphys_f <- cbind(goalphysstart_f, fitted(modelprovwc.rsabrm_goal, newdata = goalphysstart_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(goalfitstartphys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

goalfitmiddlephys_f <- cbind(goalphysmiddle_f, fitted(modelprovwc.rsabrm_goal, newdata = goalphysmiddle_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(goalfitmiddlephys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")

goalfitendphys_f  <- cbind(goalphysend_f, fitted(modelprovwc.rsabrm_goal, newdata = goalphysend_f, re_formula = NA, incl_autocor = FALSE)[,-2])
names(goalfitendphys_f) <- c("pb", "pa", "rsa.basec.lag", "timephase.r", "b.rsa.basec.lag","b.rsa.basec", "lower", "upper")





goalplotendphys_f <- ggplot(goalfitendphys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink", size = .5) + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
goalplotendphys_f
#ggsave(goalplotendphys_f, file = "goalplotendphys_f_170417.pdf", height = 6, width = 4, bg = "transparent")



goalplotmidphys_f <- ggplot(goalfitmiddlephys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink", size = .5) + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
goalplotmidphys_f
#ggsave(goalplotmidphys_f, file = "goalplotmidphys_f_170417.pdf", height = 6, width = 4, bg = "transparent")

goalplotstartphys_f <- ggplot(goalfitstartphys_f, aes(x=rsa.basec.lag, y=b.rsa.basec)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = .3, fill = "deeppink") +
  geom_line(color = "deeppink", size = 2) +
  geom_point(data = aoc2, position = position_jitter(h=0.05, w=0.05), alpha=.2, color = "deeppink", size = .5) + 
  theme_few() +
  labs(x = "Lagged Provider RSA\n (baseline centered)", 
       y = "Recipient RSA (baseline centered)", title = "" )
goalplotstartphys_f
#ggsave(goalplotstartphys_f, file = "goalplotstartphys_f_170417.pdf", height = 6, width = 4, bg = "transparent")




