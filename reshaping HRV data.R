#### Reshaping HRV Data ######

hrv <- read.csv("/Users/zeekatherine/Desktop/ISP_HR_partialdata_wide.csv")

library(reshape)

##### Dataset Contents #####
# Three phases: 
# 1) Baseline (5 min, 5 60-sec segments)
# 2) Practice Speech (5 min, 5 60-sec segments)
# 3) Recovery (3 min, 3 60-sec segments)

##### Variables ######
# subj = subject ID
# Condition = support visibility condition (note that support occurred after practice and before recovery)
# time = segment/which minute
# HR = Heart Rate
# RSA = Respiratory Sinus Arrhythmia
# IBI = Interbeat Interval
# RRate = Respiration Rate
# RAmp = Respiration Amplitude
# phase = phase (three categories)
# prac.phase = dummy coded variable indicating practice phase (1 = practice, 0 = not practice)
# recov.phase = dummy coded variable indicating recovery phase (1 = recovery, 0 = not recovery)

hrvlong.1 <- reshape(hrv, varying=c("HR_1", "HR_2", "HR_3", "HR_4", "HR_5", "HR_6", "HR_7",
                                  "HR_8", "HR_9", "HR_10", "HR_11", "HR_12","HR_13",
                                  "RSA_1", "RSA_2", "RSA_3", "RSA_4", "RSA_5", "RSA_6", "RSA_7",
                                  "RSA_8", "RSA_9", "RSA_10", "RSA_11", "RSA_12","RSA_13",
                                  "IBI_1", "IBI_2", "IBI_3", "IBI_4", "IBI_5", "IBI_6", "IBI_7",
                                  "IBI_8", "IBI_9", "IBI_10", "IBI_11", "IBI_12","IBI_13",
                                  "RRate_1", "RRate_2", "RRate_3", "RRate_4", "RRate_5", "RRate_6", "RRate_7",
                                  "RRate_8", "RRate_9", "RRate_10", "RRate_11", "RRate_12","RRate_13",
                                  "RAmp_1", "RAmp_2", "RAmp_3", "RAmp_4", "RAmp_5", "RAmp_6", "RAmp_7",
                                  "RAmp_8", "RAmp_9", "RAmp_10", "RAmp_11", "RAmp_12","RAmp_13"), direction = "long",
                                  idvar = "subj", sep = "_")

hrvlong <- hrvlong.1[order(hrvlong.1$subj),]


hrvlong$phase <- hrvlong$time
hrvlong$phase <- "practice"
hrvlong$phase[hrvlong$time<6] <- "baseline"
hrvlong$phase[hrvlong$time>10] <- "recovery"

hrvlong$prac.phase <- 0
hrvlong$prac.phase[hrvlong$phase=="practice"] <- 1

hrvlong$recov.phase <- 0
hrvlong$recov.phase[hrvlong$phase=="recovery"] <- 1


write.csv(x = hrvlong, file = "ISP_HR_partialdata_long.csv", row.names=F)



