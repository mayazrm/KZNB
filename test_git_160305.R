### Test Script for Github ###

library(car)
data(Moore)

summary(Moore$conformity)
table(Moore$partner.status)

plot(Moore$fscore, Moore$conformity) 
