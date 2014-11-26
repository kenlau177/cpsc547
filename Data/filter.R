
setwd("C://users//Ken//Documents//GitHub//cpsc547//Data")

library(dplyr)

a = read.table("datTrn_small.txt", sep=",", header=T)
b = a %>% select(m1,m2,m3,m4,m5,cl)

write.table(b, "datTrn_small.txt", sep=",")


a = read.table("datTest_small.txt", sep=",", header=T)
b = a %>% select(m1,m2,m3,m4,m5,cl)

write.table(b, "datTest_small.txt", sep=",")

