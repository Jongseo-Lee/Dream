library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")

# 문제인식
data_1 <- (rawdata$A17_1 + rawdata$A17_2 + rawdata$A17_3)/3
head(data_1)
data_1_mean <- mean(data_1)

summary(data_1)
sd(data_1)
var(data_1)

result1 <- ifelse(data_1 > data_1_mean, 2, 1)
rawdata$result1 <- result1
result1

# 주민참여
data_2 <- (rawdata$A17_4 + rawdata$A17_5 + rawdata$A17_6)/3
head(data_2)
data_2_mean <- mean(data_2)

summary(data_2)
sd(data_2)
var(data_2)

result2 <- ifelse(data_2 > data_2_mean, 2, 1)
rawdata$result2 <- result2
result2

# 건강위험
data_3 <- (rawdata$A17_7 + rawdata$A17_8)/2
head(data_3)
data_3_mean <- mean(data_3)

summary(data_3)
sd(data_3)
var(data_3)

result3 <- ifelse(data_3 > data_3_mean, 2, 1)
rawdata$result3 <- result3
result3

# 경제적위험
data_4 <- (rawdata$A17_9 + rawdata$A17_10)/2
head(data_4)
data_4_mean <- mean(data_4)

summary(data_4)
sd(data_4)
var(data_4)

result4 <- ifelse(data_4 > data_4_mean, 2, 1)
rawdata$result4 <- result4
result4

b <- cbind(data_1, data_2, data_3, data_4)

cor(b[,c(1:4)])

View(rawdata)

r <- cbind(result1, result2, result3, result4)~1
set.seed(123)
lca <- poLCA(r, data=rawdata, nclass = 2)

