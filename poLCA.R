library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")

df_1 <- rawdata %>%  # 문제인식 
  select(A17_1:A17_3)
df_1 <- as.matrix(df_1)
df_1_mean <- mean(df_1)
summary(df_1)

sd(rawdata$A17_1)
sd(rawdata$A17_2)
sd(rawdata$A17_3)

var(rawdata$A17_1)
var(rawdata$A17_2)
var(rawdata$A17_3)

cor(df_1)

result_1 <- ifelse(df_1 > df_1_mean, 2, 1)

df_2 <- rawdata %>%  # 주민참여 
  select(A17_4:A17_6)
df_2 <- as.matrix(df_2)
df_2_mean <- mean(df_2)
summary(df_2)
cor(df_2)

sd(rawdata$A17_4)
sd(rawdata$A17_5)
sd(rawdata$A17_6)

var(rawdata$A17_4)
var(rawdata$A17_5)
var(rawdata$A17_6)

result_2 <- ifelse(df_2 > df_2_mean, 2, 1)

df_3 <- rawdata %>%  # 건강위험
  select(A17_7:A17_8)
df_3 <- as.matrix(df_3)
df_3_mean <- mean(df_3)
summary(df_3)
cor(df_3)

sd(rawdata$A17_7)
sd(rawdata$A17_8)

var(rawdata$A17_7)
var(rawdata$A17_8)

result_3 <- ifelse(df_3 > df_3_mean, 2, 1)

df_4 <- rawdata %>%  # 경제적위험 
  select(A17_9:A17_10)
df_4 <- as.matrix(df_4)
df_4_mean <- mean(df_4)
summary(df_4)
cor(df_4)

sd(rawdata$A17_9)
sd(rawdata$A17_10)

var(rawdata$A17_9)
var(rawdata$A17_10)

result_4 <- ifelse(df_4 > df_4_mean, 2, 1)

df <- cbind(result_1, result_2, result_3, result_4)~1
LCA <- poLCA(df, data=rawdata, nclass = 4)
