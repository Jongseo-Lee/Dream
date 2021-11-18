library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")

# 1
data_1 <- rawdata$A17_1
head(data_1)
data_1_mean <- mean(data_1)

result1 <- ifelse(data_1 > data_1_mean, 2, 1)
rawdata$result1 <- result1
result1

# 2
data_2 <- rawdata$A17_2
head(data_2)
data_2_mean <- mean(data_2)

result2 <- ifelse(data_2 > data_2_mean, 2, 1)
rawdata$result2 <- result2
result2

# 3
data_3 <- rawdata$A17_3
head(data_3)
data_3_mean <- mean(data_3)

result3 <- ifelse(data_3 > data_3_mean, 2, 1)
rawdata$result3 <- result3
result3

# 4
data_4 <- rawdata$A17_4
head(data_4)
data_4_mean <- mean(data_4)

result4 <- ifelse(data_4 > data_4_mean, 2, 1)
rawdata$result4 <- result4
result4

# 5
data_5 <- rawdata$A17_5
data_5_mean <- mean(data_5)

result5 <- ifelse(data_5 > data_5_mean, 2, 1)
rawdata$result5 <- result5
result5

# 6
data_6 <- rawdata$A17_6
data_6_mean <- mean(data_6)

result6 <- ifelse(data_6 > data_6_mean, 2, 1)
rawdata$result6 <- result6
result6

# 7
data_7 <- rawdata$A17_7
data_7_mean <- mean(data_7)

result7 <- ifelse(data_7 > data_7_mean, 2, 1)
rawdata$result7 <- result7
result7

# 8
data_8 <- rawdata$A17_8
data_8_mean <- mean(data_8)

result8 <- ifelse(data_8 > data_8_mean, 2, 1)
rawdata$result8 <- result8
result8

# 9
data_9 <- rawdata$A17_9
data_9_mean <- mean(data_9)

result9 <- ifelse(data_9 > data_9_mean, 2, 1)
rawdata$result9 <- result9
result9

# 10
data_10 <- rawdata$A17_10
data_10_mean <- mean(data_10)

result10 <- ifelse(data_10 > data_10_mean, 2, 1)
rawdata$result10 <- result10
result10


r <- cbind(result1, result2, result3, result4, result5, result6, result7,
           result8, result9, result10)~1
set.seed(123)
lca <- poLCA(r, data=rawdata, nclass = 4)


