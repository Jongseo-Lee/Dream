library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")

df_1 <- rawdata %>%  # 문제인식 
  select(A17_1:A17_3)
df_1 <- as.matrix(df_1)
df_1_mean <- mean(df_1)
summary(df_1)
cor(df_1)

result_1 <- as.data.frame(ifelse(df_1 > df_1_mean, 2, 1))

df1_1 <- result_1$A17_1
df1_2 <- result_1$A17_2
df1_3 <- result_1$A17_3
bind1 <- as.data.frame(cbind(df1_1, df1_2, df1_3))
r1 <- stack(bind1)
r1 <- subset(r1, select=-ind)
r1[,'id'] <- 1:nrow(r1)
r1

rawdata$result1 <- r1

df_2 <- rawdata %>%  # 주민참여 
  select(A17_4:A17_6)
df_2 <- as.matrix(df_2)
df_2_mean <- mean(df_2)
summary(df_2)
cor(df_2)

result_2 <- as.data.frame(ifelse(df_2 > df_2_mean, 2, 1))

df2_1 <- result_2$A17_4
df2_2 <- result_2$A17_5
df2_3 <- result_2$A17_6
bind2 <- as.data.frame(cbind(df2_1, df2_2, df2_3))
r2 <- stack(bind2)
r2 <- subset(r2, select=-ind)
r2[,'id'] <- 1:nrow(r2)
r2

df_3 <- rawdata %>%  # 건강위험
  select(A17_7:A17_8)
df_3 <- as.matrix(df_3)
df_3_mean <- mean(df_3)
summary(df_3)
cor(df_3)

result_3 <- as.data.frame(ifelse(df_3 > df_3_mean, 2, 1))

df3_1 <- result_3$A17_7
df3_2 <- result_3$A17_8
bind3 <- as.data.frame(cbind(df3_1, df3_2))
r3 <- stack(bind3)
r3 <- subset(r3, select=-ind)
r3[,'id'] <- 1:nrow(r3)
r3

df_4 <- rawdata %>%  # 경제적위험 
  select(A17_9:A17_10)
df_4 <- as.matrix(df_4)
df_4_mean <- mean(df_4)
summary(df_4)
cor(df_4)

result_4 <- as.data.frame(ifelse(df_4 > df_4_mean, 2, 1))

df4_1 <- result_4$A17_9
df4_2 <- result_4$A17_10
bind4 <- as.data.frame(cbind(df4_1, df4_2))
bind4
r4 <- stack(bind4)
r4 <- subset(r4, select=-ind)
r4[,'id'] <- 1:nrow(r4)
r4

length(r1) <- max(length(r1), length(r2), length(r3), length(r4))
length(r2) <- max(length(r1), length(r2), length(r3), length(r4))
length(r3) <- max(length(r1), length(r2), length(r3), length(r4))
length(r4) <- max(length(r1), length(r2), length(r3), length(r4))

head(r1)
r2
r3
r4
do.call(cbind, list(r1,r2,r3,r4))

m <- merge(r1,r2,by="id")
n <- merge(r3,r4,by="id")
b <- merge(n,m,by="id")
b <- as.data.frame(b)
b
LCA <- poLCA(b, data=rawdata, nclass = 4)

nm <- list("r1", "r2", "r3", "r4")

for(i in 1:length(nm)) 
