library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")

# 1
data_1 <- rawdata$A17_1
data_1_mean <- mean(data_1)

result1 <- ifelse(data_1 > data_1_mean, 2, 1)
rawdata$result1 <- result1

# 2
data_2 <- rawdata$A17_2
data_2_mean <- mean(data_2)

result2 <- ifelse(data_2 > data_2_mean, 2, 1)
rawdata$result2 <- result2

# 3
data_3 <- rawdata$A17_3
data_3_mean <- mean(data_3)

result3 <- ifelse(data_3 > data_3_mean, 2, 1)
rawdata$result3 <- result3

# 4
data_4 <- rawdata$A17_4
data_4_mean <- mean(data_4)

result4 <- ifelse(data_4 > data_4_mean, 2, 1)
rawdata$result4 <- result4

# 5
data_5 <- rawdata$A17_5
data_5_mean <- mean(data_5)

result5 <- ifelse(data_5 > data_5_mean, 2, 1)
rawdata$result5 <- result5

# 6
data_6 <- rawdata$A17_6
data_6_mean <- mean(data_6)

result6 <- ifelse(data_6 > data_6_mean, 2, 1)
rawdata$result6 <- result6

# 7
data_7 <- rawdata$A17_7
data_7_mean <- mean(data_7)

result7 <- ifelse(data_7 > data_7_mean, 2, 1)
rawdata$result7 <- result7

# 8
data_8 <- rawdata$A17_8
data_8_mean <- mean(data_8)

result8 <- ifelse(data_8 > data_8_mean, 2, 1)
rawdata$result8 <- result8

# 9
data_9 <- rawdata$A17_9
data_9_mean <- mean(data_9)

result9 <- ifelse(data_9 > data_9_mean, 2, 1)
rawdata$result9 <- result9

# 10
data_10 <- rawdata$A17_10
data_10_mean <- mean(data_10)

result10 <- ifelse(data_10 > data_10_mean, 2, 1)
rawdata$result10 <- result10

# 잠재계층분석
r <- cbind(result1, result2, result3, result4, result5, result6, result7,
           result8, result9, result10)~1
set.seed(123)
lca <- poLCA(r, data=rawdata, nclass = 4)

# rawdata 계층구분
rawdata1 <- cbind(rawdata, lca$predclass)
View(rawdata1)


# 데이터 전처리
rawdata1$SQ1 <- ifelse(rawdata1$SQ1 == 1, "남성", "여성") # 성별

rawdata1$SQ2_2 <- ifelse(rawdata1$SQ2_2 == 2, "20대",  # 연령대별
                            ifelse(rawdata1$SQ2_2 == 3, "30대",
                                   ifelse(rawdata1$SQ2_2 == 4, "40대",
                                          ifelse(rawdata1$SQ2_2 == 5, "50대", "60대"))))

rawdata1$DQ8_1 <- ifelse(rawdata1$DQ8_1 == 1, "100 만원 이하", # 가구소득별
                            ifelse(rawdata1$DQ8_1 %in% c(2,3), "100~200만원",
                                   ifelse(rawdata1$DQ8_1 %in% c(4,5), "200~300만원", 
                                          ifelse(rawdata1$DQ8_1 %in% c(6), "300~400만원",
                                                 ifelse(rawdata1$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))

rawdata1$A1 <- ifelse(rawdata1$A1 == 1, "네", "아니오") # 송전탑 가시거리 거주별

options(digits = 4) # 소수점 확대

# 1번 계층
class1 <- rawdata1 %>% 
  filter(`lca$predclass`==1)

# class1. 성별 구분
class1_sex <- class1 %>%
  select(SQ1)
t1_1 <- table(class1_sex)
prop.table(t1_1)

# class1. 연령대 구분
class1_age <- class1 %>%
  select(SQ2_2)
t1_2 <- table(class1_age)
prop.table(t1_2)

# class1. 소득 구분
class1_income <- class1 %>%
  select(DQ8_1)
t1_3 <- table(class1_income)
prop.table(t1_3)

# class1. 송전탑 가시거리 거주 구분
class1_distance <- class1 %>%
  select(A1)
t1_4 <- table(class1_distance)
prop.table(t1_4)

# class1. 수용성
class1_reseptivity <- class1 %>%
  select(A13)
t1_5 <- table(class1_reseptivity)
prop.table(t1_5)

# class1. 보상만족도
class1_satisfaction <- class1 %>%
  select(A9)
mean(as.matrix(class1_satisfaction))
t1_6 <- table(class1_satisfaction)
prop.table(t1_6)

# class1. 보상방안
class1_method1 <- class1 %>%
  select(A16_1)
mean(as.matrix(class1_method1))

class1_method2 <- class1 %>%
  select(A16_2)
mean(as.matrix(class1_method2))

class1_method3 <- class1 %>%
  select(A16_3)
mean(as.matrix(class1_method3))

class1_method4 <- class1 %>%
  select(A16_4)
mean(as.matrix(class1_method4))

class1_method5 <- class1 %>%
  select(A16_5)
mean(as.matrix(class1_method5))

class1_method6 <- class1 %>%
  select(A16_6)
mean(as.matrix(class1_method6))

#2번 계층
class2 <- rawdata1 %>% 
  filter(`lca$predclass`==2)

# class2. 성별 구분
class2_sex <- class2 %>%
  select(SQ1)
t2_1 <- table(class2_sex)
prop.table(t2_1)

# class2. 연령대 구분
class2_age <- class2 %>%
  select(SQ2_2)
t2_2 <- table(class2_age)
prop.table(t2_2)

# class2. 소득 구분
class2_income <- class2 %>%
  select(DQ8_1)
t2_3 <- table(class2_income)
prop.table(t2_3)

# class2. 송전탑 가시거리 거주 구분
class2_distance <- class2 %>%
  select(A1)
t2_4 <- table(class2_distance)
prop.table(t2_4)

# class2. 수용성
class2_reseptivity <- class2 %>%
  select(A13)
t2_5 <- table(class2_reseptivity)
prop.table(t2_5)

# class2. 보상만족도
class2_satisfaction <- class2 %>%
  select(A9)
mean(as.matrix(class2_satisfaction))

# class2. 보상방안
class2_method1 <- class2 %>%
  select(A16_1)
mean(as.matrix(class2_method1))

class2_method2 <- class2 %>%
  select(A16_2)
mean(as.matrix(class2_method2))

class2_method3 <- class2 %>%
  select(A16_3)
mean(as.matrix(class2_method3))

class2_method4 <- class2 %>%
  select(A16_4)
mean(as.matrix(class2_method4))

class2_method5 <- class2 %>%
  select(A16_5)
mean(as.matrix(class2_method5))

class2_method6 <- class2 %>%
  select(A16_6)
mean(as.matrix(class2_method6))

#3번 계층
class3 <- rawdata1 %>% 
  filter(`lca$predclass`==3)

# class3. 성별 구분
class3_sex <- class3 %>%
  select(SQ1)
t3_1 <- table(class3_sex)
prop.table(t3_1)

# class3. 연령대 구분
class3_age <- class3 %>%
  select(SQ2_2)
t3_2 <- table(class3_age)
prop.table(t3_2)

# class3. 소득 구분
class3_income <- class3 %>%
  select(DQ8_1)
t3_3 <- table(class3_income)
prop.table(t3_3)

# class3. 송전탑 가시거리 거주 구분
class3_distance <- class3 %>%
  select(A1)
t3_4 <- table(class3_distance)
prop.table(t3_4)

# class3. 수용성
class3_reseptivity <- class3 %>%
  select(A13)
t3_5 <- table(class3_reseptivity)
prop.table(t3_5)

# class3. 보상만족도
class3_satisfaction <- class3 %>%
  select(A9)
mean(as.matrix(class3_satisfaction))

# class3. 보상방안
class3_method1 <- class3 %>%
  select(A16_1)
mean(as.matrix(class3_method1))

class3_method2 <- class3 %>%
  select(A16_2)
mean(as.matrix(class3_method2))

class3_method3 <- class3 %>%
  select(A16_3)
mean(as.matrix(class3_method3))

class3_method4 <- class3 %>%
  select(A16_4)
mean(as.matrix(class3_method4))

class3_method5 <- class3 %>%
  select(A16_5)
mean(as.matrix(class3_method5))

class3_method6 <- class3 %>%
  select(A16_6)
mean(as.matrix(class3_method6))

#4번 계층
class4 <- rawdata1 %>% 
  filter(`lca$predclass`==4)

# class4. 성별 구분
class4_sex <- class4 %>%
  select(SQ1)
t4_1 <- table(class4_sex)
prop.table(t4_1)

# class4. 연령대 구분
class4_age <- class4 %>%
  select(SQ2_2)
t4_2 <- table(class4_age)
prop.table(t4_2)

# class4. 소득 구분
class4_income <- class4 %>%
  select(DQ8_1)
t4_3 <- table(class4_income)
prop.table(t4_3)

# class4. 송전탑 가시거리 거주 구분
class4_distance <- class4 %>%
  select(A1)
t4_4 <- table(class4_distance)
prop.table(t4_4)

# class4. 수용성
class4_reseptivity <- class4 %>%
  select(A13)
t4_5 <- table(class4_reseptivity)
prop.table(t4_5)

# class4. 보상만족도
class4_satisfaction <- class4 %>%
  select(A9)
mean(as.matrix(class4_satisfaction))

# class4. 보상방안
class4_method1 <- class4 %>%
  select(A16_1)
mean(as.matrix(class4_method1))

class4_method2 <- class4 %>%
  select(A16_2)
mean(as.matrix(class4_method2))

class4_method3 <- class4 %>%
  select(A16_3)
mean(as.matrix(class4_method3))

class4_method4 <- class4 %>%
  select(A16_4)
mean(as.matrix(class4_method4))

class4_method5 <- class4 %>%
  select(A16_5)
mean(as.matrix(class4_method5))

class4_method6 <- class4 %>%
  select(A16_6)
mean(as.matrix(class4_method6))
