library(poLCA)
library(dplyr)
library(haven)
library(tidyverse)

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
lca

# rawdata 계층구분
rawdata1 <- cbind(rawdata, lca$predclass)


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

rawdata1$DQ8_2 <- ifelse(rawdata1$DQ8_2 == 1, "100 만원 이하", # 개인소득별
                         ifelse(rawdata1$DQ8_2 %in% c(2,3), "100~200만원",
                                ifelse(rawdata1$DQ8_2 %in% c(4,5), "200~300만원", 
                                       ifelse(rawdata1$DQ8_2 %in% c(6), "300~400만원",
                                              ifelse(rawdata1$DQ8_2 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))

rawdata1$DQ4 <- ifelse(rawdata1$DQ4 == 0, "무학", # 학력별
                         ifelse(rawdata1$DQ4 %in% c(1,2,3,4,5,6), "초등학교 졸업",
                                ifelse(rawdata1$DQ4 %in% c(7,8,9), "중학교 졸업", 
                                       ifelse(rawdata1$DQ4 %in% c(10,11,12), "고등학교 졸업",
                                              ifelse(rawdata1$DQ4 %in% c(13,14,15,16), "대학교 졸업", "대학원 졸업")))))


rawdata1$A1 <- ifelse(rawdata1$A1 == 1, "네", "아니오") # 송전탑 가시거리 거주별

options(digits = 4) # 소수점 확대

# 4번 계층
class1 <- rawdata1 %>% 
  filter(`lca$predclass`==1)

# class4. 성별 구분
class1_sex <- class1 %>%
  select(SQ1)
t1_1 <- table(class1_sex)
prop.table(t1_1)

# class4. 연령대 구분
class1_age <- class1 %>%
  select(SQ2_2)
t1_2 <- table(class1_age)
prop.table(t1_2)

# class4. 소득 구분
class1_income <- class1 %>%
  select(DQ8_1)
t1_3 <- table(class1_income)
prop.table(t1_3)

# class4. 개인소득 구분
class1_income1 <- class1 %>%
  select(DQ8_2)
t1_31 <- table(class1_income1)
prop.table(t1_31)

# class4. 학력 구분
class1_edu <- class1 %>%
  select(DQ4)
t1_7 <- table(class1_edu)
prop.table(t1_7)

# class4. 송전탑 가시거리 거주 구분
class1_distance <- class1 %>%
  select(A1)
t1_4 <- table(class1_distance)
prop.table(t1_4)

# class4. 수용성
class1_reseptivity <- class1 %>%
  select(A13)
t1_5 <- table(class1_reseptivity)
prop.table(t1_5)

# class4. 보상만족도
class1_satisfaction <- class1 %>%
  select(A9)
mean(as.matrix(class1_satisfaction))
t1_6 <- table(class1_satisfaction)
prop.table(t1_6)

# class4. 보상방안
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

# class4. 문4
class1_q4 <- class1 %>%
  select(A4)
t_q4 <- table(class1_q4)
prop.table(t_q4)

# class4. 문5
class1_q5 <- class1 %>%
  select(A5)
t_q5 <- table(class1_q5)
prop.table(t_q5)

# class4. 문6 1순위
class1_q6 <- class1 %>%
  select(A6_1)
t_q6 <- table(class1_q6)
prop.table(t_q6)

# class4. 문6 2순위
class1_q62 <- class1 %>%
  select(A6_2)
t_q62 <- table(class1_q62)
prop.table(t_q62)

# class4. 문7 1순위
class1_q7 <- class1 %>%
  select(A7_1)
t_q7 <- table(class1_q7)
prop.table(t_q7)

# class4. 문8
class1_q8 <- class1 %>%
  select(A8_1)
t_q8 <- table(class1_q8)
prop.table(t_q8)

# class4. 문9
class1_q9 <- class1 %>%
  select(A9)
mean(as.matrix(class1_q9))
sd(as.matrix(class1_q9))

# class4. 문12
class1_q12 <- class1 %>%
  select(A12)
mean(as.matrix(class1_q12))
sd(as.matrix(class1_q12))

# class4. 문14
class1_q14 <- class1 %>%
  select(A14)
t_q14 <- table(class1_q14)
prop.table(t_q14)

# class4. 문14
class1_q15 <- class1 %>%
  select(A15)
t_q15 <- table(class1_q15)
prop.table(t_q15)

# 3번 계층
class2 <- rawdata1 %>% 
  filter(`lca$predclass`==2)

# class3. 성별 구분
class2_sex <- class2 %>%
  select(SQ1)
t2_1 <- table(class2_sex)
prop.table(t2_1)

# class3. 연령대 구분
class2_age <- class2 %>%
  select(SQ2_2)
t2_2 <- table(class2_age)
prop.table(t2_2)

# class3. 소득 구분
class2_income <- class2 %>%
  select(DQ8_1)
t2_3 <- table(class2_income)
prop.table(t2_3)

# class3. 개인소득 구분
class2_income1 <- class2 %>%
  select(DQ8_2)
t2_31 <- table(class2_income1)
prop.table(t2_31)

# class3. 학력 구분
class2_edu <- class2 %>%
  select(DQ4)
t2_7 <- table(class2_edu)
prop.table(t2_7)

# class3. 송전탑 가시거리 거주 구분
class2_distance <- class2 %>%
  select(A1)
t2_4 <- table(class2_distance)
prop.table(t2_4)

# class3. 수용성
class2_reseptivity <- class2 %>%
  select(A13)
t2_5 <- table(class2_reseptivity)
prop.table(t2_5)

# class3. 보상만족도
class2_satisfaction <- class2 %>%
  select(A9)
mean(as.matrix(class2_satisfaction))

# class3. 보상방안
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

# class3. 문4
class2_q4 <- class2 %>%
  select(A4)
t2_q4 <- table(class2_q4)
prop.table(t2_q4)

# class3. 문5
class2_q5 <- class2 %>%
  select(A5)
t2_q5 <- table(class2_q5)
prop.table(t2_q5)

# class3. 문6 1순위
class2_q6 <- class2 %>%
  select(A6_1)
t2_q6 <- table(class2_q6)
prop.table(t2_q6)

# class3. 문6 2순위
class2_q62 <- class2 %>%
  select(A6_2)
t2_q62 <- table(class2_q62)
prop.table(t2_q62)

# class3. 문7 1순위
class2_q7 <- class2 %>%
  select(A7_1)
t2_q7 <- table(class2_q7)
prop.table(t2_q7)

# class3. 문8
class2_q8 <- class2 %>%
  select(A8_1)
t2_q8 <- table(class2_q8)
prop.table(t2_q8)

# class3. 문9
class2_q9 <- class2 %>%
  select(A9)
mean(as.matrix(class2_q9))
sd(as.matrix(class2_q9))

# class3. 문12
class2_q12 <- class2 %>%
  select(A12)
mean(as.matrix(class2_q12))
sd(as.matrix(class2_q12))

# class3. 문14
class2_q14 <- class2 %>%
  select(A14)
t_q14 <- table(class2_q14)
prop.table(t_q14)

# class3. 문14
class2_q15 <- class2 %>%
  select(A15)
t_q15 <- table(class2_q15)
prop.table(t_q15)

# 2번 계층
class3 <- rawdata1 %>% 
  filter(`lca$predclass`==3)

# class2. 성별 구분
class3_sex <- class3 %>%
  select(SQ1)
t3_1 <- table(class3_sex)
prop.table(t3_1)

# class2. 연령대 구분
class3_age <- class3 %>%
  select(SQ2_2)
t3_2 <- table(class3_age)
prop.table(t3_2)

# class2. 소득 구분
class3_income <- class3 %>%
  select(DQ8_1)
t3_3 <- table(class3_income)
prop.table(t3_3)

# class2. 개인소득 구분
class3_income1 <- class3 %>%
  select(DQ8_2)
t3_31 <- table(class3_income1)
prop.table(t3_31)

# class2. 학력 구분
class3_edu <- class3 %>%
  select(DQ4)
t3_7 <- table(class3_edu)
prop.table(t3_7)

# class2. 송전탑 가시거리 거주 구분
class3_distance <- class3 %>%
  select(A1)
t3_4 <- table(class3_distance)
prop.table(t3_4)

# class2. 수용성
class3_reseptivity <- class3 %>%
  select(A13)
t3_5 <- table(class3_reseptivity)
prop.table(t3_5)

# class2. 보상만족도
class3_satisfaction <- class3 %>%
  select(A9)
mean(as.matrix(class3_satisfaction))

# class2. 보상방안
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

# class2. 문4
class3_q4 <- class3 %>%
  select(A4)
t3_q4 <- table(class3_q4)
prop.table(t3_q4)

# class2. 문5
class3_q5 <- class3 %>%
  select(A5)
t3_q5 <- table(class3_q5)
prop.table(t3_q5)

# class2. 문6 1순위
class3_q6 <- class3 %>%
  select(A6_1)
t3_q6 <- table(class3_q6)
prop.table(t3_q6)

# class2. 문6 2순위
class3_q62 <- class3 %>%
  select(A6_2)
t3_q62 <- table(class3_q62)
prop.table(t3_q62)

# class2. 문7 1순위
class3_q7 <- class3 %>%
  select(A7_1)
t3_q7 <- table(class3_q7)
prop.table(t3_q7)

# class2. 문8
class3_q8 <- class3 %>%
  select(A8_1)
t3_q8 <- table(class3_q8)
prop.table(t3_q8)

# class2. 문9
class3_q9 <- class3 %>%
  select(A9)
mean(as.matrix(class3_q9))
sd(as.matrix(class3_q9))

# class2. 문12
class3_q12 <- class3 %>%
  select(A12)
mean(as.matrix(class3_q12))
sd(as.matrix(class3_q12))

# class2. 문14
class3_q14 <- class3 %>%
  select(A14)
t_q14 <- table(class3_q14)
prop.table(t_q14)

# class2. 문14
class3_q15 <- class3 %>%
  select(A15)
t_q15 <- table(class3_q15)
prop.table(t_q15)

# 1번 계층
class4 <- rawdata1 %>% 
  filter(`lca$predclass`==4)

# class1. 성별 구분
class4_sex <- class4 %>%
  select(SQ1)
t4_1 <- table(class4_sex)
prop.table(t4_1)

# class1. 연령대 구분
class4_age <- class4 %>%
  select(SQ2_2)
t4_2 <- table(class4_age)
prop.table(t4_2)

# class1. 소득 구분
class4_income <- class4 %>%
  select(DQ8_1)
t4_3 <- table(class4_income)
prop.table(t4_3)

# class1. 개인소득 구분
class4_income1 <- class4 %>%
  select(DQ8_2)
t4_31 <- table(class4_income1)
prop.table(t4_31)

# class1. 학력 구분
class4_edu <- class4 %>%
  select(DQ4)
t4_7 <- table(class4_edu)
prop.table(t4_7)

# class1. 송전탑 가시거리 거주 구분
class4_distance <- class4 %>%
  select(A1)
t4_4 <- table(class4_distance)
prop.table(t4_4)

# class1. 수용성
class4_reseptivity <- class4 %>%
  select(A13)
t4_5 <- table(class4_reseptivity)
prop.table(t4_5)

# class1. 보상만족도
class4_satisfaction <- class4 %>%
  select(A9)
mean(as.matrix(class4_satisfaction))

# class1. 보상방안
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

# class1. 문4
class4_q4 <- class4 %>%
  select(A4)
t4_q4 <- table(class4_q4)
prop.table(t4_q4)

# class1. 문5
class4_q5 <- class4 %>%
  select(A5)
t4_q5 <- table(class4_q5)
prop.table(t4_q5)

# class1. 문6 1순위
class4_q6 <- class4 %>%
  select(A6_1)
t4_q6 <- table(class4_q6)
prop.table(t4_q6)

# class1. 문6 2순위
class4_q62 <- class4 %>%
  select(A6_2)
t4_q62 <- table(class4_q62)
prop.table(t4_q62)

# class1. 문7 1순위
class4_q7 <- class4 %>%
  select(A7_1)
t4_q7 <- table(class4_q7)
prop.table(t4_q7)

# class1. 문8
class4_q8 <- class4 %>%
  select(A8_1)
t4_q8 <- table(class4_q8)
prop.table(t4_q8)

# class1. 문9
class4_q9 <- class4 %>%
  select(A9)
mean(as.matrix(class4_q9))
sd(as.matrix(class4_q9))

# class1. 문12
class4_q12 <- class4 %>%
  select(A12)
mean(as.matrix(class4_q12))
sd(as.matrix(class4_q12))

# class1. 문14
class4_q14 <- class4 %>%
  select(A14)
t_q14 <- table(class4_q14)
prop.table(t_q14)

# class1. 문14
class4_q15 <- class4 %>%
  select(A15)
t_q15 <- table(class4_q15)
prop.table(t_q15)

# 전체 데이터

# class. 성별 구분
class_sex <- rawdata1 %>%
  select(SQ1)
t_1 <- table(class_sex)
prop.table(t_1)

# class. 연령대 구분
class_age <- rawdata1 %>%
  select(SQ2_2)
t_2 <- table(class_age)
prop.table(t_2)

# class. 소득 구분
class_income <- rawdata1 %>%
  select(DQ8_1)
t_3 <- table(class_income)
prop.table(t_3)

# class. 개인소득 구분
class_income31 <- rawdata1 %>%
  select(DQ8_2)
t_31 <- table(class_income31)
prop.table(t_31)

# class. 학력 구분
class_edu <- rawdata1 %>%
  select(DQ4)
t_23 <- table(class_edu)
prop.table(t_23)


# class. 송전탑 가시거리 거주 구분
class_distance <- rawdata1 %>%
  select(A1)
t_4 <- table(class_distance)
prop.table(t_4)

# class. 수용성
class_reseptivity <- rawdata1 %>%
  select(A13)
t_5 <- table(class_reseptivity)
prop.table(t_5)

# class. 보상만족도
class_satisfaction <- rawdata1 %>%
  select(A9)
mean(as.matrix(class_satisfaction))

# class. 보상방안
class_method1 <- rawdata1 %>%
  select(A16_1)
mean(as.matrix(class_method1))

class_method2 <- rawdata1 %>%
  select(A16_2)
mean(as.matrix(class_method2))

class_method3 <- rawdata1 %>%
  select(A16_3)
mean(as.matrix(class_method3))

class_method4 <- rawdata1 %>%
  select(A16_4)
mean(as.matrix(class_method4))

class_method5 <- rawdata1 %>%
  select(A16_5)
mean(as.matrix(class_method5))

class_method6 <- rawdata1 %>%
  select(A16_6)
mean(as.matrix(class_method6))

# class. 문4
class_Q4 <- rawdata1 %>%
  select(A4)
t1_1 <- table(class_Q4)
prop.table(t1_1)

# class. 문5
class_Q5 <- rawdata1 %>%
  select(A5)
t1_2 <- table(class_Q5)
prop.table(t1_2)

# class. 문6_1
class_Q6 <- rawdata1 %>%
  select(A6_1)
t1_3 <- table(class_Q6)
prop.table(t1_3)

# class. 문7_1
class_Q7 <- rawdata1 %>%
  select(A7_1)
t1_4 <- table(class_Q7)
prop.table(t1_4)

# class. 문8_1
class_Q8 <- rawdata1 %>%
  select(A8_1)
t1_5 <- table(class_Q8)
prop.table(t1_5)

# class. 문9
class_Q9 <- rawdata1 %>%
  select(A9)
mean(as.matrix(class_Q9))
sd(as.matrix(class_Q9))

# class. 문12
class_Q12 <- rawdata1 %>%
  select(A12)
mean(as.matrix(class_Q12))
sd(as.matrix(class_Q12))

# class. 문16
class_Q161 <- rawdata1 %>%
  select(A16_1)
mean(as.matrix(class_Q161))
sd(as.matrix(class_Q161))

class_Q162 <- rawdata1 %>%
  select(A16_2)
mean(as.matrix(class_Q162))
sd(as.matrix(class_Q162))

class_Q163 <- rawdata1 %>%
  select(A16_3)
mean(as.matrix(class_Q163))
sd(as.matrix(class_Q163))

class_Q164 <- rawdata1 %>%
  select(A16_4)
mean(as.matrix(class_Q164))
sd(as.matrix(class_Q164))

class_Q165 <- rawdata1 %>%
  select(A16_5)
mean(as.matrix(class_Q165))
sd(as.matrix(class_Q165))

class_Q166 <- rawdata1 %>%
  select(A16_6)
mean(as.matrix(class_Q166))
sd(as.matrix(class_Q166))

class_Q171 <- rawdata1 %>%
  select(A17_1)
mean(as.matrix(class_Q171))
sd(as.matrix(class_Q171))

class_Q172 <- rawdata1 %>%
  select(A17_2)
mean(as.matrix(class_Q172))
sd(as.matrix(class_Q172))

class_Q173 <- rawdata1 %>%
  select(A17_3)
mean(as.matrix(class_Q173))
sd(as.matrix(class_Q173))

class_Q174 <- rawdata1 %>%
  select(A17_4)
mean(as.matrix(class_Q174))
sd(as.matrix(class_Q174))

class_Q175 <- rawdata1 %>%
  select(A17_5)
mean(as.matrix(class_Q175))
sd(as.matrix(class_Q175))

class_Q176 <- rawdata1 %>%
  select(A17_6)
mean(as.matrix(class_Q176))
sd(as.matrix(class_Q176))

class_Q177 <- rawdata1 %>%
  select(A17_7)
mean(as.matrix(class_Q177))
sd(as.matrix(class_Q177))

class_Q178 <- rawdata1 %>%
  select(A17_8)
mean(as.matrix(class_Q178))
sd(as.matrix(class_Q178))

class_Q179 <- rawdata1 %>%
  select(A17_9)
mean(as.matrix(class_Q179))
sd(as.matrix(class_Q179))

class_Q1710 <- rawdata1 %>%
  select(A17_10)
mean(as.matrix(class_Q1710))
sd(as.matrix(class_Q1710))

# class. 연령대 구분
class_age <- rawdata1 %>%
  select(SQ2_2)
t_2 <- table(class_age)
prop.table(t_2)

item_label = c('01번 문항', '02번 문항', '03번 문항', '04번 문항', '05번 문항', '06번 문항',
               '07번 문항', '08번 문항', '09번 문항', '10번 문항')

item_resp_prob = lca$probs
item_resp_prob[[1]] %>% as_tibble()

table = list()
for (i in 1:10) {
  table=bind_rows(table,
        item_resp_prob[[i]] %>% as_tibble() %>% 
          mutate(item=i, class=1:4)) %>% 
    select(-'Pr(1)')
}
table = table %>% 
  mutate(item = rep(item_label, each=4))
table
table %>% 
  spread(key=class, value='Pr(2)')

table %>% ggplot()+
  geom_bar(aes(x=item, y=`Pr(2)`), stat='identity')+
  labs(x="문항", y="평균이상 응답")+
  coord_flip()+
  theme_bw()+theme(legend.position = "top")+
  facet_wrap(~class)

windows()

class_name = c("4번 계층(19%)","3번 계층(28%)","2번 계층(13%)","1번 계층(40%)")
visual = table %>% 
  mutate(class_name = rep(class_name,10),
    item_cluster = fct_recode(item, "문제인식"="01번 문항",
                              "문제인식"="02번 문항",
                              "문제인식"="03번 문항",
                              "주민참여"="04번 문항",
                              "주민참여"="05번 문항",
                              "주민참여"="06번 문항",
                              "건강위험"="07번 문항",
                              "건강위험"="08번 문항",
                              "경제적위험"="09번 문항",
                              "경제적위험"="10번 문항"),
    item = fct_reorder(item, as.numeric(factor(item_cluster))))

visual %>% ggplot()+
  geom_bar(aes(x=item, y=`Pr(2)`, fill=item_cluster), stat = "identity")+
  labs(x="설문문항", y="그룹평균 이상 응답확률", fill="설문 영역")+
  coord_flip()+
  theme_bw()+theme(legend.position = "top")+
  facet_wrap(~class_name)
lca$P

view(rawdata1)


A13_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A13)

A13_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A13)

A13_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A13)

A13_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A13)

A13_class1$class <- 4
A13_class2$class <- 3
A13_class3$class <- 2
A13_class4$class <- 1

A13_class <- rbind(A13_class1,A13_class2,A13_class3,A13_class4)

A13_class %>% 
aov(class ~ A13, data=.) %>% 
  summary()
