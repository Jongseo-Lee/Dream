library(poLCA)
library(dplyr)
library(haven)
library(tidyverse)

rawdata <- read_spss("Rawdata.sav")

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

# 3번 계층
class2 <- rawdata1 %>% 
  filter(`lca$predclass`==2)

# 2번 계층
class3 <- rawdata1 %>% 
  filter(`lca$predclass`==3)

# 1번 계층
class4 <- rawdata1 %>% 
  filter(`lca$predclass`==4)

# class4. A6_1
class1_A6_1 <- class1 %>%
  select(A6_1)
table(class1_A6_1)

# class3. A6_1
class2_A6_1 <- class2 %>%
  select(A6_1)
table(class2_A6_1)

# class2. A6_1
class3_A6_1 <- class3 %>%
  select(A6_1)
table(class3_A6_1)

# class1. A6_1
class4_A6_1 <- class4 %>%
  select(A6_1)
table(class4_A6_1)

c1 = c(93,84,43,30,18,12,15)
c2 = c(21,40,8,9,1,4,1)
c3 = c(57,98,7,14,3,11,5)
c4 = c(34,98,2,10,2,3,1)

data1 <- data.frame(c1, c2, c3, c4)
data1 
chisq.test(data1)
fisher.test(data1, simulate.p.value = TRUE, B=2e6)

# class4. A6_2
class1_A6_2 <- class1 %>%
  select(A6_2)
table(class1_A6_2)

# class3. A6_2
class2_A6_2 <- class2 %>%
  select(A6_2)
table(class2_A6_2)

# class2. A6_2
class3_A6_2 <- class3 %>%
  select(A6_2)
table(class3_A6_2)

# class1. A6_2
class4_A6_2 <- class4 %>%
  select(A6_2)
table(class4_A6_2)

c11 = c(63,67,33,50,26,36,20)
c12 = c(29,18,8,15,3,4,7)
c13 = c(51,43,18,34,7,22,20)
c14 = c(57,35,6,21,5,7,19)

data2 <- data.frame(c11, c12, c13, c14)
data2 
chisq.test(data2)
fisher.test(data2, simulate.p.value = TRUE, B=2e6)

# class4. A7_1
class1_A7_1 <- class1 %>%
  select(A7_1)
table(class1_A7_1)

# class3. A7_1
class2_A7_1 <- class2 %>%
  select(A7_1)
table(class2_A7_1)

# class2. A7_1
class3_A7_1 <- class3 %>%
  select(A7_1)
table(class3_A7_1)

# class1. A7_1
class4_A7_1 <- class4 %>%
  select(A7_1)
table(class4_A7_1)

c21 = c(29,92,69,41,29,33,2)
c22 = c(1,28,14,15,10,16,0)
c23 = c(9,56,32,39,25,31,3)
c24 = c(7,29,14,59,12,24,5)

data3 <- data.frame(c21, c22, c23, c24)
data3 
chisq.test(data3)
fisher.test(data3, simulate.p.value = TRUE, B=2e6)
