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


# A13. ANOVA

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
