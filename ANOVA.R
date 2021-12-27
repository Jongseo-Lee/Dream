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

# A16-1. ANOVA

A161_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_1)

A161_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_1)

A161_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_1)

A161_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_1)

A161_class1$class <- 4
A161_class2$class <- 3
A161_class3$class <- 2
A161_class4$class <- 1

A161_class <- rbind(A161_class1,A161_class2,A161_class3,A161_class4)

A161_class %>% 
  aov(class ~ A16_1, data=.) %>% 
  summary()

# A16-2. ANOVA

A162_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_2)

A162_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_2)

A162_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_2)

A162_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_2)

A162_class1$class <- 4
A162_class2$class <- 3
A162_class3$class <- 2
A162_class4$class <- 1

A162_class <- rbind(A162_class1,A162_class2,A162_class3,A162_class4)

A162_class %>% 
  aov(class ~ A16_2, data=.) %>% 
  summary()

# A16-3. ANOVA

A163_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_3)

A163_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_3)

A163_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_3)

A163_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_3)

A163_class1$class <- 4
A163_class2$class <- 3
A163_class3$class <- 2
A163_class4$class <- 1

A163_class <- rbind(A163_class1,A163_class2,A163_class3,A163_class4)

A163_class %>% 
  aov(class ~ A16_3, data=.) %>% 
  summary()

# A16-4. ANOVA

A164_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_4)

A164_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_4)

A164_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_4)

A164_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_4)

A164_class1$class <- 4
A164_class2$class <- 3
A164_class3$class <- 2
A164_class4$class <- 1

A164_class <- rbind(A164_class1,A164_class2,A164_class3,A164_class4)

A164_class %>% 
  aov(class ~ A16_4, data=.) %>% 
  summary()

# A16-5. ANOVA

A165_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_5)

A165_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_5)

A165_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_5)

A165_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_5)

A165_class1$class <- 4
A165_class2$class <- 3
A165_class3$class <- 2
A165_class4$class <- 1

A165_class <- rbind(A165_class1,A165_class2,A165_class3,A165_class4)

A165_class %>% 
  aov(class ~ A16_5, data=.) %>% 
  summary()

# A16-6. ANOVA

A166_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A16_6)

A166_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A16_6)

A166_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A16_6)

A166_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A16_6)

A166_class1$class <- 4
A166_class2$class <- 3
A166_class3$class <- 2
A166_class4$class <- 1

A166_class <- rbind(A166_class1,A166_class2,A166_class3,A166_class4)

A166_class %>% 
  aov(class ~ A16_6, data=.) %>% 
  summary()

# A5. ANOVA

A5_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A5)

A5_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A5)

A5_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A5)

A5_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A5)

A5_class1$class <- 4
A5_class2$class <- 3
A5_class3$class <- 2
A5_class4$class <- 1

A5_class <- rbind(A5_class1,A5_class2,A5_class3,A5_class4)

A5_class %>% 
  aov(class ~ A5, data=.) %>% 
  summary()

# A6. ANOVA

A6_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A6_1)

A6_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A6_1)

A6_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A6_1)

A6_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A6_1)

A6_class1$class <- 4
A6_class2$class <- 3
A6_class3$class <- 2
A6_class4$class <- 1

A6_class <- rbind(A6_class1,A6_class2,A6_class3,A6_class4)
A6_class
A6_class %>% 
  aov(class ~ A6_1, data=.) %>% 
  summary()

# A7. ANOVA

A7_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A7_1)

A7_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A7_1)

A7_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A7_1)

A7_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A7_1)

A7_class1$class <- 4
A7_class2$class <- 3
A7_class3$class <- 2
A7_class4$class <- 1

A7_class <- rbind(A7_class1,A7_class2,A7_class3,A7_class4)

A7_class %>% 
  aov(class ~ A7_1, data=.) %>% 
  summary()

# SQ2-2. ANOVA

SQ_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(SQ2_2)

SQ_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(SQ2_2)

SQ_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(SQ2_2)

SQ_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(SQ2_2)

SQ_class1$class <- 4
SQ_class2$class <- 3
SQ_class3$class <- 2
SQ_class4$class <- 1

SQ_class <- rbind(SQ_class1,SQ_class2,SQ_class3,SQ_class4)

SQ_class %>% 
  aov(class ~ SQ2_2, data=.) %>% 
  summary()

# DQ8-1. ANOVA

DQ_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(DQ8_1)

DQ_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(DQ8_1)

DQ_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(DQ8_1)

DQ_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(DQ8_1)

DQ_class1$class <- 4
DQ_class2$class <- 3
DQ_class3$class <- 2
DQ_class4$class <- 1

DQ_class <- rbind(DQ_class1,DQ_class2,DQ_class3,DQ_class4)

DQ_class %>% 
  aov(class ~ DQ8_1, data=.) %>% 
  summary()

# DQ4. ANOVA

DQ4_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(DQ4)

DQ4_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(DQ4)

DQ4_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(DQ4)

DQ4_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(DQ4)

DQ4_class1$class <- 4
DQ4_class2$class <- 3
DQ4_class3$class <- 2
DQ4_class4$class <- 1

DQ4_class <- rbind(DQ4_class1,DQ4_class2,DQ4_class3,DQ4_class4)

DQ4_class %>% 
  aov(class ~ DQ4, data=.) %>% 
  summary()

# A14. ANOVA

A14_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A14)

A14_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A14)

A14_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A14)

A14_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A14)

A14_class1$class <- 4
A14_class2$class <- 3
A14_class3$class <- 2
A14_class4$class <- 1

A14_class <- rbind(A14_class1,A14_class2,A14_class3,A14_class4)

A14_class %>% 
  aov(class ~ A14, data=.) %>% 
  summary()

# A15. ANOVA

A15_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A15)

A15_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A15)

A15_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A15)

A15_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A15)

A15_class1$class <- 4
A15_class2$class <- 3
A15_class3$class <- 2
A15_class4$class <- 1

A15_class <- rbind(A15_class1,A15_class2,A15_class3,A15_class4)

A15_class %>% 
  aov(class ~ A15, data=.) %>% 
  summary()

# A1. ANOVA

A1_class1 <- rawdata1 %>% 
  filter(lca$predclass==4) %>% 
  select(A1)

A1_class2 <- rawdata1 %>% 
  filter(lca$predclass==3) %>% 
  select(A1)

A1_class3 <- rawdata1 %>% 
  filter(lca$predclass==2) %>% 
  select(A1)

A1_class4 <- rawdata1 %>% 
  filter(lca$predclass==1) %>% 
  select(A1)

A1_class1$class <- 4
A1_class2$class <- 3
A1_class3$class <- 2
A1_class4$class <- 1

A1_class <- rbind(A1_class1,A1_class2,A1_class3,A1_class4)

A1_class %>% 
  aov(class ~ A1, data=.) %>% 
  summary()
