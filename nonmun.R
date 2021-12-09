library(poLCA)
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")


rawdata$DQ8_1 <- ifelse(rawdata$DQ8_1 == 1, "100 만원 이하", # 가구소득별
                            ifelse(rawdata$DQ8_1 %in% c(2,3), "100~200만원",
                                   ifelse(rawdata$DQ8_1 %in% c(4,5), "200~300만원", 
                                          ifelse(rawdata$DQ8_1 %in% c(6), "300~400만원",
                                                 ifelse(rawdata$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))


rawdata$DQ8_2 <- ifelse(rawdata$DQ8_2 == 1, "100 만원 이하", # 가구소득별
                        ifelse(rawdata$DQ8_2 %in% c(2,3), "100~200만원",
                               ifelse(rawdata$DQ8_2 %in% c(4,5), "200~300만원", 
                                      ifelse(rawdata$DQ8_2 %in% c(6), "300~400만원",
                                             ifelse(rawdata$DQ8_2 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))


rawdata$DQ4 <- ifelse(rawdata$DQ4 == 0, "무학", # 학력별
                       ifelse(rawdata$DQ4 %in% c(1,2,3,4,5,6), "초등학교 졸업",
                              ifelse(rawdata$DQ4 %in% c(7,8,9), "중학교 졸업", 
                                     ifelse(rawdata$DQ4 %in% c(10,11,12), "고등학교 졸업",
                                            ifelse(rawdata$DQ4 %in% c(13,14,15,16), "대학교 졸업", "대학원 졸업")))))


A1 <- rawdata$SQ1
A1 <- as.matrix(A1)
table(A1)

A2 <- rawdata$SQ2_2
A2 <- as.matrix(A2)
table(A2)

A3 <- rawdata$DQ8_1
A3 <- as.matrix(A3)
table(A3)

A4 <- rawdata$DQ8_2
A4 <- as.matrix(A4)
table(A4)

A5 <- rawdata$DQ4
A5 <- as.matrix(A5)
table(A5)

A6 <- rawdata$A1
A6 <- as.matrix(A6)
table(A6)


