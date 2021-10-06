library(dplyr)
library(haven)
library(ggplot2)
library(scales)
library(RColorBrewer)

options(digits = 5) # 소수점 확대

rawdata <- read_spss("Rawdata.sav")
table(is.na(rawdata))
raw_citizen <- rawdata %>% # 시흥시민 364명 필터링
  filter(Gubun_Area == 1) 

# 데이터 전처리
raw_citizen$SQ1 <- ifelse(raw_citizen$SQ1 == 1, "남성", "여성") # 성별

raw_citizen$SQ2_2 <- ifelse(raw_citizen$SQ2_2 == 2, "20대",  # 연령대별
                            ifelse(raw_citizen$SQ2_2 == 3, "30대",
                                   ifelse(raw_citizen$SQ2_2 == 4, "40대",
                                          ifelse(raw_citizen$SQ2_2 == 5, "50대", "60대"))))

raw_citizen$DQ8_1 <- ifelse(raw_citizen$DQ8_1 == 1, "100 만원 이하", # 가구소득별
                            ifelse(raw_citizen$DQ8_1 %in% c(2,3), "100~200만원",
                                   ifelse(raw_citizen$DQ8_1 %in% c(4,5), "200~300만원", 
                                          ifelse(raw_citizen$DQ8_1 %in% c(6), "300~400만원",
                                                 ifelse(raw_citizen$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))

raw_citizen$A1 <- ifelse(raw_citizen$A1 == 1, "네", "아니오") # 송전탑 가시거리 거주별


sd(raw_citizen$A17_1) # A17-1. 전체 표준편차
mean(raw_citizen$A17_1) # A17-1. 전체 5점 평균
25*(mean(raw_citizen$A17_1-1)) # A17-1. 전체 100점 평균 환산


A17_1_data1 <- raw_citizen %>% # A17-1. 성별 구분
  group_by(A17_1) %>% 
  select(SQ1)
t(table(A17_1_data1)) # 행렬변환

A17_1_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_1 = sd(A17_1)) %>% as.data.frame()
A17_1_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_1 = mean(A17_1)) %>% as.data.frame()
A17_1_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_1 = 25*(mean(A17_1)-1)) %>% as.data.frame()


A17_1_data2 <- raw_citizen %>% # A17-1. 연령대 구분
  group_by(A17_1) %>% 
  select(SQ2_2)
t(table(A17_1_data2)) # 행렬변환

A17_1_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_1 = sd(A17_1)) %>% as.data.frame()
A17_1_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_1 = mean(A17_1)) %>% as.data.frame()
A17_1_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_1 = 25*(mean(A17_1)-1)) %>% as.data.frame()


A17_1_data3 <- raw_citizen %>% # A17-1. 소득 구분
  group_by(A17_1) %>% 
  select(DQ8_1)
t(table(A17_1_data3)) # 행렬변환

A17_1_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_1 = sd(A17_1)) %>% as.data.frame()
A17_1_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_1 = mean(A17_1)) %>% as.data.frame()
A17_1_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_1 = 25*(mean(A17_1)-1)) %>% as.data.frame()


A17_1_data4 <- raw_citizen %>% # A17-1. 송전탑 가시거리 거주 구분
  group_by(A17_1) %>% 
  select(A1)
t(table(A17_1_data4)) # 행렬변환

A17_1_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_1 = sd(A17_1)) %>% as.data.frame()
A17_1_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_1 = mean(A17_1)) %>% as.data.frame()
A17_1_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_1 = 25*(mean(A17_1)-1)) %>% as.data.frame()


sd(raw_citizen$A17_2) # A17-2. 전체 표준편차
mean(raw_citizen$A17_2) # A17-2. 전체 5점 평균
25*(mean(raw_citizen$A17_2-1)) # A17-2. 전체 100점 평균 환산


A17_2_data1 <- raw_citizen %>% # A17-2. 성별 구분
  group_by(A17_2) %>% 
  select(SQ1)
t(table(A17_2_data1)) # 행렬변환

A17_2_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_2 = sd(A17_2)) %>% as.data.frame()
A17_2_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_2 = mean(A17_2)) %>% as.data.frame()
A17_2_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_2 = 25*(mean(A17_2)-1)) %>% as.data.frame()


A17_2_data2 <- raw_citizen %>% # A17-2. 연령대 구분
  group_by(A17_2) %>% 
  select(SQ2_2)
t(table(A17_2_data2)) # 행렬변환

A17_2_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_2 = sd(A17_2)) %>% as.data.frame()
A17_2_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_2 = mean(A17_2)) %>% as.data.frame()
A17_2_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_2 = 25*(mean(A17_2)-1)) %>% as.data.frame()


A17_2_data3 <- raw_citizen %>% # A17-2. 소득 구분
  group_by(A17_2) %>% 
  select(DQ8_1)
t(table(A17_2_data3)) # 행렬변환

A17_2_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_2 = sd(A17_2)) %>% as.data.frame()
A17_2_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_2 = mean(A17_2)) %>% as.data.frame()
A17_2_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_2 = 25*(mean(A17_2)-1)) %>% as.data.frame()


A17_2_data4 <- raw_citizen %>% # A17-2. 송전탑 가시거리 거주 구분
  group_by(A17_2) %>% 
  select(A1)
t(table(A17_2_data4)) # 행렬변환

A17_2_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_2 = sd(A17_2)) %>% as.data.frame()
A17_2_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_2 = mean(A17_2)) %>% as.data.frame()
A17_2_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_2 = 25*(mean(A17_2)-1)) %>% as.data.frame()


sd(raw_citizen$A17_3) # A17-3. 전체 표준편차
mean(raw_citizen$A17_3) # A17-3. 전체 5점 평균
25*(mean(raw_citizen$A17_3-1)) # A17-3. 전체 100점 평균 환산


A17_3_data1 <- raw_citizen %>% # A17-3. 성별 구분
  group_by(A17_3) %>% 
  select(SQ1)
t(table(A17_3_data1)) # 행렬변환

A17_3_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_3 = sd(A17_3)) %>% as.data.frame()
A17_3_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_3 = mean(A17_3)) %>% as.data.frame()
A17_3_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_3 = 25*(mean(A17_3)-1)) %>% as.data.frame()


A17_3_data2 <- raw_citizen %>% # A17-3. 연령대 구분
  group_by(A17_3) %>% 
  select(SQ2_2)
t(table(A17_3_data2)) # 행렬변환

A17_3_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_3 = sd(A17_3)) %>% as.data.frame()
A17_3_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_3 = mean(A17_3)) %>% as.data.frame()
A17_3_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_3 = 25*(mean(A17_3)-1)) %>% as.data.frame()


A17_3_data3 <- raw_citizen %>% # A17-3. 소득 구분
  group_by(A17_3) %>% 
  select(DQ8_1)
t(table(A17_3_data3)) # 행렬변환

A17_3_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_3 = sd(A17_3)) %>% as.data.frame()
A17_3_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_3 = mean(A17_3)) %>% as.data.frame()
A17_3_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_3 = 25*(mean(A17_3)-1)) %>% as.data.frame()


A17_3_data4 <- raw_citizen %>% # A17-3. 송전탑 가시거리 거주 구분
  group_by(A17_3) %>% 
  select(A1)
t(table(A17_3_data4)) # 행렬변환

A17_3_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_3 = sd(A17_3)) %>% as.data.frame()
A17_3_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_3 = mean(A17_3)) %>% as.data.frame()
A17_3_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_3 = 25*(mean(A17_3)-1)) %>% as.data.frame()


sd(raw_citizen$A17_4) # A17-4. 전체 표준편차
mean(raw_citizen$A17_4) # A17-4. 전체 5점 평균
25*(mean(raw_citizen$A17_4-1)) # A17-4. 전체 100점 평균 환산


A17_4_data1 <- raw_citizen %>% # A17-4. 성별 구분
  group_by(A17_4) %>% 
  select(SQ1)
t(table(A17_4_data1)) # 행렬변환

A17_4_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_4 = sd(A17_4)) %>% as.data.frame()
A17_4_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_4 = mean(A17_4)) %>% as.data.frame()
A17_4_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_4 = 25*(mean(A17_4)-1)) %>% as.data.frame()


A17_4_data2 <- raw_citizen %>% # A17-4. 연령대 구분
  group_by(A17_4) %>% 
  select(SQ2_2)
t(table(A17_4_data2)) # 행렬변환

A17_4_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_4 = sd(A17_4)) %>% as.data.frame()
A17_4_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_4 = mean(A17_4)) %>% as.data.frame()
A17_4_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_4 = 25*(mean(A17_4)-1)) %>% as.data.frame()


A17_4_data3 <- raw_citizen %>% # A17-4. 소득 구분
  group_by(A17_4) %>% 
  select(DQ8_1)
t(table(A17_4_data3)) # 행렬변환

A17_4_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_4 = sd(A17_4)) %>% as.data.frame()
A17_4_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_4 = mean(A17_4)) %>% as.data.frame()
A17_4_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_4 = 25*(mean(A17_4)-1)) %>% as.data.frame()


A17_4_data4 <- raw_citizen %>% # A17-4. 송전탑 가시거리 거주 구분
  group_by(A17_4) %>% 
  select(A1)
t(table(A17_4_data4)) # 행렬변환

A17_4_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_4 = sd(A17_4)) %>% as.data.frame()
A17_4_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_4 = mean(A17_4)) %>% as.data.frame()
A17_4_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_4 = 25*(mean(A17_4)-1)) %>% as.data.frame()


sd(raw_citizen$A17_5) # A17-5. 전체 표준편차
mean(raw_citizen$A17_5) # A17-5. 전체 5점 평균
25*(mean(raw_citizen$A17_5-1)) # A17-5. 전체 100점 평균 환산


A17_5_data1 <- raw_citizen %>% # A17-5. 성별 구분
  group_by(A17_5) %>% 
  select(SQ1)
t(table(A17_5_data1)) # 행렬변환

A17_5_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_5 = sd(A17_5)) %>% as.data.frame()
A17_5_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_5 = mean(A17_5)) %>% as.data.frame()
A17_5_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_5 = 25*(mean(A17_5)-1)) %>% as.data.frame()


A17_5_data2 <- raw_citizen %>% # A17-5. 연령대 구분
  group_by(A17_5) %>% 
  select(SQ2_2)
t(table(A17_5_data2)) # 행렬변환

A17_5_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_5 = sd(A17_5)) %>% as.data.frame()
A17_5_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_5 = mean(A17_5)) %>% as.data.frame()
A17_5_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_5 = 25*(mean(A17_5)-1)) %>% as.data.frame()


A17_5_data3 <- raw_citizen %>% # A17-5. 소득 구분
  group_by(A17_5) %>% 
  select(DQ8_1)
t(table(A17_5_data3)) # 행렬변환

A17_5_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_5 = sd(A17_5)) %>% as.data.frame()
A17_5_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_5 = mean(A17_5)) %>% as.data.frame()
A17_5_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_5 = 25*(mean(A17_5)-1)) %>% as.data.frame()


A17_5_data4 <- raw_citizen %>% # A17-5. 송전탑 가시거리 거주 구분
  group_by(A17_5) %>% 
  select(A1)
t(table(A17_5_data4)) # 행렬변환

A17_5_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_5 = sd(A17_5)) %>% as.data.frame()
A17_5_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_5 = mean(A17_5)) %>% as.data.frame()
A17_5_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_5 = 25*(mean(A17_5)-1)) %>% as.data.frame()


sd(raw_citizen$A17_6) # A17-6. 전체 표준편차
mean(raw_citizen$A17_6) # A17-6. 전체 5점 평균
25*(mean(raw_citizen$A17_6-1)) # A17-6. 전체 100점 평균 환산


A17_6_data1 <- raw_citizen %>% # A17-6. 성별 구분
  group_by(A17_6) %>% 
  select(SQ1)
t(table(A17_6_data1)) # 행렬변환

A17_6_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_6 = sd(A17_6)) %>% as.data.frame()
A17_6_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_6 = mean(A17_6)) %>% as.data.frame()
A17_6_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_6 = 25*(mean(A17_6)-1)) %>% as.data.frame()


A17_6_data2 <- raw_citizen %>% # A17-6. 연령대 구분
  group_by(A17_6) %>% 
  select(SQ2_2)
t(table(A17_6_data2)) # 행렬변환

A17_6_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_6 = sd(A17_6)) %>% as.data.frame()
A17_6_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_6 = mean(A17_6)) %>% as.data.frame()
A17_6_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_6 = 25*(mean(A17_6)-1)) %>% as.data.frame()


A17_6_data3 <- raw_citizen %>% # A17-6. 소득 구분
  group_by(A17_6) %>% 
  select(DQ8_1)
t(table(A17_6_data3)) # 행렬변환

A17_6_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_6 = sd(A17_6)) %>% as.data.frame()
A17_6_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_6 = mean(A17_6)) %>% as.data.frame()
A17_6_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_6 = 25*(mean(A17_6)-1)) %>% as.data.frame()


A17_6_data4 <- raw_citizen %>% # A17-6. 송전탑 가시거리 거주 구분
  group_by(A17_6) %>% 
  select(A1)
t(table(A17_6_data4)) # 행렬변환

A17_6_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_6 = sd(A17_6)) %>% as.data.frame()
A17_6_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_6 = mean(A17_6)) %>% as.data.frame()
A17_6_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_6 = 25*(mean(A17_6)-1)) %>% as.data.frame()


sd(raw_citizen$A17_7) # A17-7. 전체 표준편차
mean(raw_citizen$A17_7) # A17-7. 전체 5점 평균
25*(mean(raw_citizen$A17_7-1)) # A17-7. 전체 100점 평균 환산


A17_7_data1 <- raw_citizen %>% # A17-7. 성별 구분
  group_by(A17_7) %>% 
  select(SQ1)
t(table(A17_7_data1)) # 행렬변환

A17_7_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_7 = sd(A17_7)) %>% as.data.frame()
A17_7_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_7 = mean(A17_7)) %>% as.data.frame()
A17_7_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_7 = 25*(mean(A17_7)-1)) %>% as.data.frame()


A17_7_data2 <- raw_citizen %>% # A17-7. 연령대 구분
  group_by(A17_7) %>% 
  select(SQ2_2)
t(table(A17_7_data2)) # 행렬변환

A17_7_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_7 = sd(A17_7)) %>% as.data.frame()
A17_7_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_7 = mean(A17_7)) %>% as.data.frame()
A17_7_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_7 = 25*(mean(A17_7)-1)) %>% as.data.frame()


A17_7_data3 <- raw_citizen %>% # A17-7. 소득 구분
  group_by(A17_7) %>% 
  select(DQ8_1)
t(table(A17_7_data3)) # 행렬변환

A17_7_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_7 = sd(A17_7)) %>% as.data.frame()
A17_7_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_7 = mean(A17_7)) %>% as.data.frame()
A17_7_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_7 = 25*(mean(A17_7)-1)) %>% as.data.frame()


A17_7_data4 <- raw_citizen %>% # A17-7. 송전탑 가시거리 거주 구분
  group_by(A17_7) %>% 
  select(A1)
t(table(A17_7_data4)) # 행렬변환

A17_7_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_7 = sd(A17_7)) %>% as.data.frame()
A17_7_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_7 = mean(A17_7)) %>% as.data.frame()
A17_7_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_7 = 25*(mean(A17_7)-1)) %>% as.data.frame()


sd(raw_citizen$A17_8) # A17-8. 전체 표준편차
mean(raw_citizen$A17_8) # A17-8. 전체 5점 평균
25*(mean(raw_citizen$A17_8-1)) # A17-8. 전체 100점 평균 환산


A17_8_data1 <- raw_citizen %>% # A17-8. 성별 구분
  group_by(A17_8) %>% 
  select(SQ1)
t(table(A17_8_data1)) # 행렬변환

A17_8_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_8 = sd(A17_8)) %>% as.data.frame()
A17_8_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_8 = mean(A17_8)) %>% as.data.frame()
A17_8_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_8 = 25*(mean(A17_8)-1)) %>% as.data.frame()


A17_8_data2 <- raw_citizen %>% # A17-8. 연령대 구분
  group_by(A17_8) %>% 
  select(SQ2_2)
t(table(A17_8_data2)) # 행렬변환

A17_8_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_8 = sd(A17_8)) %>% as.data.frame()
A17_8_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_8 = mean(A17_8)) %>% as.data.frame()
A17_8_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_8 = 25*(mean(A17_8)-1)) %>% as.data.frame()


A17_8_data3 <- raw_citizen %>% # A17-8. 소득 구분
  group_by(A17_8) %>% 
  select(DQ8_1)
t(table(A17_8_data3)) # 행렬변환

A17_8_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_8 = sd(A17_8)) %>% as.data.frame()
A17_8_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_8 = mean(A17_8)) %>% as.data.frame()
A17_8_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_8 = 25*(mean(A17_8)-1)) %>% as.data.frame()


A17_8_data4 <- raw_citizen %>% # A17-8. 송전탑 가시거리 거주 구분
  group_by(A17_8) %>% 
  select(A1)
t(table(A17_8_data4)) # 행렬변환

A17_8_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_8 = sd(A17_8)) %>% as.data.frame()
A17_8_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_8 = mean(A17_8)) %>% as.data.frame()
A17_8_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_8 = 25*(mean(A17_8)-1)) %>% as.data.frame()


sd(raw_citizen$A17_9) # A17-9. 전체 표준편차
mean(raw_citizen$A17_9) # A17-9. 전체 5점 평균
25*(mean(raw_citizen$A17_9-1)) # A17-9. 전체 100점 평균 환산


A17_9_data1 <- raw_citizen %>% # A17-9. 성별 구분
  group_by(A17_9) %>% 
  select(SQ1)
t(table(A17_9_data1)) # 행렬변환

A17_9_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_9 = sd(A17_9)) %>% as.data.frame()
A17_9_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_9 = mean(A17_9)) %>% as.data.frame()
A17_9_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_9 = 25*(mean(A17_9)-1)) %>% as.data.frame()


A17_9_data2 <- raw_citizen %>% # A17-9. 연령대 구분
  group_by(A17_9) %>% 
  select(SQ2_2)
t(table(A17_9_data2)) # 행렬변환

A17_9_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_9 = sd(A17_9)) %>% as.data.frame()
A17_9_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_9 = mean(A17_9)) %>% as.data.frame()
A17_9_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_9 = 25*(mean(A17_9)-1)) %>% as.data.frame()


A17_9_data3 <- raw_citizen %>% # A17-9. 소득 구분
  group_by(A17_9) %>% 
  select(DQ8_1)
t(table(A17_9_data3)) # 행렬변환

A17_9_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_9 = sd(A17_9)) %>% as.data.frame()
A17_9_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_9 = mean(A17_9)) %>% as.data.frame()
A17_9_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_9 = 25*(mean(A17_9)-1)) %>% as.data.frame()


A17_9_data4 <- raw_citizen %>% # A17-9. 송전탑 가시거리 거주 구분
  group_by(A17_9) %>% 
  select(A1)
t(table(A17_9_data4)) # 행렬변환

A17_9_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_9 = sd(A17_9)) %>% as.data.frame()
A17_9_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_9 = mean(A17_9)) %>% as.data.frame()
A17_9_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_9 = 25*(mean(A17_9)-1)) %>% as.data.frame()


sd(raw_citizen$A17_10) # A17-10. 전체 표준편차
mean(raw_citizen$A17_10) # A17-10. 전체 5점 평균
25*(mean(raw_citizen$A17_10-1)) # A17-10. 전체 100점 평균 환산


A17_10_data1 <- raw_citizen %>% # A17-10. 성별 구분
  group_by(A17_10) %>% 
  select(SQ1)
t(table(A17_10_data1)) # 행렬변환

A17_10_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_10 = sd(A17_10)) %>% as.data.frame()
A17_10_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_10 = mean(A17_10)) %>% as.data.frame()
A17_10_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_10 = 25*(mean(A17_10)-1)) %>% as.data.frame()


A17_10_data2 <- raw_citizen %>% # A17-10. 연령대 구분
  group_by(A17_10) %>% 
  select(SQ2_2)
t(table(A17_10_data2)) # 행렬변환

A17_10_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_10 = sd(A17_10)) %>% as.data.frame()
A17_10_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_10 = mean(A17_10)) %>% as.data.frame()
A17_10_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_10 = 25*(mean(A17_10)-1)) %>% as.data.frame()


A17_10_data3 <- raw_citizen %>% # A17-10. 소득 구분
  group_by(A17_10) %>% 
  select(DQ8_1)
t(table(A17_10_data3)) # 행렬변환

A17_10_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_10 = sd(A17_10)) %>% as.data.frame()
A17_10_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_10 = mean(A17_10)) %>% as.data.frame()
A17_10_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_10 = 25*(mean(A17_10)-1)) %>% as.data.frame()


A17_10_data4 <- raw_citizen %>% # A17-10. 송전탑 가시거리 거주 구분
  group_by(A17_10) %>% 
  select(A1)
t(table(A17_10_data4)) # 행렬변환

A17_10_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_10 = sd(A17_10)) %>% as.data.frame()
A17_10_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_10 = mean(A17_10)) %>% as.data.frame()
A17_10_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_10 = 25*(mean(A17_10)-1)) %>% as.data.frame()


sd(raw_citizen$A17_11) # A17-11. 전체 표준편차
mean(raw_citizen$A17_11) # A17-11. 전체 5점 평균
25*(mean(raw_citizen$A17_11-1)) # A17-11. 전체 100점 평균 환산


A17_11_data1 <- raw_citizen %>% # A17-11. 성별 구분
  group_by(A17_11) %>% 
  select(SQ1)
t(table(A17_11_data1)) # 행렬변환

A17_11_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_11 = sd(A17_11)) %>% as.data.frame()
A17_11_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_11 = mean(A17_11)) %>% as.data.frame()
A17_11_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_11 = 25*(mean(A17_11)-1)) %>% as.data.frame()


A17_11_data2 <- raw_citizen %>% # A17-11. 연령대 구분
  group_by(A17_11) %>% 
  select(SQ2_2)
t(table(A17_11_data2)) # 행렬변환

A17_11_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_11 = sd(A17_11)) %>% as.data.frame()
A17_11_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_11 = mean(A17_11)) %>% as.data.frame()
A17_11_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_11 = 25*(mean(A17_11)-1)) %>% as.data.frame()


A17_11_data3 <- raw_citizen %>% # A17-11. 소득 구분
  group_by(A17_11) %>% 
  select(DQ8_1)
t(table(A17_11_data3)) # 행렬변환

A17_11_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_11 = sd(A17_11)) %>% as.data.frame()
A17_11_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_11 = mean(A17_11)) %>% as.data.frame()
A17_11_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_11 = 25*(mean(A17_11)-1)) %>% as.data.frame()


A17_11_data4 <- raw_citizen %>% # A17-11. 송전탑 가시거리 거주 구분
  group_by(A17_11) %>% 
  select(A1)
t(table(A17_11_data4)) # 행렬변환

A17_11_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_11 = sd(A17_11)) %>% as.data.frame()
A17_11_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_11 = mean(A17_11)) %>% as.data.frame()
A17_11_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_11 = 25*(mean(A17_11)-1)) %>% as.data.frame()


sd(raw_citizen$A17_12) # A17-12. 전체 표준편차
mean(raw_citizen$A17_12) # A17-12. 전체 5점 평균
25*(mean(raw_citizen$A17_12-1)) # A17-12. 전체 100점 평균 환산


A17_12_data1 <- raw_citizen %>% # A17-12. 성별 구분
  group_by(A17_12) %>% 
  select(SQ1)
t(table(A17_12_data1)) # 행렬변환

A17_12_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_12 = sd(A17_12)) %>% as.data.frame()
A17_12_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_12 = mean(A17_12)) %>% as.data.frame()
A17_12_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_12 = 25*(mean(A17_12)-1)) %>% as.data.frame()


A17_12_data2 <- raw_citizen %>% # A17-12. 연령대 구분
  group_by(A17_12) %>% 
  select(SQ2_2)
t(table(A17_12_data2)) # 행렬변환

A17_12_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_12 = sd(A17_12)) %>% as.data.frame()
A17_12_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_12 = mean(A17_12)) %>% as.data.frame()
A17_12_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_12 = 25*(mean(A17_12)-1)) %>% as.data.frame()


A17_12_data3 <- raw_citizen %>% # A17-12. 소득 구분
  group_by(A17_12) %>% 
  select(DQ8_1)
t(table(A17_12_data3)) # 행렬변환

A17_12_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_12 = sd(A17_12)) %>% as.data.frame()
A17_12_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_12 = mean(A17_12)) %>% as.data.frame()
A17_12_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_12 = 25*(mean(A17_12)-1)) %>% as.data.frame()


A17_12_data4 <- raw_citizen %>% # A17-12. 송전탑 가시거리 거주 구분
  group_by(A17_12) %>% 
  select(A1)
t(table(A17_12_data4)) # 행렬변환

A17_12_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_12 = sd(A17_12)) %>% as.data.frame()
A17_12_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_12 = mean(A17_12)) %>% as.data.frame()
A17_12_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_12 = 25*(mean(A17_12)-1)) %>% as.data.frame()


sd(raw_citizen$A17_13) # A17-13. 전체 표준편차
mean(raw_citizen$A17_13) # A17-13. 전체 5점 평균
25*(mean(raw_citizen$A17_13-1)) # A17-13. 전체 100점 평균 환산


A17_13_data1 <- raw_citizen %>% # A17-13. 성별 구분
  group_by(A17_13) %>% 
  select(SQ1)
t(table(A17_13_data1)) # 행렬변환

A17_13_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_13 = sd(A17_13)) %>% as.data.frame()
A17_13_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_13 = mean(A17_13)) %>% as.data.frame()
A17_13_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_13 = 25*(mean(A17_13)-1)) %>% as.data.frame()


A17_13_data2 <- raw_citizen %>% # A17-13. 연령대 구분
  group_by(A17_13) %>% 
  select(SQ2_2)
t(table(A17_13_data2)) # 행렬변환

A17_13_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_13 = sd(A17_13)) %>% as.data.frame()
A17_13_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_13 = mean(A17_13)) %>% as.data.frame()
A17_13_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_13 = 25*(mean(A17_13)-1)) %>% as.data.frame()


A17_13_data3 <- raw_citizen %>% # A17-13. 소득 구분
  group_by(A17_13) %>% 
  select(DQ8_1)
t(table(A17_13_data3)) # 행렬변환

A17_13_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_13 = sd(A17_13)) %>% as.data.frame()
A17_13_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_13 = mean(A17_13)) %>% as.data.frame()
A17_13_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_13 = 25*(mean(A17_13)-1)) %>% as.data.frame()


A17_13_data4 <- raw_citizen %>% # A17-13. 송전탑 가시거리 거주 구분
  group_by(A17_13) %>% 
  select(A1)
t(table(A17_13_data4)) # 행렬변환

A17_13_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_13 = sd(A17_13)) %>% as.data.frame()
A17_13_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_13 = mean(A17_13)) %>% as.data.frame()
A17_13_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_13 = 25*(mean(A17_13)-1)) %>% as.data.frame()


sd(raw_citizen$A17_14) # A17-14. 전체 표준편차
mean(raw_citizen$A17_14) # A17-14. 전체 5점 평균
25*(mean(raw_citizen$A17_14-1)) # A17-14. 전체 100점 평균 환산


A17_14_data1 <- raw_citizen %>% # A17-14. 성별 구분
  group_by(A17_14) %>% 
  select(SQ1)
t(table(A17_14_data1)) # 행렬변환

A17_14_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_14 = sd(A17_14)) %>% as.data.frame()
A17_14_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_14 = mean(A17_14)) %>% as.data.frame()
A17_14_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_14 = 25*(mean(A17_14)-1)) %>% as.data.frame()


A17_14_data2 <- raw_citizen %>% # A17-14. 연령대 구분
  group_by(A17_14) %>% 
  select(SQ2_2)
t(table(A17_14_data2)) # 행렬변환

A17_14_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_14 = sd(A17_14)) %>% as.data.frame()
A17_14_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_14 = mean(A17_14)) %>% as.data.frame()
A17_14_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_14 = 25*(mean(A17_14)-1)) %>% as.data.frame()


A17_14_data3 <- raw_citizen %>% # A17-14. 소득 구분
  group_by(A17_14) %>% 
  select(DQ8_1)
t(table(A17_14_data3)) # 행렬변환

A17_14_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_14 = sd(A17_14)) %>% as.data.frame()
A17_14_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_14 = mean(A17_14)) %>% as.data.frame()
A17_14_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_14 = 25*(mean(A17_14)-1)) %>% as.data.frame()


A17_14_data4 <- raw_citizen %>% # A17-14. 송전탑 가시거리 거주 구분
  group_by(A17_14) %>% 
  select(A1)
t(table(A17_14_data4)) # 행렬변환

A17_14_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_14 = sd(A17_14)) %>% as.data.frame()
A17_14_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_14 = mean(A17_14)) %>% as.data.frame()
A17_14_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_14 = 25*(mean(A17_14)-1)) %>% as.data.frame()


sd(raw_citizen$A17_15) # A17-15. 전체 표준편차
mean(raw_citizen$A17_15) # A17-15. 전체 5점 평균
25*(mean(raw_citizen$A17_15-1)) # A17-15. 전체 100점 평균 환산


A17_15_data1 <- raw_citizen %>% # A17-15. 성별 구분
  group_by(A17_15) %>% 
  select(SQ1)
t(table(A17_15_data1)) # 행렬변환

A17_15_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A17_15 = sd(A17_15)) %>% as.data.frame()
A17_15_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A17_15 = mean(A17_15)) %>% as.data.frame()
A17_15_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A17_15 = 25*(mean(A17_15)-1)) %>% as.data.frame()


A17_15_data2 <- raw_citizen %>% # A17-15. 연령대 구분
  group_by(A17_15) %>% 
  select(SQ2_2)
t(table(A17_15_data2)) # 행렬변환

A17_15_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A17_15 = sd(A17_15)) %>% as.data.frame()
A17_15_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A17_15 = mean(A17_15)) %>% as.data.frame()
A17_15_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A17_15 = 25*(mean(A17_15)-1)) %>% as.data.frame()


A17_15_data3 <- raw_citizen %>% # A17-15. 소득 구분
  group_by(A17_15) %>% 
  select(DQ8_1)
t(table(A17_15_data3)) # 행렬변환

A17_15_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A17_15 = sd(A17_15)) %>% as.data.frame()
A17_15_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A17_15 = mean(A17_15)) %>% as.data.frame()
A17_15_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A17_15 = 25*(mean(A17_15)-1)) %>% as.data.frame()


A17_15_data4 <- raw_citizen %>% # A17-15. 송전탑 가시거리 거주 구분
  group_by(A17_15) %>% 
  select(A1)
t(table(A17_15_data4)) # 행렬변환

A17_15_data4 %>% # 송전탑 가시거리 거주별 표준편차
  group_by(A1) %>% 
  summarise(sd_A17_15 = sd(A17_15)) %>% as.data.frame()
A17_15_data4 %>% # 송전탑 가시거리 거주별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A17_15 = mean(A17_15)) %>% as.data.frame()
A17_15_data4 %>% # 송전탑 가시거리 거주별 100점평균
  group_by(A1) %>%
  summarise(mean100_A17_15 = 25*(mean(A17_15)-1)) %>% as.data.frame()
