library(dplyr)
library(haven)
options(digits = 5) # 소수점 확대


rawdata <- read_spss("Rawdata.sav")
raw_citizen <- rawdata %>%  # 시흥시민 364명 필터링
  filter(Gubun_Area == 1)


table(raw_citizen$DQ8_1) # 가구소득 재그룹화
raw_citizen$DQ8_1 <- ifelse(raw_citizen$DQ8_1 == 1, "100만원 이하",
                            ifelse(raw_citizen$DQ8_1 %in% c(2,3), "100~200만원",
                                   ifelse(raw_citizen$DQ8_1 %in% c(4,5), "200~300만원", 
                                          ifelse(raw_citizen$DQ8_1 %in% c(6), "300~400만원",
                                                 ifelse(raw_citizen$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))


sd(raw_citizen$A16_1) # A16-1. 전체 표준편차
mean(raw_citizen$A16_1) # A16-1. 전체 5점 평균
25*(mean(raw_citizen$A16_1-1)) # A16-1. 전체 100점 평균 환산


A16_1_data1 <- raw_citizen %>% # A16-1. 성별 구분
  group_by(A16_1) %>% 
  select(SQ1)
t(table(A16_1_data1)) # 행렬변환

A16_1_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_1 = sd(A16_1)) %>% as.data.frame()
A16_1_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_1 = mean(A16_1)) %>% as.data.frame()
A16_1_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_1 = 25*(mean(A16_1)-1)) %>% as.data.frame()


A16_1_data2 <- raw_citizen %>% # A16-1. 연령대 구분
  group_by(A16_1) %>% 
  select(SQ2_2)
t(table(A16_1_data2)) # 행렬변환

A16_1_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_1 = sd(A16_1)) %>% as.data.frame()
A16_1_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_1 = mean(A16_1)) %>% as.data.frame()
A16_1_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_1 = 25*(mean(A16_1)-1)) %>% as.data.frame()


A16_1_data3 <- raw_citizen %>% # A16-1. 소득 구분
  group_by(A16_1) %>% 
  select(DQ8_1)
t(table(A16_1_data3)) # 행렬변환

A16_1_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_1 = sd(A16_1)) %>% as.data.frame()
A16_1_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_1 = mean(A16_1)) %>% as.data.frame()
A16_1_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_1 = 25*(mean(A16_1)-1)) %>% as.data.frame()


sd(raw_citizen$A16_2) # A16-2. 전체 표준편차
mean(raw_citizen$A16_2) # A16-2. 전체 5점 평균
25*(mean(raw_citizen$A16_2-1)) # A16-2. 전체 100점 평균 환산


A16_2_data1 <- raw_citizen %>% # A16-2. 성별 구분
  group_by(A16_2) %>% 
  select(SQ1)
t(table(A16_2_data1)) # 행렬변환

A16_2_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_2 = sd(A16_2)) %>% as.data.frame()
A16_2_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_2 = mean(A16_2)) %>% as.data.frame()
A16_2_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_2 = 25*(mean(A16_2)-1)) %>% as.data.frame()


A16_2_data2 <- raw_citizen %>% # A16-2. 연령대 구분
  group_by(A16_2) %>% 
  select(SQ2_2)
t(table(A16_2_data2)) # 행렬변환


A16_2_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_2 = sd(A16_2)) %>% as.data.frame()
A16_2_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_2 = mean(A16_2)) %>% as.data.frame()
A16_2_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_2 = 25*(mean(A16_2)-1)) %>% as.data.frame()


A16_2_data3 <- raw_citizen %>% # A16-2. 소득 구분
  group_by(A16_2) %>% 
  select(DQ8_1)
t(table(A16_2_data3)) # 행렬변환


A16_2_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_2 = sd(A16_2)) %>% as.data.frame()
A16_2_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_2 = mean(A16_2)) %>% as.data.frame()
A16_2_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_2 = 25*(mean(A16_2)-1)) %>% as.data.frame()
