library(dplyr)
library(haven)
options(digits = 5) # 소수점 확대


rawdata <- read_spss("Rawdata.sav")
table(is.na(rawdata))
View(rawdata)


raw_citizen <- rawdata %>%  # 시흥시민 364명 필터링
  filter(Gubun_Area == 1)
View(raw_citizen)


table(raw_citizen$DQ8_1) # 가구소득 재그룹화
raw_citizen$DQ8_1 <- ifelse(raw_citizen$DQ8_1 == 1, "100만원 이하",
                        ifelse(raw_citizen$DQ8_1 %in% c(2,3), "100~200만원",
                            ifelse(raw_citizen$DQ8_1 %in% c(4,5), "200~300만원", 
                                ifelse(raw_citizen$DQ8_1 %in% c(6), "300~400만원",
                                    ifelse(raw_citizen$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))

table(raw_citizen$DQ8_1) # 가구소득 빈도 확인


sd(raw_citizen$A9) # A9. 전체 표준편차
mean(raw_citizen$A9) # A9. 전체 5점 평균
25*(mean(raw_citizen$A9-1)) # A9. 전체 100점 평균 환산

A9_data1 <- raw_citizen %>% # A9. 성별 구분
  group_by(A9) %>% 
  select(SQ1)
table(A9_data1)
t_A9_1 <- t(table(A9_data1)) # 행렬변환
t_A9_1
A9_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A9 = sd(A9)) %>% as.data.frame()
A9_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A9 = mean(A9)) %>% as.data.frame()
A9_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean5_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


A9_data2 <- raw_citizen %>% # A9. 연령대 구분
  group_by(A9) %>% 
  select(SQ2_2)
table(A9_data2)
t_A9_2 <- t(table(A9_data2)) # 행렬변환
t_A9_2
A9_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A9 = sd(A9)) %>% as.data.frame()
A9_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A9 = mean(A9)) %>% as.data.frame()
A9_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean5_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


A9_data3 <- raw_citizen %>% # A9. 소득 구분
  group_by(A9) %>% 
  select(DQ8_1)
table(A9_data3)
t_A9_3 <- t(table(A9_data3)) # 행렬변환
t_A9_3
A9_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A9 = sd(A9)) %>% as.data.frame()
A9_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A9 = mean(A9)) %>% as.data.frame()
A9_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean5_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


A9_1_data1 <- raw_citizen %>% # A9-1. 성별 구분
  group_by(A9_1) %>% 
  select(SQ1)
table(A9_1_data1)
t_A9_1_1 <- t(table(A9_1_data1)) # 행렬변환
t_A9_1_1
raw_citizen$A9_1 # 사례 수가 다른 것을 확인하고, NA 값 확인(보통이상 응답자 대상 설문 항목으로 NA값이 존재하는 것이 맞음)


A9_1_data2 <- raw_citizen %>% # A9-1. 연령별 구분
  group_by(A9_1) %>% 
  select(SQ2_2)
table(A9_1_data2)
t_A9_1_2 <- t(table(A9_1_data2)) # 행렬변환
t_A9_1_2


A9_1_data3 <- raw_citizen %>% # A9-1. 소득별 구분
  group_by(A9_1) %>% 
  select(DQ8_1)
table(A9_1_data3)
t_A9_1_3 <- t(table(A9_1_data3)) # 행렬변환
t_A9_1_3
