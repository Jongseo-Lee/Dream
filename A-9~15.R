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
  summarise(mean100_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


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
  summarise(mean100_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


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
  summarise(mean100_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


A9_data4 <- raw_citizen %>% # A9. 송전탑 가시거리 거주 구분
  group_by(A9) %>% 
  select(A1)
t(table(A9_data4)) # 행렬변환
A9_data4 %>% # 송전탑 가시거리 거주 표준편차
  group_by(A1) %>% 
  summarise(sd_A9 = sd(A9)) %>% as.data.frame()
A9_data4 %>% # 송전탑 가시거리 거주 5점평균
  group_by(A1) %>% 
  summarise(mean5_A9 = mean(A9)) %>% as.data.frame()
A9_data4 %>% # 송전탑 가시거리 거주 100점평균
  group_by(A1) %>%
  summarise(mean100_A9 = 25*(mean(A9)-1)) %>% as.data.frame()


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


A9_1_data4 <- raw_citizen %>% # A9-1. 송전탑 가시거리 거주 구분
  group_by(A9_1) %>% 
  select(A1)
t(table(A9_1_data4)) # 행렬변환


A10_data1 <- raw_citizen %>% # A10. 성별 구분
  group_by(A10) %>% 
  select(SQ1)
table(A10_data1)
t_A10_1 <- t(table(A10_data1)) # 행렬변환
t_A10_1


A10_data2 <- raw_citizen %>% # A10. 연령별 구분
  group_by(A10) %>% 
  select(SQ2_2)
table(A10_data2)
t_A10_2 <- t(table(A10_data2)) # 행렬변환
t_A10_2


A10_data3 <- raw_citizen %>% # A10. 소득별 구분
  group_by(A10) %>% 
  select(DQ8_1)
table(A10_data3)
t_A10_3 <- t(table(A10_data3)) # 행렬변환
t_A10_3

A10_data4 <- raw_citizen %>% # A10. 송전탑 가시거리 거주 구분
  group_by(A10) %>% 
  select(A1)
t(table(A10_data4)) # 행렬변환

A11_data1 <- raw_citizen %>% # A11. 성별 구분
  group_by(A11) %>% 
  select(SQ1)
table(A11_data1)
t_A11_1 <- t(table(A11_data1)) # 행렬변환
t_A11_1


A11_data2 <- raw_citizen %>% # A11. 연령별 구분
  group_by(A11) %>% 
  select(SQ2_2)
table(A11_data2)
t_A11_2 <- t(table(A11_data2)) # 행렬변환
t_A11_2


A11_data3 <- raw_citizen %>% # A11. 소득별 구분
  group_by(A11) %>% 
  select(DQ8_1)
table(A11_data3)
t_A11_3 <- t(table(A11_data3)) # 행렬변환
t_A11_3


A11_data4 <- raw_citizen %>% # A11. 송전탑 가시거리 거주 구분
  group_by(A11) %>% 
  select(A1)
t(table(A11_data4)) # 행렬변환


sd(raw_citizen$A12) # A12. 전체 표준편차
mean(raw_citizen$A12) # A12. 전체 5점 평균
25*(mean(raw_citizen$A12-1)) # A12. 전체 100점 평균 환산


A12_data1 <- raw_citizen %>% # A12. 성별 구분
  group_by(A12) %>% 
  select(SQ1)
table(A12_data1)
t_A12_1 <- t(table(A12_data1)) # 행렬변환
t_A12_1
A12_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A12 = sd(A12)) %>% as.data.frame()
A12_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A12 = mean(A12)) %>% as.data.frame()
A12_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A12 = 25*(mean(A12)-1)) %>% as.data.frame()


A12_data2 <- raw_citizen %>% # A12. 연령대 구분
  group_by(A12) %>% 
  select(SQ2_2)
table(A12_data2)
t_A12_2 <- t(table(A12_data2)) # 행렬변환
t_A12_2
A12_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A12 = sd(A12)) %>% as.data.frame()
A12_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A12 = mean(A12)) %>% as.data.frame()
A12_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A12 = 25*(mean(A12)-1)) %>% as.data.frame()


A12_data3 <- raw_citizen %>% # A12. 소득 구분
  group_by(A12) %>% 
  select(DQ8_1)
table(A12_data3)
t_A12_3 <- t(table(A12_data3)) # 행렬변환
t_A12_3
A12_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A12 = sd(A12)) %>% as.data.frame()
A12_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A12 = mean(A12)) %>% as.data.frame()
A12_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A12 = 25*(mean(A12)-1)) %>% as.data.frame()


A12_data4 <- raw_citizen %>% # A12. 송전탑 가시거리 거주 구분
  group_by(A12) %>% 
  select(A1)
t(table(A12_data4)) # 행렬변환
A12_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A12 = sd(A12)) %>% as.data.frame()
A12_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A12 = mean(A12)) %>% as.data.frame()
A12_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A12 = 25*(mean(A12)-1)) %>% as.data.frame()

A13_data1 <- raw_citizen %>% # A13. 성별 구분
  group_by(A13) %>% 
  select(SQ1)
t(table(A13_data1)) # 행렬변환


A13_data2 <- raw_citizen %>% # A13. 연령별 구분
  group_by(A13) %>% 
  select(SQ2_2)
t(table(A13_data2)) # 행렬변환


A13_data3 <- raw_citizen %>% # A13. 소득별 구분
  group_by(A13) %>% 
  select(DQ8_1)
t(table(A13_data3)) # 행렬변환


A13_data4 <- raw_citizen %>% # A13. 송전탑 가시거리 거주 구분
  group_by(A13) %>% 
  select(A1)
t(table(A13_data4)) # 행렬변환


A14_data1 <- raw_citizen %>% # A14. 성별 구분
  group_by(A14) %>% 
  select(SQ1)
t(table(A14_data1)) # 행렬변환


A14_data2 <- raw_citizen %>% # A14. 연령별 구분
  group_by(A14) %>% 
  select(SQ2_2)
t(table(A14_data2)) # 행렬변환


A14_data3 <- raw_citizen %>% # A14. 소득별 구분
  group_by(A14) %>% 
  select(DQ8_1)
t(table(A14_data3)) # 행렬변환


A14_data4 <- raw_citizen %>% # A14. 송전탑 가시거리 거주 구분
  group_by(A14) %>% 
  select(A1)
t(table(A14_data4)) # 행렬변환


A15_data1 <- raw_citizen %>% # A15. 성별 구분
  group_by(A15) %>% 
  select(SQ1)
t(table(A15_data1)) # 행렬변환


A15_data2 <- raw_citizen %>% # A15. 연령별 구분
  group_by(A15) %>% 
  select(SQ2_2)
t(table(A15_data2)) # 행렬변환


A15_data3 <- raw_citizen %>% # A15. 소득별 구분
  group_by(A15) %>% 
  select(DQ8_1)
t(table(A15_data3)) # 행렬변환


A15_data4 <- raw_citizen %>% # A15. 송전탑 가시거리 거주 구분
  group_by(A15) %>% 
  select(A1)
t(table(A15_data4)) # 행렬변환
