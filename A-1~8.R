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
raw_citizen$DQ8_1 <- ifelse(raw_citizen$DQ8_1 == 1, "100 만원 이하",
                            ifelse(raw_citizen$DQ8_1 %in% c(2,3), "100~200만원",
                                   ifelse(raw_citizen$DQ8_1 %in% c(4,5), "200~300만원", 
                                          ifelse(raw_citizen$DQ8_1 %in% c(6), "300~400만원",
                                                 ifelse(raw_citizen$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))

A1_data1 <- raw_citizen %>% # A1. 성별 구분
  group_by(A1) %>% 
  select(SQ1)
t(table(A1_data1))

A1_data2 <- raw_citizen %>% # A1. 연령대 구분
  group_by(A1) %>% 
  select(SQ2_2)
t(table(A1_data2)) # 행렬변환

A1_data3 <- raw_citizen %>% # A1. 소득 구분
  group_by(A1) %>% 
  select(DQ8_1)
t(table(A1_data3)) # 행렬변환

A1_2_data1 <- raw_citizen %>% # A1_2. 성별 구분
  group_by(A1_2) %>% 
  select(SQ1)
t(table(A1_2_data1)) # 행렬변환

A1_2_data2 <- raw_citizen %>% # A1_2. 연령대 구분
  group_by(A1_2) %>% 
  select(SQ2_2)
t(table(A1_2_data2)) # 행렬변환

A1_2_data3 <- raw_citizen %>% # A1_2. 소득 구분
  group_by(A1_2) %>% 
  select(DQ8_1)
t(table(A1_2_data3)) # 행렬변환

A4_data1 <- raw_citizen %>% # A4. 성별 구분
  group_by(A4) %>% 
  select(SQ1)
t(table(A4_data1)) # 행렬변환

A4_data2 <- raw_citizen %>% # A4. 연령대 구분
  group_by(A4) %>% 
  select(SQ2_2)
t(table(A4_data2)) # 행렬변환

A4_data3 <- raw_citizen %>% # A4. 소득 구분
  group_by(A4) %>% 
  select(DQ8_1)
t(table(A4_data3)) # 행렬변환

A4_data4 <- raw_citizen %>% # A4. 송전탑 가시거리 거주 구분
  group_by(A4) %>% 
  select(A1)
t(table(A4_data4)) # 행렬변환

A5_data1 <- raw_citizen %>% # A5. 성별 구분
  group_by(A5) %>% 
  select(SQ1)
t(table(A5_data1)) # 행렬변환

A5_data2 <- raw_citizen %>% # A5. 연령대 구분
  group_by(A5) %>% 
  select(SQ2_2)
t(table(A5_data2)) # 행렬변환

A5_data3 <- raw_citizen %>% # A5. 소득 구분
  group_by(A5) %>% 
  select(DQ8_1)
t(table(A5_data3)) # 행렬변환

A5_data4 <- raw_citizen %>% # A5. 송전탑 가시거리 거주 구분
  group_by(A5) %>% 
  select(A1)
t(table(A5_data4)) # 행렬변환

A6_1_data1 <- raw_citizen %>% # A6_1. 성별 구분
  group_by(A6_1) %>% 
  select(SQ1)
t(table(A6_1_data1)) # 행렬변환

A6_1_data2 <- raw_citizen %>% # A6_1. 연령대 구분
  group_by(A6_1) %>% 
  select(SQ2_2)
t(table(A6_1_data2)) # 행렬변환

A6_1_data3 <- raw_citizen %>% # A6_1. 소득 구분
  group_by(A6_1) %>% 
  select(DQ8_1)
t(table(A6_1_data3)) # 행렬변환

A6_1_data4 <- raw_citizen %>% # A6_1. 송전탑 가시거리 거주 구분
  group_by(A6_1) %>% 
  select(A1)
t(table(A6_1_data4)) # 행렬변환

A6_2_data1 <- raw_citizen %>% # A6_2. 성별 구분
  group_by(A6_2) %>% 
  select(SQ1)
t(table(A6_2_data1)) # 행렬변환

A6_2_data2 <- raw_citizen %>% # A6_2. 연령대 구분
  group_by(A6_2) %>% 
  select(SQ2_2)
t(table(A6_2_data2)) # 행렬변환

A6_2_data3 <- raw_citizen %>% # A6_2. 소득 구분
  group_by(A6_2) %>% 
  select(DQ8_1)
t(table(A6_2_data3)) # 행렬변환

A6_2_data4 <- raw_citizen %>% # A6_2. 송전탑 가시거리 거주 구분
  group_by(A6_2) %>% 
  select(A1)
t(table(A6_2_data4)) # 행렬변환

A7_1_data1 <- raw_citizen %>% # A7_1. 성별 구분
  group_by(A7_1) %>% 
  select(SQ1)
t(table(A7_1_data1)) # 행렬변환

A7_1_data2 <- raw_citizen %>% # A7_1. 연령대 구분
  group_by(A7_1) %>% 
  select(SQ2_2)
t(table(A7_1_data2)) # 행렬변환

A7_1_data3 <- raw_citizen %>% # A7_1. 소득 구분
  group_by(A7_1) %>% 
  select(DQ8_1)
t(table(A7_1_data3)) # 행렬변환

A7_1_data4 <- raw_citizen %>% # A7_1. 송전탑 가시거리 거주 구분
  group_by(A7_1) %>% 
  select(A1)
t(table(A7_1_data4)) # 행렬변환

A7_2_data1 <- raw_citizen %>% # A7_2. 성별 구분
  group_by(A7_2) %>% 
  select(SQ1)
t(table(A7_2_data1)) # 행렬변환

A7_2_data2 <- raw_citizen %>% # A7_2. 연령대 구분
  group_by(A7_2) %>% 
  select(SQ2_2)
t(table(A7_2_data2)) # 행렬변환

A7_2_data3 <- raw_citizen %>% # A7_2. 소득 구분
  group_by(A7_2) %>% 
  select(DQ8_1)
t(table(A7_2_data3)) # 행렬변환

A7_2_data4 <- raw_citizen %>% # A7_2. 송전탑 가시거리 거주 구분
  group_by(A7_2) %>% 
  select(A1)
t(table(A7_2_data4)) # 행렬변환

A8_1_data1 <- raw_citizen %>% # A8_1. 성별 구분
  group_by(A8_1) %>% 
  select(SQ1)
t(table(A8_1_data1)) # 행렬변환

A8_1_data2 <- raw_citizen %>% # A8_1. 연령대 구분
  group_by(A8_1) %>% 
  select(SQ2_2)
t(table(A8_1_data2)) # 행렬변환

A8_1_data3 <- raw_citizen %>% # A8_1. 소득 구분
  group_by(A8_1) %>% 
  select(DQ8_1)
t(table(A8_1_data3)) # 행렬변환

A8_1_data4 <- raw_citizen %>% # A8_1. 송전탑 가시거리 거주 구분
  group_by(A8_1) %>% 
  select(A1)
t(table(A8_1_data4)) # 행렬변환

A8_2_data1 <- raw_citizen %>% # A8_2. 성별 구분
  group_by(A8_2) %>% 
  select(SQ1)
t(table(A8_2_data1)) # 행렬변환

A8_2_data2 <- raw_citizen %>% # A8_2. 연령대 구분
  group_by(A8_2) %>% 
  select(SQ2_2)
t(table(A8_2_data2)) # 행렬변환

A8_2_data3 <- raw_citizen %>% # A8_2. 소득 구분
  group_by(A8_2) %>% 
  select(DQ8_1)
t(table(A8_2_data3)) # 행렬변환

A8_2_data4 <- raw_citizen %>% # A8_2. 송전탑 가시거리 거주 구분
  group_by(A8_2) %>% 
  select(A1)
t(table(A8_2_data4)) # 행렬변환
