library(dplyr)
library(haven)
options(digits = 5) # 소수점 확대


rawdata <- read_spss("Rawdata.sav")
raw_citizen <- rawdata %>%  # 시흥시민 364명 필터링
  filter(Gubun_Area == 1)


table(raw_citizen$DQ8_1) # 가구소득 재그룹화
raw_citizen$DQ8_1 <- ifelse(raw_citizen$DQ8_1 == 1, "100 만원 이하",
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


A16_1_data4 <- raw_citizen %>% # A16-1. 송전탑 가시거리 거주 구분
  group_by(A16_1) %>% 
  select(A1)
t(table(A16_1_data4)) # 행렬변환

A16_1_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_1 = sd(A16_1)) %>% as.data.frame()
A16_1_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_1 = mean(A16_1)) %>% as.data.frame()
A16_1_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
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


A16_2_data4 <- raw_citizen %>% # A16-2. 송전탑 가시거리 거주 구분
  group_by(A16_2) %>% 
  select(A1)
t(table(A16_2_data4)) # 행렬변환

A16_2_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_2 = sd(A16_2)) %>% as.data.frame()
A16_2_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_2 = mean(A16_2)) %>% as.data.frame()
A16_2_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A16_2 = 25*(mean(A16_2)-1)) %>% as.data.frame()


sd(raw_citizen$A16_3) # A16-3. 전체 표준편차
mean(raw_citizen$A16_3) # A16-3. 전체 5점 평균
25*(mean(raw_citizen$A16_3-1)) # A16-3. 전체 100점 평균 환산


A16_3_data1 <- raw_citizen %>% # A16-3. 성별 구분
  group_by(A16_3) %>% 
  select(SQ1)
t(table(A16_3_data1)) # 행렬변환

A16_3_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_3 = sd(A16_3)) %>% as.data.frame()
A16_3_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_3 = mean(A16_3)) %>% as.data.frame()
A16_3_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_3 = 25*(mean(A16_3)-1)) %>% as.data.frame()


A16_3_data2 <- raw_citizen %>% # A16-3. 연령대 구분
  group_by(A16_3) %>% 
  select(SQ2_2)
t(table(A16_3_data2)) # 행렬변환


A16_3_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_3 = sd(A16_3)) %>% as.data.frame()
A16_3_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_3 = mean(A16_3)) %>% as.data.frame()
A16_3_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_3 = 25*(mean(A16_3)-1)) %>% as.data.frame()


A16_3_data3 <- raw_citizen %>% # A16-3. 소득 구분
  group_by(A16_3) %>% 
  select(DQ8_1)
t(table(A16_3_data3)) # 행렬변환


A16_3_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_3 = sd(A16_3)) %>% as.data.frame()
A16_3_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_3 = mean(A16_3)) %>% as.data.frame()
A16_3_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_3 = 25*(mean(A16_3)-1)) %>% as.data.frame()


A16_3_data4 <- raw_citizen %>% # A16-3. 송전탑 가시거리 거주 구분
  group_by(A16_3) %>% 
  select(A1)
t(table(A16_3_data4)) # 행렬변환

A16_3_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_3 = sd(A16_3)) %>% as.data.frame()
A16_3_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_3 = mean(A16_3)) %>% as.data.frame()
A16_3_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A16_3 = 25*(mean(A16_3)-1)) %>% as.data.frame()


sd(raw_citizen$A16_4) # A16-4. 전체 표준편차
mean(raw_citizen$A16_4) # A16-4. 전체 5점 평균
25*(mean(raw_citizen$A16_4-1)) # A16-4. 전체 100점 평균 환산


A16_4_data1 <- raw_citizen %>% # A16-4. 성별 구분
  group_by(A16_4) %>% 
  select(SQ1)
t(table(A16_4_data1)) # 행렬변환

A16_4_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_4 = sd(A16_4)) %>% as.data.frame()
A16_4_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_4 = mean(A16_4)) %>% as.data.frame()
A16_4_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_4 = 25*(mean(A16_4)-1)) %>% as.data.frame()


A16_4_data2 <- raw_citizen %>% # A16-4. 연령대 구분
  group_by(A16_4) %>% 
  select(SQ2_2)
t(table(A16_4_data2)) # 행렬변환


A16_4_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_4 = sd(A16_4)) %>% as.data.frame()
A16_4_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_4 = mean(A16_4)) %>% as.data.frame()
A16_4_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_4 = 25*(mean(A16_4)-1)) %>% as.data.frame()


A16_4_data3 <- raw_citizen %>% # A16-4. 소득 구분
  group_by(A16_4) %>% 
  select(DQ8_1)
t(table(A16_4_data3)) # 행렬변환


A16_4_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_4 = sd(A16_4)) %>% as.data.frame()
A16_4_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_4 = mean(A16_4)) %>% as.data.frame()
A16_4_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_4 = 25*(mean(A16_4)-1)) %>% as.data.frame()


A16_4_data4 <- raw_citizen %>% # A16-4. 송전탑 가시거리 거주 구분
  group_by(A16_4) %>% 
  select(A1)
t(table(A16_4_data4)) # 행렬변환

A16_4_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_4 = sd(A16_4)) %>% as.data.frame()
A16_4_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_4 = mean(A16_4)) %>% as.data.frame()
A16_4_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A16_4 = 25*(mean(A16_4)-1)) %>% as.data.frame()


sd(raw_citizen$A16_5) # A16-5. 전체 표준편차
mean(raw_citizen$A16_5) # A16-5. 전체 5점 평균
25*(mean(raw_citizen$A16_5-1)) # A16-5. 전체 100점 평균 환산


A16_5_data1 <- raw_citizen %>% # A16-5. 성별 구분
  group_by(A16_5) %>% 
  select(SQ1)
t(table(A16_5_data1)) # 행렬변환

A16_5_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_5 = sd(A16_5)) %>% as.data.frame()
A16_5_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_5 = mean(A16_5)) %>% as.data.frame()
A16_5_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_5 = 25*(mean(A16_5)-1)) %>% as.data.frame()


A16_5_data2 <- raw_citizen %>% # A16-5. 연령대 구분
  group_by(A16_5) %>% 
  select(SQ2_2)
t(table(A16_5_data2)) # 행렬변환


A16_5_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_5 = sd(A16_5)) %>% as.data.frame()
A16_5_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_5 = mean(A16_5)) %>% as.data.frame()
A16_5_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_5 = 25*(mean(A16_5)-1)) %>% as.data.frame()


A16_5_data3 <- raw_citizen %>% # A16-5. 소득 구분
  group_by(A16_5) %>% 
  select(DQ8_1)
t(table(A16_5_data3)) # 행렬변환


A16_5_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_5 = sd(A16_5)) %>% as.data.frame()
A16_5_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_5 = mean(A16_5)) %>% as.data.frame()
A16_5_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_5 = 25*(mean(A16_5)-1)) %>% as.data.frame()


A16_5_data4 <- raw_citizen %>% # A16-5. 송전탑 가시거리 거주 구분
  group_by(A16_5) %>% 
  select(A1)
t(table(A16_5_data4)) # 행렬변환

A16_5_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_5 = sd(A16_5)) %>% as.data.frame()
A16_5_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_5 = mean(A16_5)) %>% as.data.frame()
A16_5_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A16_5 = 25*(mean(A16_5)-1)) %>% as.data.frame()


sd(raw_citizen$A16_6) # A16-6. 전체 표준편차
mean(raw_citizen$A16_6) # A16-6. 전체 5점 평균
25*(mean(raw_citizen$A16_6-1)) # A16-6. 전체 100점 평균 환산


A16_6_data1 <- raw_citizen %>% # A16-6. 성별 구분
  group_by(A16_6) %>% 
  select(SQ1)
t(table(A16_6_data1)) # 행렬변환

A16_6_data1 %>% # 성별 표준편차
  group_by(SQ1) %>% 
  summarise(sd_A16_6 = sd(A16_6)) %>% as.data.frame()
A16_6_data1 %>% # 성별 5점평균
  group_by(SQ1) %>% 
  summarise(mean5_A16_6 = mean(A16_6)) %>% as.data.frame()
A16_6_data1 %>% # 성별 100점평균
  group_by(SQ1) %>%
  summarise(mean100_A16_6 = 25*(mean(A16_6)-1)) %>% as.data.frame()


A16_6_data2 <- raw_citizen %>% # A16-6. 연령대 구분
  group_by(A16_6) %>% 
  select(SQ2_2)
t(table(A16_6_data2)) # 행렬변환


A16_6_data2 %>% # 연령별 표준편차
  group_by(SQ2_2) %>% 
  summarise(sd_A16_6 = sd(A16_6)) %>% as.data.frame()
A16_6_data2 %>% # 연령별 5점평균
  group_by(SQ2_2) %>% 
  summarise(mean5_A16_6 = mean(A16_6)) %>% as.data.frame()
A16_6_data2 %>% # 연령별 100점평균
  group_by(SQ2_2) %>%
  summarise(mean100_A16_6 = 25*(mean(A16_6)-1)) %>% as.data.frame()


A16_6_data3 <- raw_citizen %>% # A16-6. 소득 구분
  group_by(A16_6) %>% 
  select(DQ8_1)
t(table(A16_6_data3)) # 행렬변환


A16_6_data3 %>% # 소득별 표준편차
  group_by(DQ8_1) %>% 
  summarise(sd_A16_6 = sd(A16_6)) %>% as.data.frame()
A16_6_data3 %>% # 소득별 5점평균
  group_by(DQ8_1) %>% 
  summarise(mean5_A16_6 = mean(A16_6)) %>% as.data.frame()
A16_6_data3 %>% # 소득별 100점평균
  group_by(DQ8_1) %>%
  summarise(mean100_A16_6 = 25*(mean(A16_6)-1)) %>% as.data.frame()


A16_6_data4 <- raw_citizen %>% # A16-6. 송전탑 가시거리 거주 구분
  group_by(A16_6) %>% 
  select(A1)
t(table(A16_6_data4)) # 행렬변환

A16_6_data4 %>% # 소득별 표준편차
  group_by(A1) %>% 
  summarise(sd_A16_6 = sd(A16_6)) %>% as.data.frame()
A16_6_data4 %>% # 소득별 5점평균
  group_by(A1) %>% 
  summarise(mean5_A16_6 = mean(A16_6)) %>% as.data.frame()
A16_6_data4 %>% # 소득별 100점평균
  group_by(A1) %>%
  summarise(mean100_A16_6 = 25*(mean(A16_6)-1)) %>% as.data.frame()
