library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")
table(is.na(rawdata))
View(rawdata)

raw_citizen <- rawdata %>%  # 시흥시민 364명 필터링
  filter(Gubun_Area == 1)
View(raw_citizen)

A1_data1 <- raw_citizen %>% # A1. 성별 구분
  group_by(A1) %>% 
  select(SQ1)
table(A1_data1)

A1_data2 <- raw_citizen %>% # A1. 연령대 구분
  group_by(A1) %>% 
  select(SQ2_2)
table(A1_data2)

A1_2_data1 <- raw_citizen %>% # A1_2. 성별 구분
  group_by(A1_2) %>% 
  select(SQ1)
table(A1_2_data1)

A1_2_data2 <- raw_citizen %>% # A1_2. 연령대 구분
  group_by(A1_2) %>% 
  select(SQ2_2)
table(A1_2_data2)

A4_data1 <- raw_citizen %>% # A4. 성별 구분
  group_by(A4) %>% 
  select(SQ1)
table(A4_data1)

A4_data2 <- raw_citizen %>% # A4. 연령대 구분
  group_by(A4) %>% 
  select(SQ2_2)
table(A4_data2)

A5_data1 <- raw_citizen %>% # A5. 성별 구분
  group_by(A5) %>% 
  select(SQ1)
table(A5_data1)

A5_data2 <- raw_citizen %>% # A5. 연령대 구분
  group_by(A5) %>% 
  select(SQ2_2)
table(A5_data2)

A6_1_data1 <- raw_citizen %>% # A6_1. 성별 구분
  group_by(A6_1) %>% 
  select(SQ1)
table(A6_1_data1)

A6_1_data2 <- raw_citizen %>% # A6_1. 연령대 구분
  group_by(A6_1) %>% 
  select(SQ2_2)
table(A6_1_data2)

A6_2_data1 <- raw_citizen %>% # A6_2. 성별 구분
  group_by(A6_2) %>% 
  select(SQ1)
table(A6_2_data1)

A6_2_data2 <- raw_citizen %>% # A6_2. 연령대 구분
  group_by(A6_2) %>% 
  select(SQ2_2)
table(A6_2_data2)

A7_1_data1 <- raw_citizen %>% # A7_1. 성별 구분
  group_by(A7_1) %>% 
  select(SQ1)
table(A7_1_data1)

A7_1_data2 <- raw_citizen %>% # A7_1. 연령대 구분
  group_by(A7_1) %>% 
  select(SQ2_2)
table(A7_1_data2)

A7_2_data1 <- raw_citizen %>% # A7_2. 성별 구분
  group_by(A7_2) %>% 
  select(SQ1)
table(A7_2_data1)

A7_2_data2 <- raw_citizen %>% # A7_2. 연령대 구분
  group_by(A7_2) %>% 
  select(SQ2_2)
table(A7_2_data2)

A8_1_data1 <- raw_citizen %>% # A8_1. 성별 구분
  group_by(A8_1) %>% 
  select(SQ1)
table(A8_1_data1)

A8_1_data2 <- raw_citizen %>% # A8_1. 연령대 구분
  group_by(A8_1) %>% 
  select(SQ2_2)
table(A8_1_data2)

A8_2_data1 <- raw_citizen %>% # A8_2. 성별 구분
  group_by(A8_2) %>% 
  select(SQ1)
table(A8_2_data1)

A8_2_data2 <- raw_citizen %>% # A8_2. 연령대 구분
  group_by(A8_2) %>% 
  select(SQ2_2)
table(A8_2_data2)
