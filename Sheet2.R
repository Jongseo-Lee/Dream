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
