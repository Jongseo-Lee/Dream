# 드림학기 과제 1번

install.packages("dplyr")
install.packages("haven")
library(dplyr)
library(haven)

rawdata <- read_spss("Rawdata.sav")
table(is.na(rawdata))
View(rawdata)

raw_citizen <- rawdata %>%  # 시흥시민 364명 필터링
  filter(Gubun_Area == 1)
View(raw_citizen)

table(raw_citizen$SQ1) # 성별 빈도

table(raw_citizen$SQ2_2) # 나이 빈도

table(raw_citizen$DQ4) # 학력수준 재그룹화 필요
raw_edu <- ifelse(raw_citizen$DQ4 == 0, "무학",
                  ifelse(raw_citizen$DQ4 %in% c(1,2,3,4,5,6), "초등학교",
                         ifelse(raw_citizen$DQ4 %in% c(7,8,9), "중학교", 
                                ifelse(raw_citizen$DQ4 %in% c(10,11,12), "고등학교",
                                       ifelse(raw_citizen$DQ4 %in% c(13,14,15,16), "대학교", "대학원")))))
table(raw_edu) # 학력수준 빈도

table(raw_citizen$DQ8_1) # 가구소득 재그룹화 필요
raw_income <- ifelse(raw_citizen$DQ8_1 == 1, "100만원 이하",
                  ifelse(raw_citizen$DQ8_1 %in% c(2,3), "100~200만원",
                         ifelse(raw_citizen$DQ8_1 %in% c(4,5), "200~300만원", 
                                ifelse(raw_citizen$DQ8_1 %in% c(6), "300~400만원",
                                       ifelse(raw_citizen$DQ8_1 %in% c(7,8,9,10), "400만원 이상", "소득없음")))))
table(raw_income) # 가구소득 빈도
table(raw_citizen$DQ8_1 == 0) # 소득없음 빈도 0 확인
