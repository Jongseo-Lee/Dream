library(dplyr)
library(haven)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(esquisse)

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

View(raw_citizen)

A1_data <- raw_citizen %>% # A1. 데이터 전처리
  select(A1)
A1_matrix <- as.matrix(A1_data)
A1_percent <- prop.table(table(A1_matrix)) * 100
A1_percent <- round(A1_percent, 1)
A1_df <- as.data.frame(A1_percent)
A1_df

ggplot(A1_df) + # A1. 시각화
 aes(x = A1_matrix, weight = Freq) +
 geom_bar(fill = "#4682B4") +
 geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
 labs(x = "응답구분", 
 y = "응답비율", title = "A1.", subtitle = "시흥시민 364명 대상") +
 theme_minimal() +
 theme(plot.title = element_text(size = 15L, 
 face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A1_2_data <- raw_citizen %>% # A1-2. 데이터 전처리
  select(A1_2)
A1_2_matrix <- as.matrix(A1_2_data)
A1_2_percent <- prop.table(table(A1_2_matrix)) * 100
A1_2_percent <- round(A1_2_percent, 1)
A1_2_df <- as.data.frame(A1_2_percent)
A1_2_df

ggplot(A1_2_df) + # A1-2. 시각화
  aes(x = A1_2_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A1-2.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 13L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A4_data <- raw_citizen %>% # A4. 데이터 전처리
  select(A4)
A4_matrix <- as.matrix(A4_data)
A4_percent <- prop.table(table(A4_matrix)) * 100
A4_percent <- round(A4_percent, 1)
A4_df <- as.data.frame(A4_percent)
A4_df

ggplot(A4_df) + # A4. 시각화
  aes(x = A4_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A4.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A5_data <- raw_citizen %>% # A5. 데이터 전처리
  select(A5)
A5_matrix <- as.matrix(A5_data)
A5_percent <- prop.table(table(A5_matrix)) * 100
A5_percent <- round(A5_percent, 1)
A5_df <- as.data.frame(A5_percent)
A5_df

ggplot(A5_df) + # A5. 시각화
  aes(x = A5_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A5.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A6_1_data <- raw_citizen %>% # A6-1. 데이터 전처리
  select(A6_1)
A6_1_matrix <- as.matrix(A6_1_data)
A6_1_percent <- prop.table(table(A6_1_matrix)) * 100
A6_1_percent <- round(A6_1_percent, 1)
A6_1_df <- as.data.frame(A6_1_percent)
A6_1_df <- rename(A6_1_df, A6_matrix = A6_1_matrix)
A6_1_df <- A6_1_df %>% mutate(group=1)
A6_1_df
A6_2_data <- raw_citizen %>% # A6-2. 데이터 전처리
  select(A6_2)
A6_2_matrix <- as.matrix(A6_2_data)
A6_2_percent <- prop.table(table(A6_2_matrix)) * 100
A6_2_percent <- round(A6_2_percent, 1)
A6_2_df <- as.data.frame(A6_2_percent)
A6_2_df <- rename(A6_2_df, A6_matrix = A6_2_matrix)
A6_2_df <- A6_2_df %>% mutate(group=2)
A6_2_df
A6_df2 <- left_join(A6_1_df,A6_2_df, by = "A6_matrix")
A6_df2
A6_df <- bind_rows(A6_1_df,A6_2_df)
A6_df

windows()
ggplot(A6_df2) +
 geom_col(aes(x = A6_matrix, y = Freq.x), fill = "#4682B4") +
 geom_text(aes(label = Freq.x, x = A6_matrix, y = Freq.x), vjust = 1.2, color = "white") +
 geom_line(aes(x = A6_matrix, y = Freq.y, group = 1)) +
 geom_point(aes(x = A6_matrix, y = Freq.y, group = 1)) +
 geom_text(aes(label = Freq.y, x = A6_matrix, y = Freq.y, vjust = -1.0)) +
 labs(x = "응답구분", 
      y = "응답비율", title = "A6.송전탑 건립 입지문제와 관련하여 지역주민반발이\n과격화, 폭력화 되는 원인은 무엇이라고 보십니까?", subtitle = "시흥시민 364명 대상") +
 theme_minimal() +
theme(plot.title = element_text(size = 15L, 
                                face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))
  
  
