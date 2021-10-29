library(dplyr)
library(haven)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(esquisse)


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
A6_2_df$Freq <- A6_1_df$Freq + A6_2_df$Freq
A6_2_df
A6_df <- left_join(A6_1_df,A6_2_df, by = "A6_matrix")
A6_df


ggplot(A6_df) +
 geom_col(aes(x = A6_matrix, y = Freq.x), fill = "#4682B4") +
 geom_text(aes(label = Freq.x, x = A6_matrix, y = Freq.x), vjust = 1.2, color = "white") +
 geom_line(aes(x = A6_matrix, y = Freq.y, group = 1)) +
 geom_point(aes(x = A6_matrix, y = Freq.y, group = 1)) +
 geom_text(aes(label = Freq.y, x = A6_matrix, y = Freq.y, vjust = -1.0)) +
 labs(x = "응답구분", 
      y = "응답비율", title = "A6.송전탑 건립 입지문제와 관련하여 지역주민반발이\n과격화, 폭력화 되는 원인은 무엇이라고 보십니까?", subtitle = "(시흥시민 364명 대상)") +
 theme_minimal() +
theme(plot.title = element_text(size = 15L, 
                                face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L,                                                                                                        face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A7_1_data <- raw_citizen %>% # A7-1. 데이터 전처리
  select(A7_1)
A7_1_matrix <- as.matrix(A7_1_data)
A7_1_percent <- prop.table(table(A7_1_matrix)) * 100
A7_1_percent <- round(A7_1_percent, 1)
A7_1_df <- as.data.frame(A7_1_percent)
A7_1_df <- rename(A7_1_df, A7_matrix = A7_1_matrix)
A7_1_df <- A7_1_df %>% mutate(group=1)
A7_1_df
A7_2_data <- raw_citizen %>% # A7-2. 데이터 전처리
  select(A7_2)
A7_2_matrix <- as.matrix(A7_2_data)
A7_2_percent <- prop.table(table(A7_2_matrix)) * 100
A7_2_percent <- round(A7_2_percent, 1)
A7_2_df <- as.data.frame(A7_2_percent)
A7_2_df <- rename(A7_2_df, A7_matrix = A7_2_matrix)
A7_2_df <- A7_2_df %>% mutate(group=2)
A7_2_df$Freq <- A7_1_df$Freq + A7_2_df$Freq
A7_2_df
A7_df <- left_join(A7_1_df,A7_2_df, by = "A7_matrix")
A7_df


ggplot(A7_df) +
  geom_col(aes(x = A7_matrix, y = Freq.x), fill = "#4682B4") +
  geom_text(aes(label = Freq.x, x = A7_matrix, y = Freq.x), vjust = 1.2, color = "white") +
  geom_line(aes(x = A7_matrix, y = Freq.y, group = 1)) +
  geom_point(aes(x = A7_matrix, y = Freq.y, group = 1)) +
  geom_text(aes(label = Freq.y, x = A7_matrix, y = Freq.y, vjust = -1.0)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A7.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A8_1_data <- raw_citizen %>% # A8-1. 데이터 전처리
  select(A8_1)
A8_1_matrix <- as.matrix(A8_1_data)
A8_1_percent <- prop.table(table(A8_1_matrix)) * 100
A8_1_percent <- round(A8_1_percent, 1)
A8_1_df <- as.data.frame(A8_1_percent)
A8_1_df <- rename(A8_1_df, A8_matrix = A8_1_matrix)
A8_1_df <- A8_1_df %>% mutate(group=1)
A8_1_df
A8_2_data <- raw_citizen %>% # A8-2. 데이터 전처리
  select(A8_2)
A8_2_matrix <- as.matrix(A8_2_data)
A8_2_percent <- prop.table(table(A8_2_matrix)) * 100
A8_2_percent <- round(A8_2_percent, 1)
A8_2_df <- as.data.frame(A8_2_percent)
A8_2_df <- rename(A8_2_df, A8_matrix = A8_2_matrix)
A8_2_df <- A8_2_df %>% mutate(group=2)
A8_2_df$Freq <- A8_1_df$Freq + A8_2_df$Freq
A8_2_df
A8_df <- left_join(A8_1_df,A8_2_df, by = "A8_matrix")
A8_df


ggplot(A8_df) +
  geom_col(aes(x = A8_matrix, y = Freq.x), fill = "#4682B4") +
  geom_text(aes(label = Freq.x, x = A8_matrix, y = Freq.x), vjust = 1.2, color = "white") +
  geom_line(aes(x = A8_matrix, y = Freq.y, group = 1)) +
  geom_point(aes(x = A8_matrix, y = Freq.y, group = 1)) +
  geom_text(aes(label = Freq.y, x = A8_matrix, y = Freq.y, vjust = -1.0)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A8.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

  
A9_data <- raw_citizen %>% # A9. 데이터 전처리
  select(A9)
A9_matrix <- as.matrix(A9_data)
A9_matrix
A9_percent <- prop.table(table(A9_matrix)) * 100
A9_percent
A9_percent <- round(A9_percent, 1)
A9_df <- as.data.frame(A9_percent)
A9_df

ggplot(A9_df) + # A9. 시각화
  aes(x = A9_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A9.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A9_data2 <- raw_citizen %>% # A9. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A9 = mean(A9))
A9_data2  
ggplot(A9_data2) + # A9. 5점 척도 성별 시각화
 aes(x = SQ1, y = mean_A9, fill = A1) +
 geom_col(position = "dodge") +
 geom_text(aes(label = round(mean_A9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A9.", subtitle = "시흥시민 364명 대상") +
 theme_minimal() +
 theme(plot.title = element_text(size = 15L, 
                                 face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                               face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A9_data3 <- raw_citizen %>% # A9. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A9 = mean(A9))

ggplot(A9_data3) + # A9. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A9.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A9_data4 <- raw_citizen %>% # A9. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A9 = mean(A9))
A9_data4
ggplot(A9_data4) + # A9. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A9.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A9_1_data <- raw_citizen %>% # A9_1. 데이터 전처리
  select(A9_1)
A9_1_matrix <- as.matrix(A9_1_data)
A9_1_percent <- prop.table(table(A9_1_matrix)) * 100
A9_1_percent <- round(A9_1_percent, 1)
A9_1_df <- as.data.frame(A9_1_percent)
A9_1_df

ggplot(A9_1_df) + # A9-1. 시각화
  aes(x = A9_1_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A9-1.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A10_data <- raw_citizen %>% # A10. 데이터 전처리
  select(A10)
A10_matrix <- as.matrix(A10_data)
A10_percent <- prop.table(table(A10_matrix)) * 100
A10_percent <- round(A10_percent, 1)
A10_df <- as.data.frame(A10_percent)
A10_df

ggplot(A10_df) + # A10. 시각화
  aes(x = A10_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A10.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A11_data <- raw_citizen %>% # A11. 데이터 전처리
  select(A11)
A11_matrix <- as.matrix(A11_data)
A11_percent <- prop.table(table(A11_matrix)) * 100
A11_percent <- round(A11_percent, 1)
A11_df <- as.data.frame(A11_percent)
A11_df

ggplot(A11_df) + # A10. 시각화
  aes(x = A11_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A11.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


