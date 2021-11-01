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
 y = "응답비율", title = "A1.귀하의 거주지 주변에 송전탑 및 \n송전선로가 존재합니까?", subtitle = "(시흥시민 364명 대상)") +
 theme_minimal() +
 theme(plot.title = element_text(size = 15L, 
 face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

windows()

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
       y = "응답비율", title = "A1-2. 귀하의 거주지에서 가장 가까운 송전탑이\n대략 몇m 떨어져 있습니까?", subtitle = "(시흥시민 364명 대상)") +
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
       y = "응답비율", title = "A4. 귀하께서는 송전탑을 둘러싼 이해관계자(정부,지자체,시민,한국전력)의\n 갈등증폭의 가장 큰 이유가 무엇이라고 생각하십니까?)", subtitle = "(시흥시민 364명 대상)") +
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
       y = "응답비율", title = "A5. 송전탑은 안정적인 에너지 공급 및 국민 편의를 위해 꼭 필요한 구조물이지만\n그 송전탑 및 송전선로 설치에 대한 인근 지역주민의 반발을 귀하께서는 어떻게 보십니까?", subtitle = "(시흥시민 364명 대상)") +
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
      y = "응답비율", title = "A6. 송전탑 건립 입지문제와 관련하여 지역주민반발이 과격화, 폭력화\n 되는 원인은 무엇이라고 보십니까? 중요한 순서대로 2위까지 응답해 주십시오", subtitle = "(시흥시민 364명 대상)") +
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
       y = "응답비율", title = "A7. 송전탑 건립 입지문제를 둘러싼 갈등해결과정에서 협상의 걸림돌을\n무엇이라고 생각하십니까? 중요한 순서대로 2위까지 응답해 주십시오.", subtitle = "(시흥시민 364명 대상)") +
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
       y = "응답비율", title = "A8. 송전탑 건립 입지결정과 관련한 갈등문제해결을 위한 협상의 방법 중 가장 실효성이\n높은해결방법은무엇이라고 보십니까? 중요한 순서대로 2위까지 응답해 주십시오.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

  
A9_data <- raw_citizen %>% # A9. 데이터 전처리
  select(A9)
A9_matrix <- as.matrix(A9_data)
A9_percent <- prop.table(table(A9_matrix)) * 100
A9_percent <- round(A9_percent, 1)
A9_df <- as.data.frame(A9_percent)

ggplot(A9_df) + # A9. 시각화
  aes(x = A9_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A9. 현재까지 송전탑 입지지역 내의 주민들에게 지급되고 있는\n 보상의 정도가 적정하다고 생각하십니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A9_data2 <- raw_citizen %>% # A9. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A9 = mean(A9))
ggplot(A9_data2) + # A9. 5점 척도 성별 시각화
 aes(x = SQ1, y = mean_A9, fill = A1) +
 geom_col(position = "dodge") +
 geom_text(aes(label = round(mean_A9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A9. 현재까지 송전탑 입지지역 내의 주민들에게 지급되고 있는\n 보상의 정도가 적정하다고 생각하십니까?", subtitle = "시흥시민 364명 대상") +
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
       y = "5점 척도", title = "A9. 현재까지 송전탑 입지지역 내의 주민들에게 지급되고 있는\n 보상의 정도가 적정하다고 생각하십니까?", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A9_data4 <- raw_citizen %>% # A9. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A9 = mean(A9))

ggplot(A9_data4) + # A9. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A9. 현재까지 송전탑 입지지역 내의 주민들에게 지급되고 있는\n 보상의 정도가 적정하다고 생각하십니까?", subtitle = "시흥시민 364명 대상") +
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
       y = "응답비율", title = "A9-1. (A9.에서 3. 보통이다 이상에 응답한 경우만 제시)\n적정하지 않다면 왜 그렇다고 생각하십니까?", subtitle = "(시흥시민 364명 대상)") +
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
       y = "응답비율", title = "A10. 송전탑 입지결정과 관련한 행정정보의 공개수준에 있어\n 귀하께서는 그 공개범위가 어디까지라고 생각하십니까?", subtitle = "(시흥시민 364명 대상)") +
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

ggplot(A11_df) + # A11. 시각화
  aes(x = A11_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A11. 송전탑입지 계획,결정과정 등에 관련 지역주민들이\n참여 하는것에 대해 어떻게 생각하십니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A12_data <- raw_citizen %>% # A12. 데이터 전처리
  select(A12)
A12_matrix <- as.matrix(A12_data)
A12_percent <- prop.table(table(A12_matrix)) * 100
A12_percent <- round(A12_percent, 1)
A12_df <- as.data.frame(A12_percent)

ggplot(A12_df) + # A12. 시각화
  aes(x = A12_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A12. 평소 송전탑 입지를 둘러 싼 이해관계자의 갈등을\n다루는데 있어 언론의 보도태도는 어떻습니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A12_data2 <- raw_citizen %>% # A12. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A12 = mean(A12))

ggplot(A12_data2) + # A12. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A12. 평소 송전탑 입지를 둘러 싼 이해관계자의 갈등을\n다루는데 있어 언론의 보도태도는 어떻습니까?", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A12_data3 <- raw_citizen %>% # A12. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A12 = mean(A12))

windows()
ggplot(A12_data3) + # A12. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A12. 평소 송전탑 입지를 둘러 싼 이해관계자의 갈등을\n다루는데 있어 언론의 보도태도는 어떻습니까?", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A12_data4 <- raw_citizen %>% # A12. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A12 = mean(A12))

ggplot(A12_data4) + # A12. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A12. 평소 송전탑 입지를 둘러 싼 이해관계자의 갈등을\n다루는데 있어 언론의 보도태도는 어떻습니까?", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A13_data <- raw_citizen %>% # A13. 데이터 전처리
  select(A13)
A13_matrix <- as.matrix(A13_data)
A13_percent <- prop.table(table(A13_matrix)) * 100
A13_percent <- round(A13_percent, 1)
A13_df <- as.data.frame(A13_percent)

ggplot(A13_df) + # A13. 시각화
  aes(x = A13_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A13. 귀하의 거주지역 근처에 송전탑이 들어선다면 어떻게 하시겠습니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A14_data <- raw_citizen %>% # A14. 데이터 전처리
  select(A14)
A14_matrix <- as.matrix(A14_data)
A14_percent <- prop.table(table(A14_matrix)) * 100
A14_percent <- round(A14_percent, 1)
A14_df <- as.data.frame(A14_percent)

ggplot(A14_df) + # A14. 시각화
  aes(x = A14_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A14. 송전탑 입지결정과 같은 갈등분쟁이 발생할 때\n어떤 기관이나 단체가 이를 가장 적절하게 조정할 수 있다라고 보십니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A15_data <- raw_citizen %>% # A15. 데이터 전처리
  select(A15)
A15_matrix <- as.matrix(A15_data)
A15_percent <- prop.table(table(A15_matrix)) * 100
A15_percent <- round(A15_percent, 1)
A15_df <- as.data.frame(A15_percent)

ggplot(A15_df) + # A15. 시각화
  aes(x = A15_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A15. 송전탑을 어디에 건설할 것인가를 결정(송전탑 경과지 선정) 하는데\n있어서, 주민의견을 수용하는 가장 합리적인 방안은 무엇입니까?", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_1_data <- raw_citizen %>% # A16-1. 데이터 전처리
  select(A16_1)
A16_1_matrix <- as.matrix(A16_1_data)
A16_1_percent <- prop.table(table(A16_1_matrix)) * 100
A16_1_percent <- round(A16_1_percent, 1)
A16_1_df <- as.data.frame(A16_1_percent)
windows()
ggplot(A16_1_df) + # A16_1. 시각화
  aes(x = A16_1_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-1. (주민 불이익 최소를 위한 중요도)송전탑 및 변전소를\n 혐오성, 위험성 등을 최소화할 수 있는 현대적 첨단시설로 설치한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_1_data2 <- raw_citizen %>% # A16-1. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_1 = mean(A16_1))

ggplot(A16_1_data2) + # A16_1. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-1. (주민 불이익 최소를 위한 중요도)송전탑 및 변전소를\n 혐오성, 위험성 등을 최소화할 수 있는 현대적 첨단시설로 설치한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_1_data3 <- raw_citizen %>% # A16-1. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_1 = mean(A16_1))

windows()
ggplot(A16_1_data3) + # A16-1. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-1. (주민 불이익 최소를 위한 중요도)송전탑 및 변전소를\n 혐오성, 위험성 등을 최소화할 수 있는 현대적 첨단시설로 설치한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_1_data4 <- raw_citizen %>% # A16-1. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_1 = mean(A16_1))

ggplot(A16_1_data4) + # A16-1. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-1. (주민 불이익 최소를 위한 중요도)송전탑 및 변전소를\n 혐오성, 위험성 등을 최소화할 수 있는 현대적 첨단시설로 설치한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A16_2_data <- raw_citizen %>% # A16-2. 데이터 전처리
  select(A16_2)
A16_2_matrix <- as.matrix(A16_2_data)
A16_2_percent <- prop.table(table(A16_2_matrix)) * 100
A16_2_percent <- round(A16_2_percent, 1)
A16_2_df <- as.data.frame(A16_2_percent)

ggplot(A16_2_df) + # A16_2. 시각화
  aes(x = A16_2_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-2. (주민 불이익 최소를 위한 중요도)조경시설 등 주위공간의\n 시설을 아름답게 가꾸어 미관상 좋지 않은 인상을 불식시킨다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_2_data2 <- raw_citizen %>% # A16-2. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_2 = mean(A16_2))

ggplot(A16_2_data2) + # A16-2. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-2. (주민 불이익 최소를 위한 중요도)조경시설 등 주위공간의\n 시설을 아름답게 가꾸어 미관상 좋지 않은 인상을 불식시킨다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_2_data3 <- raw_citizen %>% # A16-2. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_2 = mean(A16_2))

ggplot(A16_2_data3) + # A16-2. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-2. (주민 불이익 최소를 위한 중요도)조경시설 등 주위공간의\n 시설을 아름답게 가꾸어 미관상 좋지 않은 인상을 불식시킨다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_2_data4 <- raw_citizen %>% # A16-2. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_2 = mean(A16_2))

ggplot(A16_2_data4) + # A16-2. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-2. (주민 불이익 최소를 위한 중요도)조경시설 등 주위공간의\n 시설을 아름답게 가꾸어 미관상 좋지 않은 인상을 불식시킨다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A16_3_data <- raw_citizen %>% # A16-3. 데이터 전처리
  select(A16_3)
A16_3_matrix <- as.matrix(A16_3_data)
A16_3_percent <- prop.table(table(A16_3_matrix)) * 100
A16_3_percent <- round(A16_3_percent, 1)
A16_3_df <- as.data.frame(A16_3_percent)

ggplot(A16_3_df) + # A16-3. 시각화
  aes(x = A16_3_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-3. (주민 불이익 최소를 위한 중요도)송전탑에서 발생하는\n자기장 등을 항시 모니터링 하면서 안전에 만전을 기한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_3_data2 <- raw_citizen %>% # A16-3. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_3 = mean(A16_3))

ggplot(A16_3_data2) + # A16-3. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-3. (주민 불이익 최소를 위한 중요도)송전탑에서 발생하는\n자기장 등을 항시 모니터링 하면서 안전에 만전을 기한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_3_data3 <- raw_citizen %>% # A16-3. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_3 = mean(A16_3))

ggplot(A16_3_data3) + # A16-3. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-3. (주민 불이익 최소를 위한 중요도)송전탑에서 발생하는\n자기장 등을 항시 모니터링 하면서 안전에 만전을 기한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_3_data4 <- raw_citizen %>% # A16-3. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_3 = mean(A16_3))

ggplot(A16_3_data4) + # A16-3. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-3. (주민 불이익 최소를 위한 중요도)송전탑에서 발생하는\n자기장 등을 항시 모니터링 하면서 안전에 만전을 기한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A16_4_data <- raw_citizen %>% # A16-4. 데이터 전처리
  select(A16_4)
A16_4_matrix <- as.matrix(A16_4_data)
A16_4_percent <- prop.table(table(A16_4_matrix)) * 100
A16_4_percent <- round(A16_4_percent, 1)
A16_4_df <- as.data.frame(A16_4_percent)

ggplot(A16_4_df) + # A16-4. 시각화
  aes(x = A16_4_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-4. (주민 불이익 최소를 위한 중요도)지가하락으로 인한\n재산상의 손실을 적절히 보상해 준다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_4_data2 <- raw_citizen %>% # A16-4. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_4 = mean(A16_4))

ggplot(A16_4_data2) + # A16-4. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-4. (주민 불이익 최소를 위한 중요도)지가하락으로 인한\n재산상의 손실을 적절히 보상해 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_4_data3 <- raw_citizen %>% # A16-4. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_4 = mean(A16_4))

ggplot(A16_4_data3) + # A16-4. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-4. (주민 불이익 최소를 위한 중요도)지가하락으로 인한\n재산상의 손실을 적절히 보상해 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_4_data4 <- raw_citizen %>% # A16-4. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_4 = mean(A16_4))

ggplot(A16_4_data4) + # A16-4. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-4. (주민 불이익 최소를 위한 중요도)지가하락으로 인한\n재산상의 손실을 적절히 보상해 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A16_5_data <- raw_citizen %>% # A16-5. 데이터 전처리
  select(A16_5)
A16_5_matrix <- as.matrix(A16_5_data)
A16_5_percent <- prop.table(table(A16_5_matrix)) * 100
A16_5_percent <- round(A16_5_percent, 1)
A16_5_df <- as.data.frame(A16_5_percent)

ggplot(A16_5_df) + # A16-5. 시각화
  aes(x = A16_5_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-5. (주민 불이익 최소를 위한 중요도)지역주민을 위한 편익시설을\n 같이 건설하고 무료이용,사용료 감면 등의 혜택을 준다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_5_data2 <- raw_citizen %>% # A16-5. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_5 = mean(A16_5))

ggplot(A16_5_data2) + # A16-5. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-5. (주민 불이익 최소를 위한 중요도)지역주민을 위한 편익시설을\n 같이 건설하고 무료이용,사용료 감면 등의 혜택을 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_5_data3 <- raw_citizen %>% # A16-5. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_5 = mean(A16_5))

ggplot(A16_5_data3) + # A16-5. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-5. (주민 불이익 최소를 위한 중요도)지역주민을 위한 편익시설을\n 같이 건설하고 무료이용,사용료 감면 등의 혜택을 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_5_data4 <- raw_citizen %>% # A16-5. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_5 = mean(A16_5))

ggplot(A16_5_data4) + # A16-5. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-5. (주민 불이익 최소를 위한 중요도)지역주민을 위한 편익시설을\n 같이 건설하고 무료이용,사용료 감면 등의 혜택을 준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A16_6_data <- raw_citizen %>% # A16-6. 데이터 전처리
  select(A16_6)
A16_6_matrix <- as.matrix(A16_6_data)
A16_6_percent <- prop.table(table(A16_6_matrix)) * 100
A16_6_percent <- round(A16_6_percent, 1)
A16_6_df <- as.data.frame(A16_6_percent)

ggplot(A16_6_df) + # A16-6. 시각화
  aes(x = A16_6_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A16-6. (주민 불이익 최소를 위한 중요도)\n세금(재산세, 도시계획세 등)을 일정액 감면해준다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_6_data2 <- raw_citizen %>% # A16-6. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A16_6 = mean(A16_6))

ggplot(A16_6_data2) + # A16-6. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A16_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-6. (주민 불이익 최소를 위한 중요도)\n세금(재산세, 도시계획세 등)을 일정액 감면해준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_6_data3 <- raw_citizen %>% # A16-6. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A16_6 = mean(A16_6))

ggplot(A16_6_data3) + # A16-6. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A16_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-6. (주민 불이익 최소를 위한 중요도)\n세금(재산세, 도시계획세 등)을 일정액 감면해준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A16_6_data4 <- raw_citizen %>% # A16-6. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A16_6 = mean(A16_6))

ggplot(A16_6_data4) + # A16-6. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A16_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A16_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A16-6. (주민 불이익 최소를 위한 중요도)\n세금(재산세, 도시계획세 등)을 일정액 감면해준다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_1_data <- raw_citizen %>% # A17-1. 데이터 전처리
  select(A17_1)
A17_1_matrix <- as.matrix(A17_1_data)
A17_1_percent <- prop.table(table(A17_1_matrix)) * 100
A17_1_percent <- round(A17_1_percent, 1)
A17_1_df <- as.data.frame(A17_1_percent)

ggplot(A17_1_df) + # A16-6. 시각화
  aes(x = A17_1_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-1. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로 건립은 사회적 갈등 문제라 생각한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_1_data2 <- raw_citizen %>% # A17-1. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_1 = mean(A17_1))

ggplot(A17_1_data2) + # A17-1. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-1. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로 건립은 사회적 갈등 문제라 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_1_data3 <- raw_citizen %>% # A17-1. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_1 = mean(A17_1))

ggplot(A17_1_data3) + # A17-1. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-1. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로 건립은 사회적 갈등 문제라 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_1_data4 <- raw_citizen %>% # A17-1. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_1 = mean(A17_1))

ggplot(A17_1_data4) + # A17-1. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_1, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_1,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-1. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로 건립은 사회적 갈등 문제라 생각한다..", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_2_data <- raw_citizen %>% # A17-2. 데이터 전처리
  select(A17_2)
A17_2_matrix <- as.matrix(A17_2_data)
A17_2_percent <- prop.table(table(A17_2_matrix)) * 100
A17_2_percent <- round(A17_2_percent, 1)
A17_2_df <- as.data.frame(A17_2_percent)

ggplot(A17_2_df) + # A17-2. 시각화
  aes(x = A17_2_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-2. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과할 경우 지역의 문제라고 생각한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_2_data2 <- raw_citizen %>% # A17-2. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_2 = mean(A17_2))

ggplot(A17_2_data2) + # A17-2. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-2. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과할 경우 지역의 문제라고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_2_data3 <- raw_citizen %>% # A17-2. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_2 = mean(A17_2))

ggplot(A17_2_data3) + # A17-2. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-2. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과할 경우 지역의 문제라고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_2_data4 <- raw_citizen %>% # A17-2. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_2 = mean(A17_2))

ggplot(A17_2_data4) + # A17-2. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_2, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_2,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-2. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과할 경우 지역의 문제라고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_3_data <- raw_citizen %>% # A17-3. 데이터 전처리
  select(A17_3)
A17_3_matrix <- as.matrix(A17_3_data)
A17_3_percent <- prop.table(table(A17_3_matrix)) * 100
A17_3_percent <- round(A17_3_percent, 1)
A17_3_df <- as.data.frame(A17_3_percent)

ggplot(A17_3_df) + # A17-3. 시각화
  aes(x = A17_3_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-3. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과한다면 매우 심각한 문제로 간주할 것이다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_3_data2 <- raw_citizen %>% # A17-3. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_3 = mean(A17_3))

ggplot(A17_3_data2) + # A17-3. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-3. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과한다면 매우 심각한 문제로 간주할 것이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_3_data3 <- raw_citizen %>% # A17-3. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_3 = mean(A17_3))

ggplot(A17_3_data3) + # A17-3. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-3. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과한다면 매우 심각한 문제로 간주할 것이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_3_data4 <- raw_citizen %>% # A17-3. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_3 = mean(A17_3))

ggplot(A17_3_data4) + # A17-3. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_3, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_3,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-3. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 우리지역을 경과한다면 매우 심각한 문제로 간주할 것이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_4_data <- raw_citizen %>% # A17-4. 데이터 전처리
  select(A17_4)
A17_4_matrix <- as.matrix(A17_4_data)
A17_4_percent <- prop.table(table(A17_4_matrix)) * 100
A17_4_percent <- round(A17_4_percent, 1)
A17_4_df <- as.data.frame(A17_4_percent)

ggplot(A17_4_df) + # A17-4. 시각화
  aes(x = A17_4_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-4. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로의 경과지선정 과정에서 주민참여 및 역할이 충분히 보장되어야 한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_4_data2 <- raw_citizen %>% # A17-4. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_4 = mean(A17_4))

ggplot(A17_4_data2) + # A17-4. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-4. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로의 경과지선정 과정에서 주민참여 및 역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_4_data3 <- raw_citizen %>% # A17-4. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_4 = mean(A17_4))

ggplot(A17_4_data3) + # A17-4. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-4. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로의 경과지선정 과정에서 주민참여 및 역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_4_data4 <- raw_citizen %>% # A17-4. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_4 = mean(A17_4))

ggplot(A17_4_data4) + # A17-4. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_4, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_4,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-4. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로의 경과지선정 과정에서 주민참여 및 역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_5_data <- raw_citizen %>% # A17-5. 데이터 전처리
  select(A17_5)
A17_5_matrix <- as.matrix(A17_5_data)
A17_5_percent <- prop.table(table(A17_5_matrix)) * 100
A17_5_percent <- round(A17_5_percent, 1)
A17_5_df <- as.data.frame(A17_5_percent)

ggplot(A17_5_df) + # A17-5. 시각화
  aes(x = A17_5_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-5. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의 경과지\n주민지원을 위한 의사결정과정에 주민의 참여/역할이 충분히 보장되어야 한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_5_data2 <- raw_citizen %>% # A17-5. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_5 = mean(A17_5))

ggplot(A17_5_data2) + # A17-5. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-5. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의 경과지\n주민지원을 위한 의사결정과정에 주민의 참여/역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_5_data3 <- raw_citizen %>% # A17-5. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_5 = mean(A17_5))

ggplot(A17_5_data3) + # A17-5. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-5. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의 경과지\n주민지원을 위한 의사결정과정에 주민의 참여/역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_5_data4 <- raw_citizen %>% # A17-5. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_5 = mean(A17_5))

ggplot(A17_5_data4) + # A17-5. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_5, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_5,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-5. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의 경과지\n주민지원을 위한 의사결정과정에 주민의 참여/역할이 충분히 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_6_data <- raw_citizen %>% # A17-6. 데이터 전처리
  select(A17_6)
A17_6_matrix <- as.matrix(A17_6_data)
A17_6_percent <- prop.table(table(A17_6_matrix)) * 100
A17_6_percent <- round(A17_6_percent, 1)
A17_6_df <- as.data.frame(A17_6_percent)

ggplot(A17_6_df) + # A17-6. 시각화
  aes(x = A17_6_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-6. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로\n 안전점검 과정에 주요 관련자의 참여가 보장되어야 한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_6_data2 <- raw_citizen %>% # A17-6. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_6 = mean(A17_6))

ggplot(A17_6_data2) + # A17-6. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-6. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로\n 안전점검 과정에 주요 관련자의 참여가 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_6_data3 <- raw_citizen %>% # A17-6. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_6 = mean(A17_6))

ggplot(A17_6_data3) + # A17-6. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-6. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로\n 안전점검 과정에 주요 관련자의 참여가 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_6_data4 <- raw_citizen %>% # A17-6. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_6 = mean(A17_6))

ggplot(A17_6_data4) + # A17-6. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_6, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_6,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-6. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로\n 안전점검 과정에 주요 관련자의 참여가 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_7_data <- raw_citizen %>% # A17-7. 데이터 전처리
  select(A17_7)
A17_7_matrix <- as.matrix(A17_7_data)
A17_7_percent <- prop.table(table(A17_7_matrix)) * 100
A17_7_percent <- round(A17_7_percent, 1)
A17_7_df <- as.data.frame(A17_7_percent)

ggplot(A17_7_df) + # A17-6. 시각화
  aes(x = A17_7_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-7. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로는\n건강에 위협을 느낄정도로 매우 위험한 시설이다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_7_data2 <- raw_citizen %>% # A17-7. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_7 = mean(A17_7))

ggplot(A17_7_data2) + # A17-7. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_7, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_7,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-7. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로는\n건강에 위협을 느낄정도로 매우 위험한 시설이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_7_data3 <- raw_citizen %>% # A17-7. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_7 = mean(A17_7))

ggplot(A17_7_data3) + # A17-7. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_7, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_7,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-7. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로는\n건강에 위협을 느낄정도로 매우 위험한 시설이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_7_data4 <- raw_citizen %>% # A17-7. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_7 = mean(A17_7))

ggplot(A17_7_data4) + # A17-7. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_7, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_7,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-7. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로\n 안전점검 과정에 주요 관련자의 참여가 보장되어야 한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_8_data <- raw_citizen %>% # A17-8. 데이터 전처리
  select(A17_8)
A17_8_matrix <- as.matrix(A17_8_data)
A17_8_percent <- prop.table(table(A17_8_matrix)) * 100
A17_8_percent <- round(A17_8_percent, 1)
A17_8_df <- as.data.frame(A17_8_percent)

ggplot(A17_8_df) + # A17-8. 시각화
  aes(x = A17_8_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-8. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의\n피해는 빨리 드러나지 않고 언제 증상이 나타날지도 모른다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_8_data2 <- raw_citizen %>% # A17-8. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_8 = mean(A17_8))

ggplot(A17_8_data2) + # A17-8. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_8, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_8,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-8. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의\n피해는 빨리 드러나지 않고 언제 증상이 나타날지도 모른다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_8_data3 <- raw_citizen %>% # A17-8. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_8 = mean(A17_8))

ggplot(A17_8_data3) + # A17-8. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_8, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_8,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-8. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의\n피해는 빨리 드러나지 않고 언제 증상이 나타날지도 모른다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_8_data4 <- raw_citizen %>% # A17-8. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_8 = mean(A17_8))

ggplot(A17_8_data4) + # A17-8. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_8, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_8,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-8. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑,송전선로의\n피해는 빨리 드러나지 않고 언제 증상이 나타날지도 모른다..", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_9_data <- raw_citizen %>% # A17-9. 데이터 전처리
  select(A17_9)
A17_9_matrix <- as.matrix(A17_9_data)
A17_9_percent <- prop.table(table(A17_9_matrix)) * 100
A17_9_percent <- round(A17_9_percent, 1)
A17_9_df <- as.data.frame(A17_9_percent)

ggplot(A17_9_df) + # A17-8. 시각화
  aes(x = A17_9_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-9. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 도시개발이 지연된다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_9_data2 <- raw_citizen %>% # A17-9. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_9 = mean(A17_9))

ggplot(A17_9_data2) + # A17-9. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-9. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 도시개발이 지연된다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_9_data3 <- raw_citizen %>% # A17-9. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_9 = mean(A17_9))

ggplot(A17_9_data3) + # A17-9. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-9. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 도시개발이 지연된다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_9_data4 <- raw_citizen %>% # A17-9. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_9 = mean(A17_9))

ggplot(A17_9_data4) + # A17-9. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_9, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_9,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-9. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 도시개발이 지연된다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_10_data <- raw_citizen %>% # A17-10. 데이터 전처리
  select(A17_10)
A17_10_matrix <- as.matrix(A17_10_data)
A17_10_percent <- prop.table(table(A17_10_matrix)) * 100
A17_10_percent <- round(A17_10_percent, 1)
A17_10_df <- as.data.frame(A17_10_percent)

ggplot(A17_10_df) + # A17-10. 시각화
  aes(x = A17_10_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-10. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 주택과 토지 가격이 하락한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_10_data2 <- raw_citizen %>% # A17-10. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_10 = mean(A17_10))

ggplot(A17_10_data2) + # A17-10. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_10, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_10,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-10. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 주택과 토지 가격이 하락한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_10_data3 <- raw_citizen %>% # A17-10. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_10 = mean(A17_10))

ggplot(A17_10_data3) + # A17-10. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_10, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_10,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-10. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 주택과 토지 가격이 하락한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_10_data4 <- raw_citizen %>% # A17-10. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_10 = mean(A17_10))

ggplot(A17_10_data4) + # A17-10. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_10, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_10,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-10. (송전탑 입지결정 및 지상송전탑 지중화 관련)\n송전탑,송전선로가 도심을 경과하면 주택과 토지 가격이 하락한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_11_data <- raw_citizen %>% # A17-11. 데이터 전처리
  select(A17_11)
A17_11_matrix <- as.matrix(A17_11_data)
A17_11_percent <- prop.table(table(A17_11_matrix)) * 100
A17_11_percent <- round(A17_11_percent, 1)
A17_11_df <- as.data.frame(A17_11_percent)

ggplot(A17_11_df) + # A17-11. 시각화
  aes(x = A17_11_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-11. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가적으로 에너지를\n안정적으로 공급하기 위해서 송전탑과 송전선로 건설은 반드시 필요하다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_11_data2 <- raw_citizen %>% # A17-11. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_11 = mean(A17_11))

ggplot(A17_11_data2) + # A17-11. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_11, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_11,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-11. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가적으로 에너지를\n안정적으로 공급하기 위해서 송전탑과 송전선로 건설은 반드시 필요하다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_11_data3 <- raw_citizen %>% # A17-11. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_11 = mean(A17_11))

ggplot(A17_11_data3) + # A17-11. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_11, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_11,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-11. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가적으로 에너지를\n안정적으로 공급하기 위해서 송전탑과 송전선로 건설은 반드시 필요하다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_11_data4 <- raw_citizen %>% # A17-11. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_11 = mean(A17_11))

ggplot(A17_11_data4) + # A17-11. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_11, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_11,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-11. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가적으로 에너지를\n안정적으로 공급하기 위해서 송전탑과 송전선로 건설은 반드시 필요하다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_12_data <- raw_citizen %>% # A17-12. 데이터 전처리
  select(A17_12)
A17_12_matrix <- as.matrix(A17_12_data)
A17_12_percent <- prop.table(table(A17_12_matrix)) * 100
A17_12_percent <- round(A17_12_percent, 1)
A17_12_df <- as.data.frame(A17_12_percent)

ggplot(A17_12_df) + # A17-12. 시각화
  aes(x = A17_12_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-12. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가 에너지계획에 의해\n필요하다면 우리지역에 송전탑과 송전선로 건설에 찬성한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_12_data2 <- raw_citizen %>% # A17-12. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_12 = mean(A17_12))

ggplot(A17_12_data2) + # A17-12. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-12. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가 에너지계획에 의해\n필요하다면 우리지역에 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_12_data3 <- raw_citizen %>% # A17-12. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_12 = mean(A17_12))

ggplot(A17_12_data3) + # A17-12. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-12. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가 에너지계획에 의해\n필요하다면 우리지역에 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_12_data4 <- raw_citizen %>% # A17-12. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_12 = mean(A17_12))

ggplot(A17_12_data4) + # A17-12. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_12, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_12,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-12. (송전탑 입지결정 및 지상송전탑 지중화 관련)국가 에너지계획에 의해\n필요하다면 우리지역에 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_13_data <- raw_citizen %>% # A17-13. 데이터 전처리
  select(A17_13)
A17_13_matrix <- as.matrix(A17_13_data)
A17_13_percent <- prop.table(table(A17_13_matrix)) * 100
A17_13_percent <- round(A17_13_percent, 1)
A17_13_df <- as.data.frame(A17_13_percent)

ggplot(A17_13_df) + # A17-13. 시각화
  aes(x = A17_13_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-13. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑 건립의 반대급부로\n지역 내 경제적 편익이 주어진다면, 우리지역 송전탑과 송전선로 건설에 찬성한다", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_13_data2 <- raw_citizen %>% # A17-13. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_13 = mean(A17_13))

ggplot(A17_13_data2) + # A17-13. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_13, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_13,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-13. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑 건립의 반대급부로\n지역 내 경제적 편익이 주어진다면, 우리지역 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_13_data3 <- raw_citizen %>% # A17-13. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_13 = mean(A17_13))

ggplot(A17_13_data3) + # A17-13. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_13, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_13,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-13. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑 건립의 반대급부로\n지역 내 경제적 편익이 주어진다면, 우리지역 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_13_data4 <- raw_citizen %>% # A17-13. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_13 = mean(A17_13))

ggplot(A17_13_data4) + # A17-13. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_13, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_13,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-13. (송전탑 입지결정 및 지상송전탑 지중화 관련)송전탑 건립의 반대급부로\n지역 내 경제적 편익이 주어진다면, 우리지역 송전탑과 송전선로 건설에 찬성한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))


A17_14_data <- raw_citizen %>% # A17-14. 데이터 전처리
  select(A17_14)
A17_14_matrix <- as.matrix(A17_14_data)
A17_14_percent <- prop.table(table(A17_14_matrix)) * 100
A17_14_percent <- round(A17_14_percent, 1)
A17_14_df <- as.data.frame(A17_14_percent)

ggplot(A17_14_df) + # A17-14. 시각화
  aes(x = A17_14_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-14. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서는 기꺼이 지역주민들이 공사비용의 일부를 지불해야 한다고 생각한다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_14_data2 <- raw_citizen %>% # A17-14. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_14 = mean(A17_14))

ggplot(A17_14_data2) + # A17-14. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_14, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_14,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-14. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서는 기꺼이 지역주민들이 공사비용의 일부를 지불해야 한다고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_14_data3 <- raw_citizen %>% # A17-14. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_14 = mean(A17_14))

ggplot(A17_14_data3) + # A17-14. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_14, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_14,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-14. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서는 기꺼이 지역주민들이 공사비용의 일부를 지불해야 한다고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_14_data4 <- raw_citizen %>% # A17-14. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_14 = mean(A17_14))

ggplot(A17_14_data4) + # A17-14. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_14, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_14,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-14. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서는 기꺼이 지역주민들이 공사비용의 일부를 지불해야 한다고 생각한다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))



A17_15_data <- raw_citizen %>% # A17-15. 데이터 전처리
  select(A17_15)
A17_15_matrix <- as.matrix(A17_15_data)
A17_15_percent <- prop.table(table(A17_15_matrix)) * 100
A17_15_percent <- round(A17_15_percent, 1)
A17_15_df <- as.data.frame(A17_15_percent)

ggplot(A17_15_df) + # A17-14. 시각화
  aes(x = A17_15_matrix, weight = Freq) +
  geom_bar(fill = "#4682B4") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.7)) +
  labs(x = "응답구분", 
       y = "응답비율", title = "A17-15. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서 나는 기꺼이 공사비용의 일부를 추가 전기요금으로 지불할 것이다.", subtitle = "(시흥시민 364명 대상)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_15_data2 <- raw_citizen %>% # A17-15. 5점 척도 성별 데이터 전처리
  group_by(SQ1, A1) %>% 
  summarise(mean_A17_15 = mean(A17_15))

ggplot(A17_15_data2) + # A17-15. 5점 척도 성별 시각화
  aes(x = SQ1, y = mean_A17_15, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_15,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-15. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서 나는 기꺼이 공사비용의 일부를 추가 전기요금으로 지불할 것이다.") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_15_data3 <- raw_citizen %>% # A17-15. 5점 척도 연령별 데이터 전처리
  group_by(SQ2_2, A1) %>% 
  summarise(mean_A17_15 = mean(A17_15))

ggplot(A17_15_data3) + # A17-15. 5점 척도 연령별 시각화
  aes(x = SQ2_2, y = mean_A17_15, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_15,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-15. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서 나는 기꺼이 공사비용의 일부를 추가 전기요금으로 지불할 것이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))

A17_15_data4 <- raw_citizen %>% # A17-15. 5점 척도 소득별 데이터 전처리
  group_by(DQ8_1, A1) %>% 
  summarise(mean_A17_15 = mean(A17_15))

ggplot(A17_15_data4) + # A17-15. 5점 척도 소득별 시각화
  aes(x = DQ8_1, y = mean_A17_15, fill = A1) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(mean_A17_15,2), vjust = -0.7), position = position_dodge(1)) +
  labs(x = "응답구분", 
       y = "5점 척도", title = "A17-15. (송전탑 입지결정 및 지상송전탑 지중화 관련)우리 지역의 송전탑,송전선로를\n지중화하기 위해서 나는 기꺼이 공사비용의 일부를 추가 전기요금으로 지불할 것이다.", subtitle = "시흥시민 364명 대상") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, 
                                  face = "bold", hjust = 0.5), plot.subtitle = element_text(size = 12L, hjust = 0.5), axis.title.y = element_text(size = 15L, 
                                                                                                                                                  face = "bold"), axis.title.x = element_text(size = 15L, face = "bold"))
