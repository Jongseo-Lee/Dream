library(dplyr) 
library(haven)
library(ggplot2)
library(scales)
library(RColorBrewer)

rawdata <- read_spss("Rawdata.sav")
table(is.na(rawdata))
raw_citizen <- rawdata %>% # 시흥시민 364명 필터링
  filter(Gubun_Area == 1) 

raw_citizen$A1 <- ifelse(raw_citizen$A1 == 1, "네", "아니오") # 송전탑 가시거리 거주별

View(raw_citizen)

A1_data <- raw_citizen %>% # A1. 성별 구분
  select(A1)
A1_matrix <- as.matrix(A1_data)
A1_percent <- prop.table(table(a1)) * 100
A1_percent <- round(A1_percent, 1)
A1_percent
A1_df <- as.data.frame(A1_percent)
A1_df

plot <- ggplot(data = A1_df, aes(x=a1, y=Freq)) +
  geom_bar(stat = "identity", width=0.5, fill="steelblue") +
  geom_text(aes(label=Freq), vjust=-1.5, size=4.0) +
  xlab("응답구분") + ylab("응답비율") +
  theme_minimal()

plot + ggtitle("A1. 귀하의 거주지 주변에 송전탑 및 송전선로가\n 존재합니까? (육안으로 볼 수 있는 거리)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18)) +
  theme(axis.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x="성별구분", y="응답비율", fill = "응답구분") +
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c("20%","40%","60%","80%")) +
  
  geom_text(aes(label = Freq), vjust = -0.5,
            position = position_dodge(.9), size = 4.5) +
  
  theme(legend.title = element_text(face = "bold", size = 15)) +
  theme(legend.text = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.9,0.9))
plot

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
