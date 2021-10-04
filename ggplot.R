library(dplyr)
library(haven)
library(ggplot2)

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
A1_data1$A1 <- ifelse(A1_data1$A1 == 1, "네", "아니오")
table(A1_data1)
A2_matrix <- as.matrix(t(table(A1_data1))) # 데이터 행렬화
A2_percent <- prop.table(A2_matrix,1) * 100 # 백분율 산정
A2_percent <- round(A2_percent, 1)
A2_percent
A2_df <- as.data.frame(A2_percent) # 데이터프레임화

A2_plot <- ggplot(A2_df, aes(x=SQ1, y=Freq, fill=A1)) +
  geom_bar(stat="identity", position = "dodge2") # 시각화
A2_plot + ggtitle("A1. 귀하의 거주지 주변에 송전탑 및 송전선로가\n 존재합니까? (육안으로 볼 수 있는 거리)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20)) +
  theme(axis.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x="성별구분", y="응답비율", fill = "응답구분") +
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c("20%","40%","60%","80%")) +
  geom_text(aes(label = Freq), vjust = -0.5,
            position = position_dodge(.9), size = 4.5) +
  theme(legend.title = element_text(face = "bold", size = 15)) +
  theme(legend.text = element_text(face = "bold", size = 11, color = "#330066")) +
  theme(legend.position = c(0.9,0.9)) +
  scale_color_continuous(name = "성별구분")
  
  


