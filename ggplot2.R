library(dplyr)
library(haven)
library(ggplot2)
library(scales)
library(RColorBrewer)

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

raw_data <- raw_citizen %>% 
  group_by(SQ1) %>% 
  summarise("응답비율" = mean(A1))

raw_data
A1_data1 <- raw_citizen %>% # A1. 성별 구분
  group_by(A1) %>% 
  select(SQ1)
t(table(A1_data1))

# 시각화
A1_1_plot <- ggplot(A1_1_df, aes(x=SQ1, y=Freq, fill=A1)) +
  geom_bar(stat="identity", position = "dodge2")

# 데이터 축
A1_1_plot + ggtitle("A1. 귀하의 거주지 주변에 송전탑 및 송전선로가\n 존재합니까? (육안으로 볼 수 있는 거리)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 18)) +
  theme(axis.title = element_text(face = "bold", hjust = 0.5, size = 15)) +
  labs(x="성별구분", y="응답비율", fill = "응답구분") +
  scale_y_continuous(breaks = c(20,40,60,80),
                     labels = c("20%","40%","60%","80%")) +
  
  geom_text(aes(label = Freq), vjust = -0.5,
            position = position_dodge(.9), size = 4.5) +
  
  # 범례
  theme(legend.title = element_text(face = "bold", size = 15)) +
  theme(legend.text = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.9,0.9))
