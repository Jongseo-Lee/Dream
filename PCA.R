library(ggplot2)
library(reshape2)

# A_16 데이터 전처리
rawdata <- read_spss("Rawdata.sav")
rawdata_A16 <- rawdata %>% 
  select(A16_1:A16_6)
head(rawdata_A16)

# A_16 PCA분석
pca_rawdata_A16 <- prcomp(rawdata_A16, scale = TRUE)
summary(pca_rawdata_A16)
biplot(pca_rawdata_A16)

# A_16 상관관계 시각화
rawdata_A16_cor <- round(cor(rawdata_A16),2)
melt_A16 <- melt(rawdata_A16_cor)
ggplot(melt_A16, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="white", high="red",
                       space = "Lab") +
  geom_text(aes(label=value), color = "black", size=4)

# A_17 데이터 전처리
rawdata <- read_spss("Rawdata.sav")
rawdata_A17 <- rawdata %>% 
  select(A17_1:A17_15)
head(rawdata_A17)

# A_17 PCA분석
pca_rawdata_A17 <- prcomp(rawdata_A17, scale = TRUE)
summary(pca_rawdata_A17)
biplot(pca_rawdata_A17)

# A_17 상관관계 시각화
rawdata_A17_cor <- round(cor(rawdata_A17),2)
melt_A17 <- melt(rawdata_A17_cor)
ggplot(melt_A17, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="white", high="red",
                       space = "Lab") +
  geom_text(aes(label=value), color = "black", size=4)
