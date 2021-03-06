library(dplyr)
library(haven)
library(psych)
library(GPArotation)

df <- read_spss("Rawdata.sav")
df_A16 <- df %>% 
  select(A16_1:A16_6)
head(df_A16)

pc1 <- prcomp(df_A16)
summary(pc1)
pc1$rotation
pc1$sdev^2

screeplot(pc1, type = "l")

factor_A16_1 <- factanal(df_A16, factors = 3, rotation = "varimax")
factor_A16_1$loadings
factor_A16_2 <- factanal(df_A16, factors = 3, rotation = "oblimin")
factor_A16_2$loadings


df_A17 <- df %>% 
  select(A17_1:A17_10)
head(df_A17)

pc2 <- prcomp(df_A17)
pc2
summary(pc2)
round(pc2$rotation,3)
round(pc2$sdev^2,5)
windows()
screeplot(pc2, type = "l")

factor_A17_1 <- factanal(df_A17, factors = 4, rotation = "varimax")
factor_A17_1$loadings
factor_A17_2 <- factanal(df_A17, factors = 4, rotation = "oblimin")
factor_A17_2$loadings

df_A17_2 <- df %>% 
  select(A17_7:A17_16)
head(df_A17_2)

pc2_1 <- prcomp(df_A17_2)
summary(pc2_1)
round(pc2_1$rotation,5)
pc2_1$sdev^2

screeplot(pc2_1, type = "l")
windows()
factor_A17_3 <- factanal(df_A17_2, factors = 4, rotation = "varimax")
factor_A17_3$loadings
factor_A17_4 <- factanal(df_A17_2, factors = 3, rotation = "oblimin")
factor_A17_4$loadings
