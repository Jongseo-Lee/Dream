library(dplyr)
library(haven)
library(psy)

df <- read_spss("Rawdata.sav")

df2 <- df %>% # 산점도 행렬
  select(A17_1:A17_10)
windows()
pairs(df2, panel = panel.smooth)

df_1 <- df %>% 
  select(A17_1:A17_3)
cronbach(df_1)

df_2 <- df %>% 
  select(A17_4:A17_6)
cronbach(df_2)

df_3 <- df %>% 
  select(A17_7:A17_8)
cronbach(df_3)

df_4 <- df %>% 
  select(A17_9:A17_10)
cronbach(df_4)



