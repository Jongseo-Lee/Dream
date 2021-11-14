library(poLCA)
library(vcd)
library(dplyr)
library(haven)

df1_1 <- result_1$A17_1
df1_2 <- result_1$A17_2
df1_3 <- result_1$A17_3
bind1 <- as.data.frame(cbind(df1_1, df1_2, df1_3))
r1 <- stack(bind1)

df2_1 <- result_2$A17_4
df2_2 <- result_2$A17_5
df2_3 <- result_2$A17_6
bind2 <- as.data.frame(cbind(df2_1, df2_2, df2_3))
r2 <- stack(bind2)

df3_1 <- result_3$A17_7
df3_2 <- result_3$A17_8
bind3 <- as.data.frame(cbind(df3_1, df3_2))
r3 <- stack(bind3)

df4_1 <- result_4$A17_9
df4_2 <- result_4$A17_10
bind4 <- as.data.frame(cbind(df4_1, df4_2))
r4 <- stack(bind4)

r1
r2
r3
r4

df2 <- cbind(r1, r2, r3, r4)




