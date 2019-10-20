library(dplyr)
tail(df)
df %>% 
  slice(423)

newline <- df %>% 
  slice(423)
newline[1,12]
ncol(newline)
c(NA, NA, NA, NA, NA, NA, NA, NA, "1bclimb", "19.10.2019", "16:19", 424, 
  Тихвинская, Евгения,)