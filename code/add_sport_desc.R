library(dplyr)
sportsdesc %>% 
  filter(value =="ps")

sportsdesc %>% 
  filter(desc =="художественная гимнастика")

sportsdesc %>% 
  filter(value =="ps")

which(sportsdesc$value=="ps")

sportsdesc$desc[37] <- "парусный спорт"

which(sportsdesc$value=="artgym")
sportsdesc$desc[24] <- "художественная гимнастика"

add <- matrix(c("spgym", "спортивная гимнастика"), byrow = F)
sportsdesc$desc[43] <- "спортивная гимнастика"

add<-data.frame("stour", "спортивный туризм")
names(add)<-c("value","desc")

add<-data.frame("hfigth", "рукопашный бой")
names(add)<-c("value","desc")

add<-data.frame("test", "test")
names(add)<-c("value","desc")

sportsdesc <- rbind(sportsdesc, add)

save(sportsdesc, file=paste0(getwd(), '/data/sportsdesc.Rdata'))
