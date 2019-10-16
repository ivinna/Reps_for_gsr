library(dplyr)

# поиск по значению
sportsdesc %>% 
  filter(value =="ps")

sportsdesc %>% 
  filter(desc =="художественная гимнастика")

sportsdesc %>% 
  filter(value =="ps")

#находим номер строки с определённым значением 
which(sportsdesc$value=="ps")
which(sportsdesc$value=="test")
which(sportsdesc$value=="artgym")

#замена описания 
sportsdesc$desc[37] <- "парусный спорт"

#создаём новую табличку из одной строки с описанием

add<-data.frame("stour", "спортивный туризм")

add<-data.frame("hfigth", "рукопашный бой")

add<-data.frame("rugby", "регби")

# присваиваем имена как в основной таблице
names(add)<-c("value","desc")

# добавляем новую строку в таблицу
sportsdesc <- rbind(sportsdesc, add)

# обновляем файл
save(sportsdesc, file=paste0(getwd(), '/data/sportsdesc.Rdata'))


