# Merge files, create shared table
# function reads first three elements from txt
scann <- function(x){
  scan(x, what=character(), sep=",", quiet=TRUE, nmax = 3)
}

# Эти функции нужны для создания объекта с сырыми данными всех КГР тестирований в виде списка, rawgsr
#чтение файлов .txt полностью
scann2 <- function(x){
  scan(x, what=character(), sep=",", quiet=TRUE)
} 
#удаляет первые 7 элементов вектора. Можно ли это сделать сразу функцией scan
sn <- function(x) x[-(1:7)]
#расчитывает фон, релаксацию, активацию
count <- function(x){
  q <- 8 #измерений в секунду
  m <- 1 #количество минут
  s <- 10 #количество измерений отрезка
  l <- 2900#длина файла
  f <- 1:(m*q*60)#фон - измерения за минуту исходного уровня (1-я минута)
  a <- (m*q*60-s):(m*q*60)#заключительные 10 измерений фона (1-я минута)
  m <- 4 #количество минут
  b <- (m*q*60-s):(m*q*60)#заключительные 10 секунд релаксации (2-4 минута)
  c <- (m*q*60+1):l#активация - заключительная минута теста (4-5 минута)
  z <- 1/x*1000000
  c(round(mean(z[f], na.rm = T), digits = 2),
    round(100-mean(x[b], na.rm = T)*100/mean(x[a], na.rm = T), digits = 0),
    round(100-min(x[c], na.rm = T)*100/mean(x[b], na.rm = T), digits = 0))
}


find_self_point <- function(x){
  seval <- as.numeric(as.character(df[4:6]))
  real<- vec_int(1/df[33:35]*1000000)
  self<- vec_int(seval)
  comp <- real==self
  ifelse(find_point(comp[1], df$o_fon[n], df$rf_selfws[n])>0, 
         find_point(self[1], df$o_fon[n], df$rf_selfws[n]), 0)
}


# find_self_point <- function(x){
#   seval <- as.numeric(as.character(x[4:6]))
#   fuu <- function(x){as.numeric(1/x*1000)}
#   real <- vec_int(fuu(x[33:35]))
#   self<- vec_int(seval)
#   comp <- real==self
#   ifelse(find_point(comp[1], x[33], x[42])>0, 
#          find_point(self[1], x[33], x[42]), 0)
# }

find_self_point <- function(x){
  seval <- as.numeric(as.character(x[4:6]))
  fuu <- function(x){as.numeric(1/x*1000)}
  real <- vec_int(fuu(x[33:35]))
  self<- vec_int(seval)
  comp <- real==self
  ifelse(find_point(comp[1], x[33], x[42])>0, 
         find_point(comp[1], x[33], x[42]), 0)
}

find_self_point2 <- function(x){
  seval <- as.numeric(as.character(x[4:6]))
  fuu <- function(x){as.numeric(1/x*1000)}
  real <- vec_int(fuu(x[33:35]))
  self<- vec_int(seval)
  comp <- real==self
  ifelse(find_point(comp[1], x[33], x[43])>0, 
         find_point(comp[1], x[33], x[43]), 0)
}


vec_int <- function(v){
  vec <- c()
  vec[1] <- ifelse(v[1]<v[2], T, F)#T релаксация не осуществлена 
  vec[2] <- ifelse(v[1]<v[3], T, F)#T активация от фона не осуществлена
  vec[3] <- ifelse(v[2]<v[3], T, F)#T активация от релаксации осуществлена
  vec
}


find_point <- function(a, b, c){
  ifelse(a==T, b-b*c/100, b+b*c/100)
}

find_point2 <- function(a, b, c){
  ifelse(a==T, b+b*c/100, b-b*c/100)
}

renew2 <- function(){
  #files <- paste0("rawdata/GSR/", list.files("rawdata/GSR", pattern="*.txt"))
  #files <- paste0("../rawdata/GSR/", list.files("rawdata/GSR"))
  files <- paste0(getwd(),"/rawdata/GSR/",  list.files("rawdata/GSR"))
  
  dat <- lapply(files, scann)
  dat <- data.frame(do.call(rbind, dat))
  dat$X1 <- gsub("[#User: ", "", dat$X1, fixed=TRUE)
  dat$X2 <- gsub(" #myDate: ", "", dat$X2, fixed=TRUE)
  dat$X3 <- gsub(" #myTime: ", "", dat$X3, fixed=TRUE)
  names(dat) <- c("id", "date", "time")
  ath <- read.xlsx("data/athletes.xlsx", sheetIndex = 1, encoding = "UTF-8")
  df <- inner_join(dat, ath, by = "id")
  df$N<- 1:nrow(df)
  
  #####
  rawgsr <- lapply(files, scann2)%>%
    lapply(sn)%>%
    lapply(as.numeric)
  gsr <- lapply(rawgsr, count)
  gsr <- data.frame(do.call(rbind, gsr))
  names(gsr) <- c("fon", "rel", "act")
  df <- cbind(df, gsr)
  #####добавим отрезки для расчёта
  persec <- 8 #измерений в секунду
  min_fon <- 1 #количество минут фона
  min_rel <- 4 #количество минут до начала активации
  s <- 10 #количество измерений отрезка
  index1 <- min_fon*persec*60#расчитываем, сколько измерений в нужном нам количестве минут (480)
  index2 <- min_rel*persec*60#расчитываем, сколько измерений в нужном нам количестве минут (1920)
  fon_ind <- (index1-s):index1#10 измерений до начала релаксации
  rel_ind <- (index2-s):index2#10 измерений до конца релаксации
  act_ind <- (index2+1):length(gsr)#отрезок от начала активации до конца теста
  fo_fon <-function(x) { mean(x[fon_ind])}
  fo_rel <- function(x) {mean(x[rel_ind])}
  #fo_act <- function(x) {min(x[(index2+1):length(x)])}
  fo_act <- function(x) {
    x <- x[-length(x)]
    k <- length(x)
    min(x[(index2+1):k])}
  
  o_fon <- lapply(rawgsr, fo_fon)
  o_fon <- data.frame(do.call(rbind, o_fon))
  
  o_rel <- lapply(rawgsr, fo_rel)
  o_rel <- data.frame(do.call(rbind, o_rel))
  
  o_act <- lapply(rawgsr, fo_act)
  o_act <- data.frame(do.call(rbind, o_act))
  df <- cbind(df, o_fon, o_rel, o_act)
  names(df)[26:28] <- c("o_fon", "o_rel", "o_act")
  #####
  getwd()
  files <- paste0(getwd(),  "/rawdata/seval/", list.files(paste0(getwd(),  "/rawdata/seval")))
  seval <- lapply(files, scann2)
  seval <- data.frame(do.call(rbind, seval))
  seval$X1 <- gsub("[#User: ", "", seval$X1, fixed=TRUE)
  seval$X2 <- gsub(" #myDate: ", "", seval$X2, fixed=TRUE)
  seval$X3 <- gsub(" #myTime: ", "", seval$X3, fixed=TRUE)
  names(seval) <- c("id", "date", "time", "sfon", "srel", "sact")
  seval$sev <- paste0(seval$id, seval$date, seval$time)
  df$sev <- paste0(df$id, df$date, df$time)
  df <- full_join(seval, df, by = "sev")
  #####
  save(df, file=paste0(getwd(), '/data/df.Rdata'))
  save(rawgsr, file=paste0(getwd(), '/data/rawgsr.Rdata'))
}

#здесь собраны все функции и их описание
find_diff <- function(v){
  ifelse(sign(v[1])==sign(v[2]),
         ifelse(abs(v[1])>abs(v[2]),abs(v[1])-abs(v[2]), abs(v[2])-abs(v[1])),
         sum(abs(v[1]), abs(v[2])))
}
perc2 <- function(vec){round(vec[2]*100/vec[1], digits = 2)}#не нужна
# выясняет, какое значение больше и находит разницу
fdiff <- function(v){
  ifelse(abs(v[1])>abs(v[2]),abs(v[1])-abs(v[2]), abs(v[2])-abs(v[1]))
}
# находит разницу в процентах от первого значения, например, сколько процентов релаксация составляет от фона
# принимает на вход вектор из двух чисел
# 1. находит разницу между двумя значениями
# 2. находит, сколько эта разница составляет в процентах от первого числа
fd <- function(v){#первое значение - фон, относительно которого оценивается
  round(fdiff(v)*100/abs(v[1]), digits = 2)
}
# находит разницу между 100% и  разницей в процентах между 
# первым и вторым числом
fd2 <- function(v){round(100-v[2]*100/v[1], digits=2)}
#функция применяется к векторам self и real
vec_int <- function(v){
  vec <- c()
  vec[1] <- ifelse(v[1]<v[2], T, F)#T релаксация не осуществлена 
  vec[2] <- ifelse(v[1]<v[3], T, F)#T активация от фона не осуществлена
  vec[3] <- ifelse(v[2]<v[3], T, F)#T активация от релаксации осуществлена
  vec
}


add_perc <- function(x, y){
  x+x*y/100
}
