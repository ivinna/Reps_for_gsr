
copy_and_rename <- function(a, b){#a - папка, из которой копируем
  # удаляем файлы из папки b
  sapply(paste0(b, "/", list.files(b)), unlink)
  # копируем файлы из папки a в папку b
  file.copy(paste0(a, "/", list.files(a)),  b)
    # устанавливаем рабочую директорию
  setwd(b)
  # переименовываем файлы
  file.rename(list.files(pattern="*.txt"),
              paste0("var", 1:length(list.files(pattern="*.txt"))))
  # возвращаем рабочую директорию
  setwd("C:/R/Reps_for_gsr")
}
#Копируем данные КГР
a <- "C:/Users/iivanova/Desktop/spbniifk/Projects/gsr_analysys7/rawdata/GSR"
b <- "C:/R/Reps_for_gsr/rawdata/GSR"

copy_and_rename(a, b)

#Копируем данные самооценки
a <- "C:/Users/iivanova/Desktop/spbniifk/Projects/gsr_analysys7/rawdata/seval"
b <- "C:/R/Reps_for_gsr/rawdata/seval"
