file.rename(list.files(pattern="water_*.img"), paste0("water_", 1:700))

list.files("C:/R/Reps_for_gsr/rawdata/GSR")
getwd()
file.rename(list.files("C:/R/Reps_for_gsr/rawdata/GSR", pattern="*.txt"),
            paste0("var", 1:409))