## Script to run all steps in order

## Empty temp folders
temp_dir <- "~/R project/Code replication/build/temp/"
setwd(temp_dir)
file.remove(file.path(temp_dir), list.files(temp_dir))

temp_dir <- "~/R project/Code replication/analysis/temp/"
setwd(temp_dir)
file.remove(file.path(temp_dir), list.files(temp_dir))

## Run scripts in build part
fileDir <- "~/R project/Code replication/build/code/"
fileList <- c("data_filter.R",
              "data_merge.R",
              "data_treatment.R",
              "data_split.R",
              "data_lag.R")
filePath <- paste0(fileDir,fileList)
lapply(filePath, source)

## Run scripts in analysis part
fileDir <- "~/R project/Code replication/analysis/code/"
fileList <- c()
filePath <- paste0(fileDir,fileList)
lapply(filePath, source)