## Script to run all steps in order

## Empty temp folders
temp_dir <- c("~/R project/Code replication/build/temp/",
              "~/R project/Code replication/analysis/temp/")
temp <- lapply(temp_dir, function(x) 
  if (file.exists(x)){
    file.remove(file.path(x), list.files(x))
    }
  )

## Run scripts in build part
fileDir <- "~/R project/Code replication/build/code/"
fileList <- c("data_filter.R",
              "data_merge.R",
              "data_treatment.R",
              "data_split.R",
              "data_lag.R")
filePath <- paste0(fileDir,fileList)
temp <- lapply(filePath, source)

## Run scripts in analysis part
fileDir <- "~/R project/Code replication/analysis/code/"
fileList <- c()
filePath <- paste0(fileDir,fileList)
temp <- lapply(filePath, source)