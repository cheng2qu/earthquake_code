## Script to run all steps in order -----

## Empty temp folders -----
temp_dir <- "build/temp/"
file.remove(paste0(temp_dir,list.files(temp_dir)))

temp_dir <- "analysis/temp/"
file.remove(paste0(temp_dir,list.files(temp_dir)))

## Empty output folders -----
temp_dir <- "build/output/"
file.remove(paste0(temp_dir,list.files(temp_dir)))

temp_dir <- "analysis/output/"
file.remove(paste0(temp_dir,list.files(temp_dir)))

## Run scripts in build part -----
fileList <- c("data_filter.R",
              "data_merge.R",
              "data_treatment.R",
              "data_split.R",
              "data_lag.R")
filePath <- paste0("build/code/", fileList)
lapply(filePath, source)

## Run scripts in analysis part -----
fileList <- c("stats_summary.R",
              "model1.R",
              "model2.R",
              "model3.R",
              "model4.R",
              "model5.R")
filePath <- paste0("analysis/code/", fileList)
lapply(filePath, source)