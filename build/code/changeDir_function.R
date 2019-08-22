## Function changeDir to switch working directory

changeDir <- function(path_dir){
  # Create temp direction if not exists
  if (!file.exists(path_dir)) {
    dir.create(path_dir)
  }
  # Switch working directory to temp folder
  setwd(path_dir)
}