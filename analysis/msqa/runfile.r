#run tool
root <- 'C:/Users/HP USER/Desktop/Final/data/'
dirs <- list.files(root)
len <- NROW(dirs)
setwd(root)
setwd("code")
source("func.r")
setwd("..")
for (i in c(1:len))
{
  if (dirs[i] != "code")
  {
    newdir <- paste(root, dirs[i], sep = "")
    setwd(newdir)
    source("classify.r")
    sink()
    source("eval.r")
    setwd("..")
  }
}