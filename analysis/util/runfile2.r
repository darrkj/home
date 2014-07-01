#run tool
names <- list("australian.csv", "bang.csv", "bc_wisc.csv", "bc_wisc_c.csv", "bupa.csv", "car.csv", "exam.csv", 
              "german.csv", "haberman.csv", "ionosphere.csv", "magic04.csv","monks1.csv", "monks3.csv", 
              "pima.csv", "saheart.csv", "sonar.csv", "spambase.csv", "spect.csv", "spectf.csv", "transfusion.csv")
root <- 'C:/Users/HP USER/Dropbox/Final/data/'
data <- 'C:/Users/HP USER/Dropbox/Final/data/data/'



size <- NULL
prop <- NULL

len <- NROW(names)
#setwd(data)
for (i in c(1:len))
{
  data <- read.table(names[[i]], header=T, sep=",")
  data <- data[complete.cases(data),]
  size[i] <- dim(data)[1]
  prop[i] <- table(data$y)[2]/size[[i]]
}


load("C:/Users/HP USER/Dropbox/Final/data/data/australian.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/bang.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/bc_wisc_c.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/bc_wisc.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/bupa.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/car.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/exam.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/german.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/haberman.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/ionosphere.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/magic04.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/monks1.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/monks3.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/pima.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/saheart.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/sonar.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/spambase.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/spect.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/spectf.RData")
load("C:/Users/HP USER/Dropbox/Final/data/data/transfusion.RData")

result <- list(australian, bang, bc_wisc, bc_wisc_c, bupa, car, exam, german, haberman,
ionosphere, magic04, monks1, monks3, pima, saheart, sonar, spambase,  spect, spectf, transfusion)

files <- list("austrailian", "bang", "bc_wisc", "bc_wisc_c", "bupa", "car", "exam", "german", "haberman",
"ionosphere", "magic04", "monks1", "monks3", "pima", "saheart", "sonar", "spambase", " spect", "spectf", "transfusion")

rm(australian, bang, bc_wisc, bc_wisc_c, bupa, car, exam, german, haberman, 
ionosphere, magic04, monks1, monks3, pima, saheart, sonar, spambase,  spect, spectf, transfusion)