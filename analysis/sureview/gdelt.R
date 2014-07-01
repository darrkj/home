setwd('../..')
xx <- read.delim('20140425.export.CSV', header = F)


yy <- grep('suicide', xx$V58, value = T)

zz <- grep('military', yy, value = T)





for (i in zz) print(i)
