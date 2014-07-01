options(stringsAsFactors = FALSE)
library(httr)
library(RCurl)


base <- 'ftp://ftp.sec.gov/edgar/daily-index'
# year <- 1994:2014

ext <- expand.grid(1995:2013, c('QTR1', 'QTR2', 'QTR3', 'QTR4'))
ext <- paste(ext[, 1], ext[, 2], sep = '/')

query <- paste(base, ext, '', sep = '/')


files <- list()
j <- 1
for (i in query) {
  xx <- unlist(strsplit(as.character(getURL(url = i)), split = '\n'))
  x <- unlist(lapply(strsplit(xx, split = ' '), tail, 1))
  files[[i]] <- data.frame(a = i, b = x)
  Sys.sleep(5)
  j <- j + 1
  print(j)
}



y <- recurBind(files)[[1]]

z <- paste(y$a, y$b, sep = '')


# company crawler    form   index  master sitemap

forms <- grep('company', z, value = T)

idx <- grep('idx', forms, value = T)






files <- list()
j <- 1
for (i in idx) {
  files[[i]] <- tryCatch(clean(idx[1]), error = function(cond) NA)
  
  Sys.sleep(15)
  j <- j + 1
  print(j)
}

z <- recurBind(files)[[1]]








recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- rbind(dList[[(i * 2) - 1]], dList[[i * 2]])
    j <- j + 1
  }
  # In case length was odd, just add last set to the end.
  if (floor(len) != len) {
    data[[j]] <- dList[[len * 2]]
  }
  # Less data to store on the stack, tail call optimization would be nice here.
  # Try removing this and check out the time diff.
  rm(dList, len, j)
  # Recursive call.
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}




clean <- function(site) {
  data <- unlist(strsplit(as.character(getURL(url = site)), split = '\n'))
  ind <- grep('-----------------------', data)
  datas <- data[(ind+1):length(data)]
  
  x1 <- substr(datas, 1, 62)
  x2 <- substr(datas, 63, 74)
  x3 <- substr(datas, 75, 86)
  x4 <- substr(datas, 87, 98)
  x5 <- substr(datas, 99, nchar(datas))
  xx <- data.frame(x1, x2, x3, x4, x5)
  
  names(xx) <- c('CompanyName', 'FormType', 'CIK', 'DateFiled', 'File Name')
  for (j in 1:ncol(xx)) {
    while(length(grep('  ', xx[, j])) > 0) {
      xx[, j] <- gsub('  ', ' ', xx[, j])
    }
  }
  xx
}

# 
# 'ftp://ftp.sec.gov/edgar/daily-index/2013/QTR1/'
# 
# '<script>addRow('
# 
# 
# 'ftp://ftp.sec.gov/edgar/daily-index/'
# '2013/QTR1/'
# 
# 
# http://www.sec.gov/Archives/edgar/data/1084869/0001084869-14-000001-index.htm
# ftp://ftp.sec.gov/edgar/full-index/crawler.idx