library(plyr)


dirTree <- function(dir) {
  cur <- getwd()
  setwd(dir)
  x <- file.info(list.files())[, 1:2]
  if (nrow(x) != 0) { 
    x$name <- rownames(x)
    rownames(x) <- NULL
    x$dir <- getwd()
    dirs <- x[x$isdir == TRUE, ]$name
    
    for (i in dirs) {
      setwd(i)
      xx <- file.info(list.files())[, 1:2]
      if (nrow(xx) != 0) {
        xx$name <- rownames(xx)
        xx$dir <- getwd()
        rownames(xx) <- NULL
        x <- rbind(x, xx)
      }
      x <- rbind(x, dirTree(paste(dir, i, sep = '/')))
      setwd(dir)
    }
  }
  return(x)
  setwd(cur)
}


c <- dirTree(getwd())

size <- ddply(c, .(dir), summarize, size2 = sum(size))
size$name <- unlist(lapply(strsplit(size$dir, '/'), tail, 1))
size$dir <- NULL
c <- merge(c, size, by = 'name', all.x = TRUE)

c$size3 <- ifelse(c$isdir == TRUE, c$size2, c$size)
c$size2 <- NULL
c$size <- NULL
c$size <- c$size3
c$size3 <- NULL


# need to make an actual tree structure.

length(unique(c$dir))
#normalizePath {base}
