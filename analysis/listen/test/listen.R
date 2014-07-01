folder <- 'test'
#file.info(folder)

t <- file.info(folder)$atime
while(TRUE) {
  val <- file.info(folder)$atime
  if(t != val) {
    t <- val
    cat('something happened at: ', as.character(val), '\n')
  }
  Sys.sleep(1)
}
