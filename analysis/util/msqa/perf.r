#load("res2.RData")
#source("func.r")
sets <- list.files('C:/Users/Darrell/Dropbox/MSQA/data')
len <- NROW(sets)
mod <- list("$glm.", "$gam.", "$rpart.", "$bayes.", "$lda.", "$nnet.")

d1st <- c(0, 0, 0, 0, 0, 0)
d2nd <- c(0, 0, 0, 0, 0, 0)
d3rd <- c(0, 0, 0, 0, 0, 0)
d4th <- c(0, 0, 0, 0, 0, 0)
d5th <- c(0, 0, 0, 0, 0, 0)
d6th <- c(0, 0, 0, 0, 0, 0)
dbet <- c(0, 0, 0, 0, 0, 0)
dwor <- c(0, 0, 0, 0, 0, 0)
dtot <- c(0, 0, 0, 0, 0, 0)

term <- "f"
#iterate over both
for (i in c(1:len))
{
  glm <- eval(parse(text=paste(sets[i], mod[1], term, sep = "")))
  gam <- eval(parse(text=paste(sets[i], mod[2], term, sep = "")))
  rpart <- eval(parse(text=paste(sets[i], mod[3], term, sep = "")))
  bayes <- eval(parse(text=paste(sets[i], mod[4], term, sep = "")))
  lda <- eval(parse(text=paste(sets[i], mod[5], term, sep = "")))
  nnet <- eval(parse(text=paste(sets[i], mod[6], term, sep = "")))

  glm[is.na(glm)] <- 0
  gam[is.na(gam)] <- 0
  rpart[is.na(rpart)] <- 0
  bayes[is.na(bayes)] <- 0
  lda[is.na(lda)] <- 0
  nnet[is.na(nnet)] <- 0

  vect <- c(glm, gam, rpart, bayes, lda, nnet)
  ord <- order(vect)
  d1st[ord[6]] <- d1st[ord[6]] + 1
  d2nd[ord[5]] <- d2nd[ord[5]] + 1
  d3rd[ord[4]] <- d3rd[ord[4]] + 1
  d4th[ord[3]] <- d4th[ord[3]] + 1
  d5th[ord[2]] <- d5th[ord[2]] + 1
  d6th[ord[1]] <- d6th[ord[1]] + 1
  dtot[ord[6]] <- dtot[ord[6]] + 6
  dtot[ord[5]] <- dtot[ord[5]] + 5
  dtot[ord[4]] <- dtot[ord[4]] + 4
  dtot[ord[3]] <- dtot[ord[3]] + 3
  dtot[ord[2]] <- dtot[ord[2]] + 2
  dtot[ord[1]] <- dtot[ord[1]] + 1
  dbet[ord[6]] <- dbet[ord[6]] + vect[ord[6]] - vect[ord[1]]
  dbet[ord[5]] <- dbet[ord[5]] + vect[ord[5]] - vect[ord[1]]
  dbet[ord[4]] <- dbet[ord[4]] + vect[ord[4]] - vect[ord[1]]
  dbet[ord[3]] <- dbet[ord[3]] + vect[ord[3]] - vect[ord[1]]
  dbet[ord[2]] <- dbet[ord[2]] + vect[ord[2]] - vect[ord[1]]
  dwor[ord[5]] <- dwor[ord[5]] + vect[ord[6]] - vect[ord[5]]
  dwor[ord[4]] <- dwor[ord[4]] + vect[ord[6]] - vect[ord[4]]
  dwor[ord[3]] <- dwor[ord[3]] + vect[ord[6]] - vect[ord[3]]
  dwor[ord[2]] <- dwor[ord[2]] + vect[ord[6]] - vect[ord[2]]
  dwor[ord[1]] <- dwor[ord[1]] + vect[ord[6]] - vect[ord[1]]
}
davg <- dbet - dwor
################################################################
out <- data.frame(d1st, d2nd, d3rd, d4th, d5th, d6th, dtot, dbet, dwor, davg)
write.table(out, file = "f.csv", append = FALSE, quote = FALSE, sep = ", ", 
   eol = "\n", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE, 
   qmethod = c("escape", "double"))
