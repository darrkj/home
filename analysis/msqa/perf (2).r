load("C:/Users/HP USER/Dropbox/Final/data/output.RData")
rm(names)
ind <- order(prop)
names <- files[ind]
prop <- prop[ind]
data <- result[ind]

auc <- NULL
spec <- NULL
prec <- NULL
rec <- NULL
acc <- NULL
f <- NULL
for (i in 1:20)
{
  auc[i] <- data[[i]][1,]
  spec[i] <- data[[i]][2,]
  prec[i] <- data[[i]][3,]
  rec[i] <- data[[i]][4,]
  acc[i] <- data[[i]][5,]
  f[i] <- data[[i]][6,]
}

# Define 2 vectors
cars <- c(1, 3, 6, 4, 9)
trucks <- c(2, 5, 4, 5, 12)

# Graph cars using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0,12))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos", col.main="red", font.main=4)




len <- NROW(files)
mod <- list("$glm.", "$gam.", "$rpart.", "$bayes.", "$lda.", "$nnet.")

d1st <- c(0, 0, 0, 0, 0, 0)
d2nd <- c(0, 0, 0, 0, 0, 0)
d3rd <- c(0, 0, 0, 0, 0, 0)
d4th <- c(0, 0, 0, 0, 0, 0)
d5th <- c(0, 0, 0, 0, 0, 0)
d6th <- c(0, 0, 0, 0, 0, 0)
d7th <- c(0, 0, 0, 0, 0, 0)
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
