library(tm)
library(SnowballC)

tfidf <- function(tfr) {
  n <- length(tfr)
  term.df <- sum(tfr[1:n] > 0)
  weight <- rep(0, n)
  weight[tfr > 0] <- (1 + log2(tfr[tfr > 0])) * log2(n / term.df)
  return(weight)
}

termDocMatrix <- function(...) {
  corp <- Corpus(VectorSource(c(...)))
  corp <- tm_map(corp, function(x) iconv(enc2utf8(x), sub = "byte"))
  corp <- tm_map(corp, tolower)
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, stemDocument)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, stripWhitespace)
  return(as.matrix(TermDocumentMatrix(corp)))
}

index <- function(docs) {
  td.matrix <- termDocMatrix(docs)
  tfidf.matrix <- t(apply(td.matrix, c(1), FUN = tfidf))
  colnames(tfidf.matrix) <- colnames(td.matrix)
  tfidf.matrix <- scale(tfidf.matrix, center = FALSE, 
                        scale = sqrt(colSums(tfidf.matrix ^ 2)))
}

search <- function(query, ind, doc, disp = TRUE) {
  if (gsub(' ', '', query) == '') print ("Input Text")
  else {
    query.vector <- rep(0, nrow(ind))
    id <- which(rownames(ind) %in% rownames(termDocMatrix(query)))
    query.vector[id] <- 1
    if (sum(query.vector) == 0) print('No relavent documents')
    else {
      doc.scores <- t(query.vector) %*% ind
    
      results.df <- data.frame(doc = colnames(ind), 
                               score = t(doc.scores), 
                               text = unlist(doc))
      results.df <- results.df[order(results.df$score, decreasing = TRUE), ]
      results.df <- results.df[results.df$score > 0, ]
    
      options(width = 2000)
      if (disp) {
        print(results.df, row.names = FALSE, right = FALSE, digits = 2)
      } else {
        print(results.df[, 1:2], row.names = FALSE, right = FALSE, digits = 2)
      }
    }
  }
}




# Create text data.
doc.list <- list("Stray cats are running all over the place. I see 10 a day!",
                 "Cats are killers. They kill billions of animals a year.",
                 "The best food in Columbus, OH is   the North Market.",
                 "Brand A is the best tasting cat food around. Your cat will love it.",
                 "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats.",
                 "The Arnold Classic came to town this weekend. It reminds us to be healthy.",
                 "I have nothing to say. In summary, I have told you nothing.",
                 "THIS IS ALL CAPS, DOES IT PICK UP",
                 "this is about baseball and sports",
                 "something about movies, back to the future")

N.docs <- length(doc.list)
names(doc.list) <- paste("doc", c(1:N.docs), sep = '')


tfidf.matrix <- index(doc.list)
search('cat food healthi', tfidf.matrix, doc.list)
