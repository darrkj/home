library(httr)

exts <- c(1:429)
locs <- c()

for (i in exts) {
  # Create the URL string
  url <- paste("http://www.r-bloggers.com/page/", i, sep = "")
  # Get the blog posts from each historical page.
  x <- as.character(GET(url))
  # Break it apart from one large string.
  x <- unlist(strsplit(x, split = "\n", fixed = TRUE))
  # Capture the lines have links to blogs.
  x <- grep("<h2><a href=", x, value = TRUE, fixed = TRUE)
  # Remove all the tab characters. 
  x <- gsub("\t", "", x)
  # Remove leading characters from text lines.
  x <- gsub('<h2><a href=\"', "", x, fixed = TRUE)
  # Remove the trailing text.
  x <- substr(x, 1, as.integer(regexpr("title", x)) - 4)
  # Append the link to the growing list.
  locs <- c(locs, x)
  print(url)
  Sys.sleep(1)
}

rm(exts, i, url, x)


# Now get the content of each url
cleanText <- function(url) {
  xx <- as.character(GET(locs[1]))
  
  first <- "This article was first published on"
  last <- "for the author, please follow the link and comment on his blog"
  
  m <- as.integer(regexpr(first, xx))
  n <- as.integer(regexpr(last, xx))

  xxx <- substr(xx, m, n)
  xxx <- unlist(strsplit(xxx, split = "\n", fixed = TRUE))
  xxx <- xxx[xxx != ""]
  d <- '<h2>|</h2>|<h3>|</h3>|<p>|</p>|<em>|</em>'
  xxx <- gsub(d, "", xxx)
  xxx <- xxx[2:(grep('<p class=\"', xxx)-1)]
  xxx
}

xxx <- cleanText(locs[1])
text <- list()
text <- append(text, list(xxx))





svd(matrix(runif(25), nrow = 5))





.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
