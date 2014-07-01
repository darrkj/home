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






gsub("\\", "/", "C:\Users\myname\Documents", fixed = TRUE)



x <- scan(what = "C:\Users\myname\Documents")
xa <- gsub('\\\\', '/', x)


