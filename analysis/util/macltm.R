# file from http://jmlr.org/jmlr.xml
# Read the RSS file into workspace.
text <- readLines("mlrss")

# Look for lines that have address in them.
y <- grep('http', text, value = TRUE)
y <- grep('.html', y, value = TRUE)
y <- grep('isPermaLink', y, value = TRUE, invert = TRUE)

# Get rid of tags.
y <- gsub('<link>', '', y)
y <- gsub('</link>', '', y)

x <- gsub('csail.mit.edu', 'org', y)
x <- gsub('papers/v', 'papers/volume', x)
x <- gsub('.html', '', x)

name <- lapply(strsplit(x, '/'), tail, n = 1)
name <- paste('/', name, '.pdf', sep = '')

# List of paper URLs.
paper <- paste(x, name, sep = '')

rm(x, y, name, text)

library(rjson)
library(httr)


# This is useful for importing data.
options(stringsAsFactors = FALSE)

# Create dir for data files.
dir <- "files"
dir.create(dir, showWarnings = FALSE)

setwd(dir)

for (url in paper) {
  # Take the base of the file name at this loaction.
  file <- basename(url)

  # Download the file from the internet.
  download.file(url, file)
}

setwd('..')

#####################################
###     End of downloading files
#####################################

# Negation operator.
`%ni%` <- Negate(`%in%`)



# Pull in description from text file
rows <- which(text == '<description>') + 1

# Just return the description.
desc <- text[rows]


xx <- paste(desc, sep = '', collapse = '||')

library(tau)

# Tokenize the text.
x3 <- tokenize(xx)

stopList <- c(' ', ',', 'the', '.', 'of', 'a', 'and', 'to', 'in', 'is', 'that', 
              'for', 'as', 'with', 'are', 'on', 'or', 'it', 'has', 'from', ')',
              'We', 'we', 'this', 'The', 'an', 'by', 'be', 'In', '(', 'This',
              'data', 'such', 'which', 'can', 'have')

x4 <- x3[x3 %ni% stopList]

sort(table(x4))



rem <- function(x, y) gsub(x, '', y, ignore.case = TRUE)

x1 <- rem('the', desc) 
x2 <- rem('this', x1)
# Write file for each desc
setwd('desc')
x <- 0
for (i in x2) {
  x <- x + 1
  cat(i, file = paste('x_', x, '.txt', sep = ''))
}
  
  
  
  
  
  
  
  
  
  
  






