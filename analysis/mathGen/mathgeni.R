
library(httr)

source('queue.R')


# Pull a page from a given ID, '51566', returns the html
pull <- function(link) {
  link <- paste('http://www.genealogy.ams.org/id.php?id=', link, sep = '')
  strsplit(as.character(GET(link)), '\n')[[1]]
}

# Get the students from a given page.
students <- function(page) {
  page <- grep('<tr', page, value = T)
  unlist(lapply(page, clean))
}

# Get all links from a given page.
links <- function(page) {
  page <- grep('href=\"id.php', page, value = T)
  page <- grep('Click', page, invert = T, value = T)
  page
}


clean <- function(junk) {
  strsplit(strsplit(junk, 'p?id=')[[1]][2], '\">')[[1]][1]
}

advisor <- function(page, stud) {
  ids <- unlist(lapply(links(page), clean))
  ids[!(ids %in% stud)]
}


name <- function(page) {
  page <- grep('</h2>', page, value = T)
  gsub(' </h2>', '', page, fixed = T)
}

year <- function(page) {
  p1 <- page[grep('Ph.D.', page)+1]
  p2 <- page[grep('Ph. D.', page)+1]
  p3 <- page[grep('Dr. rer. nat.', page)+1]
  p4 <- page[grep('Dr.', page)+1]
  page <- c(p1, p2, p3, p4)
  
  ind <- gregexpr('</span>', page)[[1]]
  as.integer(substr(page, ind[1]+8, ind[2]-1))
}


work <- function(page) {
  stud <- students(page)
  adv <- advisor(page, stud)
  list(stud = stud, adv = adv, name = name(page), year = year(page))
  #list(stud = stud, adv = adv, name = name(page))
}


q <- Queue$new()


add <- function(elem) {
  if (!is.na(elem) & !(elem %in% l)) {
    l[length(l) + 1] <<- elem
    q$push(elem)
  }
}


l <- list()
l <- add("21566")


tree <- list()

###############################################################################



i <- 1
while ( i < 1000 ) {
#while ( q$size() > 0 ) {
  id <- q$pop()
  x <- work(pull(id))

  tree[[id]] <- x
  lapply(c(x$stud, x$adv), add)
  i <- i + 1
  cat(i, q$size(), '\n')
}


rm(i, id, x)







#

























####



advisor <- function(page) {
  page <- grep('<tr', page, value = T, invert = T)
  adv <- grep('<p style=\"text-align: center; line-height: 2.75ex\">', page, 
              fixed = T, value = T)
  #phd <- grep('  #006633; margin-left: 0.5em\">', x, fixed = T, value = T)
  #pers <- grep('</h2>', x, fixed = T, value = T)
  if(length(adv) != 0) {
    adv <- strsplit(adv, 'Advisor 2')[[1]]
    adv <- unlist(lapply(adv, clean))
  } else {
    adv <- NA
  }
  adv
}






students <- function(page) {
  page <- grep('<tr', page, value = T)
  x <- page[1]
  st <- gregexpr('php?id=', x, fixed = T)
  en <- gregexpr('\">', x, fixed = T)
  x <-  substr(x, st[[1]]+7, en[[1]][2]-1)
  
  page <- page[-1]
  page <- gsub('<tr ><td><a href=\"id.php?id=', '', page, fixed = T)
  page <- gsub('<tr style=\"background-color: #E5E6CF;\"><td><a href=\"id.php?id=', 
               '', page, fixed = T)
  page <- gsub('</a></td><td>', '|', page, fixed = T)
  page <- gsub('</td><td style=\"padding-left: 2px; padding-right: 2px\">', '|', 
               page, fixed = T)
  page <- gsub('</td><td style=\"text-align: center\">' , '|', page, fixed = T)
  page <- gsub('\">', '|', page, fixed = T)
  page <- gsub('</td></tr>', '', page, fixed = T)
  c(unlist(lapply(strsplit(page, '|', fixed = T), `[`, 1)), x)
}



students <- function(page) {
  page <- grep('<tr ', page, value = T)
  page <- gsub('<tr ><td><a href=\"id.php?id=', '', page, fixed = T)
  page <- gsub('<tr style=\"background-color: #E5E6CF;\"><td><a href=\"id.php?id=', 
               '', page, fixed = T)
  page <- gsub('</a></td><td>', '|', page, fixed = T)
  page <- gsub('</td><td style=\"padding-left: 2px; padding-right: 2px\">', '|', 
               page, fixed = T)
  page <- gsub('</td><td style=\"text-align: center\">' , '|', page, fixed = T)
  page <- gsub('\">', '|', page, fixed = T)
  page <- gsub('</td></tr>', '', page, fixed = T)
  unlist(lapply(strsplit(page, '|', fixed = T), `[`, 1))
}


site <- 'http://www.genealogy.ams.org/'

seed <- 'http://www.genealogy.ams.org/id.php?id=21566'
seed <- 'http://www.genealogy.ams.org/id.php?id=27506'

pull <- function(url) strsplit(as.character(GET(url)), '\n')[[1]]




x <- pull(seed)



x <- grep('<tr', x, value = T, invert = T)
x <- x[x != '']

adv <- grep('<p style=\"text-align: center; line-height: 2.75ex\">', 
            x, fixed = T, value = T)
phd <- grep('  #006633; margin-left: 0.5em\">', x, fixed = T, value = T)
pers <- grep('</h2>', x, fixed = T, value = T)

#library(XML)

#y <- readHTMLTable(seed)[[1]]


xx <- gsub('<tr ><td><a href=\"id.php?id=', '', xx, fixed = T)
xx <- gsub('<tr style=\"background-color: #E5E6CF;\"><td><a href=\"id.php?id=', 
           '', xx, fixed = T)
xx <- gsub('</a></td><td>', '|', xx, fixed = T)
xx <- gsub('</td><td style=\"padding-left: 2px; padding-right: 2px\">', '|', xx, 
           fixed = T)
xx <- gsub('</td><td style=\"text-align: center\">' , '|', xx, fixed = T)
xx <- gsub('\">', '|', xx, fixed = T)
xx <- gsub('</td></tr>', '', xx, fixed = T)



id.php?id=36533

Wen-An Yong
'</a></td><td>'
Ruprecht-Karls-Universit&auml;t Heidelberg

1992


