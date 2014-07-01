library(httr)
library(XML)
library(boRg)

site <- 'norml.org/laws/'


x <- strsplit(as.character(GET(site)), '\n')[[1]]
x <- grep('<area shape="poly"' , x, value = TRUE)
x <- substr(x, regexec('href=\"/', x), nchar(x))
x <- gsub('href=\"/', '', x)
x <- gsub('" alt="" />', '', x)
x <- unique(x)







getState <- function(state) {
  site <- paste('norml.org/', state, sep = '')
  
  x <- strsplit(as.character(GET(site)), '\n')[[1]]
  x <- grep('<td width' , x, value = TRUE)
  
  x <- substr(x, regexec('\">', x), nchar(x))
  
  x <- gsub('</td>', '', x)
  x <- gsub('\">', '', x)
  
  data <- data.frame(a = NA, b = NA, c = NA, d = NA, e = NA)
  for (i in seq(1, length(x), 4)) {
    data <- rbind(data, data.frame(a = x[i], b = x[i+1],
                                   c = x[i+2], d = x[i+3], e = state)) 
  }
  data[-1, ]
}




drug <- lapply(x, getState)

d <- recurBind(drug)[[1]]


d$e <- gsub('laws/item/', '', d$e)
d$e <- gsub('-penalties', '', d$e)


names(d) <- c('Offense', 'Penalty',	'Incarceration', 
              'MaxFine', 'State')

d$MaxFine <- gsub('$ ', '', d$MaxFine, fixed = T)
d$MaxFine <- gsub(',', '', d$MaxFine, fixed = T)


d$MaxFine <- as.integer(d$MaxFine)



s <- 'http://www.sentencingproject.org/map/statedata.cfm?abbrev='



s <- paste(s, state.abb, '&mapdata=true', sep = '')

ss <- lapply(s, function(x) recurBind(readHTMLTable(x)[5:11])[[1]])
#






