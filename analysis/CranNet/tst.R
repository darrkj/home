library(XML)
library(rjson)
library(RJSONIO)
options(stringsAsFactors = FALSE)

# Write header info for gml file
write("", file = "sites.txt", append = FALSE)

# http://www.omdbapi.com/?i=&t=american+beauty
#http://www.imdb.com/xml/find?json=1&nr=1&nm=on&q=jeniffer+garner
#http://imdbapi.org/?q=american+beauty&type=json&plot=none&episode=0&limit=1&yg=0&mt=none&lang=en-US&offset=
# Read data about all packages from CRAN

cc <- "http://www.imdb.com/name/nm0004950/t"
dd <- fromJSON(paste(readLines(cc), collapse=""))

json_file <- "http://imdbapi.org/?q=american+beauty&type=json&plot=none&episode=0&limit=1&yg=0&mt=none&lang=en-US&offset="
json_data <- fromJSON(paste(readLines(json_file), collapse=""))


start <- "http://cran.r-project.org/web/packages/"
end <- "/index.html"

for (i in 1:nrow(pacList)) {
  package <- paste(start, pacList$name[i], end, sep = "")
  pacInfo <- readHTMLTable(package)[[1]]
  id <- paste("    id", pacList$name[i])
  write("  node", file = "crnet.gml", append = TRUE)
  write("  [", file = "crnet.gml", append = TRUE)
  write(id, file = "crnet.gml", append = TRUE)
  
  for (j in 1:nrow(pacInfo)) {
    if (pacInfo[j,1] == "Version:") {
      ver <- paste("    version ", '"',  pacInfo[j,2], '"', sep = "")
      write(ver, file = "crnet.gml", append = TRUE)
    } else if (pacInfo[j,1] == "Depends:") {
      deplist <- unlist(strsplit(pacInfo[j,2], ","))
    } else if (pacInfo[j,1] == "Published:") {
      date <- paste("    date ",  '"', pacInfo[j,2], '"', sep = "")
      write(date, file = "crnet.gml", append = TRUE)
    }
  }
  
  write("  ]", file = "crnet.gml", append = TRUE)
  
  for (j in 1:length(deplist)) {
    write("  edge", file = "edge.gml", append = TRUE)
    write("  [", file = "edge.gml", append = TRUE)
    edgeName <- paste("    id ", edgeCount, sep = "")
    edgeCount <- edgeCount + 1
    sourc <- paste("    source ", pacList$name[i], sep = "")
    target <- paste("    target ", deplist[j], sep = "")
    write(edgeName, file = "edge.gml", append = TRUE)
    write(sourc, file = "edge.gml", append = TRUE)
    write(target, file = "edge.gml", append = TRUE)
    write("  ]", file = "edge.gml", append = TRUE)
  }
}
write("]", file = "edge.gml", append = TRUE)
#pacInfo <- readHTMLTable("htasbio/index.html")

#pacHist <- "http://cran.r-project.org/src/contrib/Archive/asbio/"