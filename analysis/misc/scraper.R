library(XML)
options(stringsAsFactors = FALSE)

# Write header info for gml file
write("graph", file = "crnet.gml", append = FALSE)
write("[", file = "crnet.gml", append = TRUE)
write("  Creator Gephi", file = "crnet.gml", append = TRUE)
write("  directed 1", file = "crnet.gml", append = TRUE)

edgeCount <- 1
write("", file = "edge.gml", append = FALSE)
# Read data about all packages from CRAN
pacList <- readHTMLTable(
  "http://cran.r-project.org/web/packages/available_packages_by_date.html")[[1]]

# Clean dataframe structure to make analysis easier
pacList$date <- as.character(pacList[,1])
pacList$name <- as.character(pacList[,2])
pacList$title <- as.character(pacList[,3])
pacList[,1] <- NULL
pacList[,1] <- NULL
pacList[,1] <- NULL


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