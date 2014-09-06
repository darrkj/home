
# This is always need when pulling data from anywhere.
options(stringsAsFactors = FALSE)

# This will be used to make the GET request of the URL
library(httr)
# This contains functions to pull the structured table for each page.
library(XML)

library(devtools)

# Utility functions
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/queue.R')
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/recurBind.R')
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/igraph_2_d3.R')
source_url('https://raw.githubusercontent.com/darrkj/home/gh-pages/rcode/d3plot.R')

################################################################################
#
#  Create functions need for pulling and cleaning data.
#
################################################################################

# Pull a page from a given ID, '51566', returns the html
pull <- function(id) {
  # Create URL string from inout ID.
  url <- paste('http://www.genealogy.ams.org/id.php?id=', id, sep = '')
  # Pull html from URL and break up into lines of text.
  html <- strsplit(as.character(GET(url)), '\n')[[1]]
  # Pull the structured tabke of students and years 
  table <- readHTMLTable(html)[[1]]
  # Return both pieces of data in a list.
  list(html = html, table = table)
}


# Get the students from a given page.
students <- function(html) {
  # Pull lines which denote a row from a table, <tr> tag.
  html <- grep('<tr', html, value = T)
  # Apply html cleaning code to parse out ID.
  unlist(lapply(html, clean))
}

# Parse html table fields for relavent ID fieldss.
clean <- function(junk) {
  # Apply to string splits to get what is in between the two strings.
  strsplit(strsplit(junk, 'p?id=')[[1]][2], '\">')[[1]][1]
}


# Get all links from a given page.
links <- function(html) {
  # These are the lines with a link to a Mathematician.
  html <- grep('href=\"id.php', html, value = TRUE)
  # Ones that have click take you to a sorted list, ignore them.
  html <- grep('Click', html, invert = TRUE, value = TRUE)
  # If a line has multiple advisors, break it up to get them all.
  multiple <- html[grep('</a><br>', html, fixed = TRUE)]
  # Add this back to the list of links, last will '</p>.
  c(html, unlist(strsplit(multiple, '</a><br>')))
}

# Function to get the advisor from a list of all links.
advisors <- function(html, students) {
  # Get all links from html and clean them.
  ids <- unlist(lapply(links(html), clean))
  # Unique for cases where there are multiples from link function, and complete
  # for the strings return that are not valid links.
  ids <- unique(ids[complete.cases(ids)])
  # Subset this list to those that are not studnets of this person.
  ids[!(ids %in% students)]
}

# Function to get the name from a given page.
name <- function(html) {
  html <- grep('</h2>', html, value = TRUE)
  gsub(' </h2>', '', html, fixed = TRUE)
}


# This function is used to call other functions and group results into a struct.
work <- function(html) {
  # Pull students from page.
  student <- students(html)
  # Pull the advisor from the page.
  advisor <- advisors(html, student)
  # Group results to return.
  list(student = student, advisor = advisor, name = name(html))
}


################################################################################
#
#  Create queue and init.
#
#  The following code creates/uses functions that are not pure.
#
################################################################################


# Create the for pages still to be visited. 
q <- Queue$new()
# Create list to store already visited pages.
l <- list()

# Add an element to the queue. No return value, only called for side effects.
add <- function(elem) {
  # Check for an elements current inclusion.
  if (!is.na(elem) & !(elem %in% l)) {
    # Insert element into running list in calling scope.
    l[length(l) + 1] <<- elem
    # Push element into the queue.
    q$push(elem)
  }
}


##### Calling functions with side effects.
# This will populate the queue with few sites.
add("21566")
add("176844")
add("131446")
#####

# This will be the data capture from each page.
data <- list()


################################################################################
#
#  Code to actually run and collect data.
#
################################################################################


# Init counter.
i <- 1

# while ( q$size() > 0 ) {
while ( i < 3 ) {
  # Take the next ID from teh queue.  
  id <- q$pop()
  # Pull the raw html and table form URL.
  raw <- pull(id)
  # Clean up structured fields from html.
  person <- work(raw$html)
  # Find students that have other studnets, new site to pull.
  newLink <- person$stud[raw$table$Descendants != '']
  # Add data to growing structure.
  data[[id]] <- list(student = person$student, 
                     advisor = person$advisor, 
                     name = person$name, 
                     table = raw$table)
  
  # Add all possible people to queue, it will sort out whether they are new.
  ##### Non-pure #####
  lapply(c(newLink, person$advisor), add)
  ##### End Non-pure #####
  
  # Increment and display.
  i <- i + 1
  cat(i, q$size(), '\n')
}

# Clean up workspace after data collection.
rm(i, id, raw, person, newLink)



###############################################################################

# rm(add, advisor, clean, links, name, pull, students, work)


tree <- list()
for ( i in 1:length(data )) {
  tmp <- data[[i]]
  tree[[i]] <- data.frame(from = tmp$name, fromid = names(data)[i], 
                       tmp$table[, c('Name', 'Year')], toid = tmp$student)
  print(i)
}




t <- recurBind(tree)[[1]]


tt <- unique(c(t$fromid, t$toid))

nums <- 1:max(as.numeric(tt))


xy <- as.character(nums) %in% tt







library(igraph)



gr <- graph.data.frame(t[, c(2, 5)])
d3plot(gr)

