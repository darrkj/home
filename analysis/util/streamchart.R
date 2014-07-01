# library(ggplot2)
# library(plyr)
# 
# 
stream <- function(data) { 


return(NULL)
}
# p <- ggplot(data, aes(x=Time))
# p <- p + geom_area(subset = .(Type %in% c('a', 'b', 'c', 'd')),
#                    aes(y=Value, fill=Type),
#                    position = 'stack')
# p <- p + geom_hline(yintercept=0)
# p
# 
library(ggplot2)
library(plyr)

data = read.table(text="Time    Type    Value
                  1   a   8
                  2   a   10
                  3   a   10
                  4   a   5
                  5   a   3
                  1   b   9
                  2   b   5
                  3   b   7
                  4   b   8
                  5   b   3
                  1   c   -3
                  2   c   -1
                  3   c   -5
                  4   c   -4
                  5   c   -7
                  1   d   -11
                  2   d   -3
                  3   d   -9
                  4   d   -6
                  5   d   -6", header=TRUE)

p <- ggplot(data, aes(x=Time))
p <- p + geom_line(subset = .(Type %in% c('a', 'b')),
                   aes(y=Value, colour = Type),
                   position = 'stack')
p <- p + geom_line(subset = .(Type %in% c('c', 'd')),
                   aes(y=Value, colour = Type),
                   position = 'stack')
p



p <- ggplot(data, aes(x=Time))
p <- p + geom_area(subset = .(Type %in% c('a', 'b')),
                   aes(y=Value, fill=Type),
                   position = 'stack')
p <- p + geom_area(subset = .(Type %in% c('c', 'd')),
                   aes(y=Value, fill = Type),
                   position = 'stack')
p <- p + geom_hline(yintercept=0)
p






sub <- movData[substr(as.character(movData$date), 1, 4) == "2012",]

sub2 <- sub[, c("date", "name", "Gross")]

sub2$date <- as.numeric(sub2$date)
sub2$date <- sub2$date - min(sub2$date)


mm <- c(318, 316, 296, 284, 274, 275, 248, 247, 235, 228, 174)

p <- ggplot(sub2, aes(x=date))
p <- p + geom_area(subset = .(name %in% unique(sub2$name)[mm]),
                   aes(y=Gross, fill=name),
                   position = 'stack')
p <- p + geom_hline(yintercept=0)
p + scale_fill_discrete(guide=FALSE)


require(devtools)
install_github('rCharts', 'ramnathv')

require(rCharts)
names(iris) = gsub("\\.", "", names(iris))
p1 <- rPlot(SepalLength ~ SepalWidth | Species, data = iris, color = "Species", 
            type = "point")
p1$addParams(width = 550)
p1$printChart("chart1")  # use p1$show() from your R console