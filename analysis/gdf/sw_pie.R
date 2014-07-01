# Load data from csv, plug in file name and F if first row
# is not the name of the field

data <- read.csv("yourfile.csv", header=T)


x1 <- c(1, 3, 6, 4, 9)
#x1 <- data[,1]

x2 <- c(4, 6, 9, 14, 11)
#x2 <- data[,2]

names <- c("val1","val2","val3","val4","val5")
# <- data[,3]


#transform the range of x2 to 0 to 1
x2 <- (x2 - min(x2))/(max(x2) - min(x2))

#color progression form left(smaller) to right(larger)
colorFunc <- colorRamp(c("green", "yellow","orange", "red" ))

#create colors from 2nd dim and color function closure
#these will be rgb green to red progrssing through yellow and orange
colors <- rgb(colorFunc(x2), maxColorValue = 255)

#create a label for percentage
labs <- round(x1/sum(x1) * 100, 1)

# Concatenate a '%' char after each value
labs <- paste(labs, "%", sep="")

# Create a pie chart with defined heading and custom colors
# and labels
pie(x1, main="NAME", col=colors, labels=labs, cex=0.8)

# Create a legend at the right   
legend(1.5, 0.5, names, cex=0.8, fill=colors)





require(CircStats)

set.seed(543)

unif.data <- runif(50, 0, 2*pi)
rose.diag(unif.data, bins=10, main = 'stacked Points', pts=TRUE)