epls <- read.csv("SAM_Exclusions_Public_Extract_12300.csv", header = TRUE)

vendor <- read.csv("vendorHeader.csv", header = TRUE)

library(XML)
library(agrep)

epls$Address1 <- epls$Address.1
epls$Address.1 <- NULL
epls$Address2 <- epls$Address.2
epls$Address.2 <- NULL
epls$Address3 <- epls$Address.3
epls$Address.3 <- NULL
epls$Address4 <- epls$Address.4
epls$Address.4 <- NULL

vens3 <- cbind(vens2, 1)
nams3 <- cbind(nams2, 2)

lst <- merge(vens3, nams3, all.vens3 = TRUE)