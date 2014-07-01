library(lubridate)
library(sqldf)
setwd("~/Users/HP USER/Desktop/datasci")
regData <- read.csv("RegistrationData.csv")

regData$Row.ID <- NULL
regData$yes_rsvp_count <- NULL
regData$status <- NULL
regData$offset <- NULL
regData$rsvp_id <- NULL


  
regData$time2 <- difftime(strptime(regData$time, "%m/%d/%Y %H:%M"),
                          min(strptime(regData$created, "%m/%d/%Y %H:%M")), 
                          units = c("mins"))

regData$created2 <- difftime(strptime(regData$created, "%m/%d/%Y %H:%M"),
                             min(strptime(regData$created, "%m/%d/%Y %H:%M")), 
                             units = c("mins"))



event <- sqldf("select event_id, name, min(created2) 
                from regData
                group by event_id")

eventEnds <- sqldf("select max(time2)
                    from regData")
                    #group by event_id")
eventEnds2 <- sqldf("select max(created2)
                    from regData
                    group by event_id")

#event <- merge(eventStarts, eventEnds)

event$start <- event$"min(created2)"
event$"min(created2)" <- NULL


event$end <- matrix(eventEnds, 
                    nrow = nrow(event), 
                    ncol = 1)
#event$"max(time2)" <- NULL
write("", file = "tester.gexf", append = FALSE)
for (i in 1:nrow(event)) {
  node <- paste('   <node id="', 
                event[i,1], 
                '" label="',
                event[i,2],
                '" start="',
                event[i,3],
                '" end="728447" >',
                sep = "")
  write(node, file = "tester.gexf", append = TRUE)
}
  write("     </node>", file = "tester.gexf", append = TRUE)
person <- sqldf("select member_id, min(created2)
                    from regData
                    group by member_id")

person$end <- matrix(eventEnds, 
                      nrow = nrow(person), 
                      ncol = 1)
write("", file = "tester.gexf", append = FALSE)
for (i in 1:nrow(person)) {
  node <- paste('   <node id="', 
                person[i,1], 
                '" label="',
                person[i,1],
                '" start="',
                person[i,2],
                '" end="728447" >',
                sep = "")
  write(node, file = "tester.gexf", append = TRUE)
  write("     </node>", file = "tester.gexf", append = TRUE)

}
write("  </nodes>", file = "tester.gexf", append = TRUE)
write("  <edges>", file = "tester.gexf", append = TRUE)


edge <- regData
edge$name <- NULL
edge$time <- NULL
edge$created <- NULL
#<edge source="n1" target="n3" start="2008"/>

#write("", file = "tester.gexf", append = FALSE)
for (i in 1:nrow(regData)) {
  edge <- paste('      <edge source="', 
                regData[i,4], 
                '" target="',
                regData[i,5],
                '" start="',
                regData[i,7],
                '"/>',
                sep = "")
  write(edge, file = "tester.gexf", append = TRUE)
}














#write("nodes", file = "tester.gexf", append = FALSE)
 
# i <- 0
# for (x in nrow(event)) {
#   i <- i + 1
# #   paste('      <node id="', x$event_id, 
# #         '" label="Node ', i. '" start="',
# #         x$start, '" end="', x$end, '" >' , sep="")      
# }


#write("[", file = "crnet.gml", append = TRUE)
