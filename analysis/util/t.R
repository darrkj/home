df <- data.frame(a = rep(1:10, 2), 
                 b = sample(1:3, 20, T), 
                 c = sample(1:2, 20, T))
library(plyr)
xdf <- merge(ddply(df, .(a), summarize, b = max(b)), df, by = c('a', 'b'))












































trimMerge <- function(data, fun, on, using) {
  x <- ddply(data, .(on), summarize, using = fun(using))
}



quote(merge(ddply(df, .(a), summarize, b = max(b)), 
                  df, by = c('a', 'b')))