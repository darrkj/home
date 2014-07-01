library(twitteR)

# loading the package is required once each session
require(XML)

# initialize a storage variable for Twitter tweets
mydata <- character(0)

# paginate to get more tweets
for (page in c(1:15))
{
  # search parameter
  twitter_q <- URLencode('#USPS')
  # construct a URL
  twitter_url = paste('http://search.twitter.com/search.atom?q=',twitter_q,'&rpp=100&page=', page, sep='')
  # fetch remote URL and parse
  mydata.xml <- xmlParseDoc(twitter_url, asText=F)
  # extract the titles
  mydata.vector <- xpathSApply(mydata.xml, '//s:entry/s:title', xmlValue, namespaces =c('s'='http://www.w3.org/2005/Atom'))
  # aggregate new tweets with previous tweets
  mydata <- c(mydata.vector, mydata)
}

# how many tweets did we get?
length(mydata)

library(twitteR)

delta.tweets <- searchTwitter('@delta', n=1500)

length(delta.tweets)
class(dlta.tweets)

dmhash_tweets = searchTwitter("#usps", n= 1000)

dm2_tweets = searchTwitter("usps", since='2012-05-12', 
                           until='2012-10-17')