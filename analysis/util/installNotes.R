install.packages('boRg', contriburl = 'software.elderresearch.com/boRg/')

library(devtools)

install_url(url = 'software.elderresearch.com', subdir = '/boRg/')

utils::download.file(url = 'software.elderresearch.com/boRg', destfile = 'x')



library(httr)
set_config(use_proxy(...))



# See http://www.hidemyass.com/proxy-list for a list of public proxies
# to test with
GET("http://had.co.nz", c(use_proxy("64.251.21.73", 8080), verbose()))



install.pak