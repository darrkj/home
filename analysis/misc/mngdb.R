library(RMongo)

mongo <- mongoDbConnect("t", "localhost", 27017)

output <- dbInsertDocument(mongo, "test_data", '{"foo": "bar"}')

dbGetQuery(mongo, "test_data", 
                     '{"name": "mongo"}')

output <- dbGetQueryForKeys(mongo, "test_data", 
                            '{"foo": "bar"}')