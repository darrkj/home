#do log reg of adult data here

train <- read.csv("data/adult.txt", header = TRUE, sep = "|")

test <- read.csv("data/adult-test.txt", header = TRUE, sep = "|")

train$yy <- ifelse(train$y == "<=50K", 0, 1)

test$yy <- ifelse(test$y == "<=50K", 0, 1)

prop.train <- sum(train$yy)/nrow(train)
prop.test <- sum(test$yy)/nrow(test)

ad.glm <- glm(yy ~ age + education.num + hours.per.week, data = train)

ad.pred <- predict(ad.glm, test)

beta0 <- ad.glm[[1]][[1]]
beta1 <- ad.glm[[1]][[2]]
beta2 <- ad.glm[[1]][[3]]
beta3 <- ad.glm[[1]][[4]]

pred <- NULL

pred$prob <-  1 / (exp(-1*(beta0 + test$age * beta1 + test$education.num * beta2 + 
                             test$hours.per.week * beta3)) + 1)

pred$act <- test$yy
pred$maj <- 0

pred$pred <- round(pred$prob)

pred$prob2 <- pred$prob - prop.test/2
pred$pred2 <- round(pred$prob2)

accuracy <- sum(pred$act == round(pred$pred))/nrow(test)
accuracy2 <- sum(pred$act == round(pred$pred2))/nrow(test)
accuracy3 <- sum(pred$act == pred$maj)/nrow(test)


