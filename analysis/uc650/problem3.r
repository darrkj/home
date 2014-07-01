#############################################
# Problem 3 Boston Housing Data

#Create text file to output data into
sink("problem3.txt", append=FALSE, split=FALSE)
library(MASS)
library(boot)
library(gam)
library(stats)
library(rpart)
library(tree)

data(Boston);     #this data is in MASS package
colnames(Boston) 
#column variable's names; the last variable "medv" is used as y

#part i

# dim[1] = num of observation dim [2] = num of column
size <- floor(dim(Boston)[1]*0.9); 

bds<-sample(rownames(Boston),size)

trainingset<-Boston[bds,]
testset <- Boston[setdiff(rownames(Boston), bds),]

#training set vectors
y1 <- trainingset[,14]
u1 <- trainingset[,1]
u2 <- trainingset[,2]
u3 <- trainingset[,3]
u4 <- trainingset[,4]
u5 <- trainingset[,5]
u6 <- trainingset[,6]
u7 <- trainingset[,7]
u8 <- trainingset[,8]
u9 <- trainingset[,9]
u10 <- trainingset[,10]
u11 <- trainingset[,11]
u12 <- trainingset[,12]
u13 <- trainingset[,13]

#test set vectors
y2 <- trainingset[,14]
v1 <- trainingset[,1]
v2 <- trainingset[,2]
v3 <- trainingset[,3]
v4 <- trainingset[,4]
v5 <- trainingset[,5]
v6 <- trainingset[,6]
v7 <- trainingset[,7]
v8 <- trainingset[,8]
v9 <- trainingset[,9]
v10 <- trainingset[,10]
v11 <- trainingset[,11]
v12 <- trainingset[,12]
v13 <- trainingset[,13]


#final model from homework one, x3 and x7 removed
bos_old <- glm(y2 ~ v1+v2+v4+v5+v6+v8+v9+v10+v11+v12+v13)

cross_validate <- summary(bos_old)
cross_validate


y <- Boston[,14]
x1 <- Boston[,1]
x2 <- Boston[,2]
x3 <- Boston[,3]
x4 <- Boston[,4]
x5 <- Boston[,5]
x6 <- Boston[,6]
x7 <- Boston[,7]
x8 <- Boston[,8]
x9 <- Boston[,9]
x10 <- Boston[,10]
x11 <- Boston[,11]
x12 <- Boston[,12]
x13 <- Boston[,13]


#cv.glm
bos.glm <- glm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)
cv.err <- cv.glm(Boston, bos.glm, K=10)
cv_glm <- summary(cv.err)
#predict
bos.lm <- lm(y ~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)
pred.err <- predict(bos.lm)
predict_lm <- summary(pred.err)
#gam
gam_mod1 <- gam(y ~ s(x1)+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)
gam_mod2 <- gam(y ~ lo(x1)+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13)

#add output for gam models

#cart
#both rpart and tree on the training set
tree1_tr <- rpart(y2 ~ v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13)
tree2_tr <- tree(y2 ~ v1+v2+v3+v4+v5+v6+v7+v8+v9+v10+v11+v12+v13)

#both rpart and tree on the test set
tree1_te <- rpart(y1 ~ u1+u2+u3+u4+u5+u6+u7+u8+u9+u10+u11+u12+u13)
tree2_te <- tree(y1 ~ u1+u2+u3+u4+u5+u6+u7+u8+u9+u10+u11+u12+u13)