a1 <- rate(transfusion$glm.prob, transfusion$y)
b1 <- rate(transfusion$gam.prob, transfusion$y)
c1 <- rate(transfusion$rpart.prob, transfusion$y)
d1 <- rate(transfusion$bayes.prob, transfusion$y)
e1 <- rate(transfusion$lda.prob, transfusion$y)
f1 <- rate(transfusion$nnet.prob, transfusion$y)

a2 <- roc(a1$tp, a1$fp, a1$tn, a1$fn)
b2 <- roc(b1$tp, b1$fp, b1$tn, b1$fn)
c2 <- roc(c1$tp, c1$fp, c1$tn, c1$fn)
d2 <- roc(d1$tp, d1$fp, d1$tn, d1$fn)
e2 <- roc(e1$tp, e1$fp, e1$tn, e1$fn)
f2 <- roc(f1$tp, f1$fp, f1$tn, f1$fn)

transfusion$glm.auc <- auc(a2$t, a2$f)
transfusion$gam.auc <- auc(b2$t, b2$f)
transfusion$rpart.auc <- auc(c2$t, c2$f)
transfusion$bayes.auc <- auc(d2$t, d2$f)
transfusion$lda.auc <- auc(e2$t, e2$f)
transfusion$nnet.auc <- auc(f2$t, f2$f)

par(mfrow=c(2,3))
plot(a2$f, a2$t, type='p', col="red", main="glm", 
	xlab="False Positive Rate", ylab="True Positive Rate")
plot(b2$f, b2$t, type='p', col="red", main="gam", 
	xlab="False Positive Rate", ylab="True Positive Rate")
plot(c2$f, c2$t, type='p', col="red", main="rpart", 
	xlab="False Positive Rate", ylab="True Positive Rate")
plot(d2$f, d2$t, type='p', col="red", main="bayes", 
	xlab="False Positive Rate", ylab="True Positive Rate")
plot(e2$f, e2$t, type='p', col="red", main="lda", 
	xlab="False Positive Rate", ylab="True Positive Rate")
plot(f2$f, f2$t, type='p', col="red", main="nnet", 
	xlab="False Positive Rate", ylab="True Positive Rate")

dev.copy(png,'plots/transfusion_roc.png')
dev.off()