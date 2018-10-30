
dataset=read.csv("F:/Thesis/Leukemia/Selected4.csv")
dataset=dataset[2:6]

library(caTools)
set.seed(123)
split=sample.split(dataset$CLASS,SplitRatio = 0.84)
training=subset(dataset,split==TRUE)
testing=subset(dataset,split==FALSE)

training[-5]=scale(training[-5])
testing[-5]=scale(testing[-5])

library(e1071)

#set.seed(123)
#tmodel<-tune(svm,CLASS~.,data=training,
#            ranges = list(epsilon=seq(0,1,0.1),cost=2^(2:10)))
#classifier<-tmodel$best.model

classifier=svm(formula=factor(CLASS) ~ .,
               data=training,
               scale=TRUE,
               type='C-classification',
               kernel='radial',
               cost=32,
               gamma=0.1428571)

y_pred=predict(classifier,newdata=testing[-5])
cm=table(testing[,5],y_pred)
miscl<-1-sum(diag(cm))/sum(cm)
print((1-miscl)*100)