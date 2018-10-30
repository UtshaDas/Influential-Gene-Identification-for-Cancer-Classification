dataset=read.csv("F:/Thesis/ColonCancer/SelectedGene.csv")
dataset=dataset[2:13]

library(caTools)
set.seed(123)
split=sample.split(dataset$V2001,SplitRatio = 0.75)
training=subset(dataset,split==TRUE)
testing=subset(dataset,split==FALSE)

training[-12]=scale(training[-12])
testing[-12]=scale(testing[-12])

library(e1071)

#set.seed(123)
#tmodel<-tune(svm,V2001~.,data=training,
#             ranges = list(epsilon=seq(0,1,0.1),cost=2^(2:10)))
#classifier<-tmodel$best.model

classifier=svm(formula=factor(V2001) ~ .,
               data=training,
               scale=TRUE,
               type='C-classification',
               kernel='sigmoid',
               cost=0.2,
               gamma=0.09)

y_pred=predict(classifier,newdata=testing[-12])
cm=table(testing[,12],y_pred)
miscl<-1-sum(diag(cm))/sum(cm)
print((1-miscl)*100)