hcc.data[colSums(!is.na(hcc.data)) > 0]
data_naomit <- na.omit(hcc.data)
str(data_naomit)
for(i in names(data_naomit)){
  data_naomit[,i]= as.factor(data_naomit[,i])
}
str(data_naomit)
set.seed(123)
sampel <- sample(nrow(data_naomit),replace = T, prob = c(0.8,0.2))
trainingdat <- data_naomit[sampel==1, ]
testingdat <- data_naomit[sampel==2, ]
print(paste("Jumlah train data :", nrow(trainingdat)))
print(paste("Jumlah test data :", nrow(testingdat)))
library(party)
library(psych)
library(caret)
fit <- ctree(V50~V1 + V2 + V3 + V4, data=trainingdat)
fit
plot(fit)
prediksi<-predict(fit,testingdat)
confusionMatrix(table(prediksi,testingdat$V50))