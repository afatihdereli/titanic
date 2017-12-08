library(data.table)

train<-fread("train.csv", stringsAsFactors = FALSE)

test <- fread("test.csv", stringsAsFactors = FALSE)

gender_submission<-read.delim("gender_submission.csv", header=TRUE, sep=",")

test$Survived<-rep(NA,nrow(test))
data<-rbind(train,test)




str(data)
summary(data)


data$Embarked <-as.factor(data$Embarked)
data$Pclass <-as.factor(data$Pclass)
data$Sex <-as.factor(data$Sex)
data$Survived <-as.factor(data$Survived)
data$N_Family<-data$SibSp+data$Parch+1
data$individual_price<-data$Fare/data$N_Family

data$individual_price[1044]<-mean(data$individual_price[which(data$Pclass ==3 & is.na(data$Fare)==FALSE)])
data$Embarked[which(data$Embarked=="")]<-"S"

data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)
data$Title[data$Title == 'Ms']          <- 'Miss'

data$Age[data$Title == 'Dr' & is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Dr' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Master'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Master' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Miss'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Miss' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Mr'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Mr' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Mrs'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Mrs' & is.na(data$Age)==FALSE])

data$CabinCode <- substr(data$Cabin,0,1)

tr_data<-data[1:891,]
te_data<-data[892:1309,]


model_logistic=glm(Survived~Pclass+Sex+Age+N_Family+individual_price+CabinCode,family=binomial(link="logit" ),data = tr_data)
summary(model_logistic)
mean(tr_data$Survived==round(predict(model_logistic,tr_data,type="response")))

te_data$Survived<-round(predict(model_logistic, te_data, type="response"))

write.csv(te_data[,1:2], file = 'Titanic.csv', row.names = F)

#NeuralNet Approach
library(neuralnet)

# Var1 <- rpois(100,0.5)
# Var2 <- rbinom(100,2,0.6)
# Var3 <- rbinom(100,1,0.5)
# SUM <- as.integer(abs(Var1+Var2+Var3+(rnorm(100))))
# sum.data <- data.frame(Var1+Var2+Var3, SUM)
m <- model.matrix( 
  ~ Survived + Sex +Pclass + Age + N_Family + Fare , 
  data = tr_data 
)

net.sum <- neuralnet( Survived1 ~ Sexmale + Pclass2 + Pclass3 + Age + N_Family + Fare, m, hidden=6,
                            act.fct="tanh")
#print(net.sum)
#◙plot(net.sum)

# main <- glm(SUM~Var1+Var2+Var3, sum.data, family=poisson())
# full <- glm(SUM~Var1*Var2*Var3, sum.data, family=poisson())
# prediction(net.sum, te_data)

#temp_test <- subset(te_data, select = c("alcohol", "malic", "ash", "ash_alcalinity", "magnesium", "phenols", "flavanoids", "nonflavanoids", "proanthocyanins", "color_intensity", "hue", "od280", "proline"))

m2 <- model.matrix( 
  ~  Pclass + Sex + Age + N_Family + Fare, 
  data = te_data 
)

nn_pred<-compute(net.sum,m2[,2:ncol(m2)])

res<-cbind(te_data[,1],round(nn_pred$net.result))

names(res)<-c("PassengerId","Survived")

write.csv(res, file = 'Titanic.csv', row.names = F)



#SVM Model
library(e1071)


te_data[is.na(te_data$Fare),]$Fare<-mean(te_data$Fare,na.rm = TRUE)

SVMmodel<-svm(as.factor(Survived) ~ Age+SibSp+Parch+Sex+Fare, data = tr_data, cost = 100, gamma = 1)

prediction<-predict(SVMmodel, te_data[,c(5,6,7,8,10)])
plot(prediction)
# Pclass=table(prediction,te_data[,2])
# plot(Pclass)
# me=mean(Pclass)
# print(me)

output<-data.frame(te_data$PassengerId, data.frame(prediction))

colnames(output)=cbind("PassengerId","Survived")

write.csv(output, file = 'Titanic.csv', row.names = F)


#XGBoost
library(xgboost)

xgb <- xgb.cv(data = data.matrix(tr_data[,-1]), 
               label = tr_data[,2], 
              nfold = 3, nrounds = cv.nround
)

y_pred <- predict(xgb, data.matrix(X_test[,-1]))

#Random Forest


#Ctree
library(rpart)
library(rattle)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + individual_price + Embarked,
             data=tr_data,
             method="class")

#fancyRpartPlot(fit)

Prediction <- predict(fit, te_data, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "Titanic.csv", row.names = FALSE)
