
train = read.csv("impvartrain.csv", header = T, na.strings = c("", " ", NA))
test= read.csv("impvartest.csv", header = T, na.strings = c("", " ", NA))

act_test=read.csv("test.csv", header = T, na.strings = c("", " ", NA))
View(train)
str(train)
test$S.TEST_SCORE=NULL

#######################  imputation    #######################################
names(train)
library(mice)
vars.for.imputation= c("Entrance_Test", "Percentile_ET")
imputed = complete(mice(train[vars.for.imputation]))
train[vars.for.imputation]= imputed

vars.for.imputation= c("Entrance_Test", "Percentile_ET")
imputed = complete(mice(test[vars.for.imputation]))
test[vars.for.imputation]= imputed

test$S.TEST =ifelse(test$S.TEST== 0, 1, test$S.TEST)
write.csv(test, "test.csv", row.names = F)


####################################################################################

View(train)
View(test)
train$Salary=NULL


set.seed(1000)

split <- sample(2, nrow(train), replace=TRUE, prob=c(0.70, 0.30))
split.train = train[split==1, 1:23]
split.test = train[split==2, 1:23]

nrow(split.test)

split.train$ID=NULL

# decision tree
library(rpart)
fit=rpart(Placement ~. , data = train, method = "class", control= rpart.control(cp=0.04) )


library(caret)
library(e1071)
numFolds= trainControl(method = "cv", number =10) ## no of folds
cpGrid = expand.grid(.ntree = seq(200, 500, 30)) ## possible values for cp parameter
train(as.factor(Placement)~., data = train, method = "rf", 
      trControl= numFolds, tuneGrid = cpGrid)

library(rpart.plot)
library(rattle)
library(RColorBrewer)

fancyRpartPlot(fit)
fit.predict = predict(fit, test, type= "class")
fit.predict

table(fit.predict, split.test$Placement)
54/71

submit = data.frame(ID= act_test$ID, Placement = fit.predict)

write.csv(submit, "decision.csv", row.names = F)
table(fit.predict)


library(randomForest)
rf.fit = randomForest(as.factor(Placement) ~. , data = train, importance=T,ntree=500 )

varImpPlot(rf.fit)
fit.predict = predict(rf.fit, test, type= "class")
fit.predict

submit = data.frame(ID= act_test$ID, Placement = fit.predict)

write.csv(submit, "decision.csv", row.names = F)

library(ggplot2)
ggplot(train, aes(Percent_MBA, Percentile_ET, colour= as.factor(Placement)))+ geom_point()

library(magrittr)
library(ggvis)
train %>% ggvis(~ID, ~HSC_SSC, fill= ~as.factor(Placement))%>% layer_points()

impvariablestest = data.frame(Percent_HSC = test$Percent_HSC,Percent_SSC= test$Percent_SSC,
                          Percent_MBA= test$Percent_MBA, Percentile_ET= test$Percentile_ET,
                          Percent_Degree= test$Percent_Degree,Marks_Projectwork= test$Marks_Projectwork,
                          Marks_BOCA= test$Marks_BOCA, Marks_Communication= test$Marks_Communication,
                          Course_Degree = test$Course_Degree, Experience_Yrs= test$Experience_Yrs)


View(impvariablestest)
impvariablestrain$Placement= train$Placement

write.csv(impvariablestrain, "impvartrain.csv", row.names = F)


avg =test[, 1]+test[, 2]
avg = avg/2
as.matrix(avg)

test$HSC_SSC=avg

avg =test[, 4]+test[, 5]+test[,6]
avg = avg/3
as.matrix(avg)
test$BOC_COM_PRO= avg

test$Marks_Communication=NULL
label.y = train$Placement
train$Placement=label.y
train$Placement=NULL
test$Course_Degree=NULL

svm.fit= svm(as.factor(Placement)~. , data = train, kernel= "sigmoid", coef= 1)

svm_tune <- tune(svm, train.x=train, train.y=label.y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))

svm_tune

svm.fit
svm.predict =predict(svm.fit, test)

submit = data.frame(ID= act_test$ID, Placement = svm.predict)

write.csv(submit, "decision.csv", row.names = F)
