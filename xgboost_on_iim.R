library(xgboost)
library(methods)
library(data.table)
library(magrittr)

rm(afteronehot2)


mbatrain= fread('C:/Users/sharad/Downloads/R_for_intimidated/IIM_B contest/imputedtrainonehot.csv', header = T, stringsAsFactors = F)
mbatest= fread('C:/Users/sharad/Downloads/R_for_intimidated/IIM_B contest/imputedtestonehot.csv', header = T, stringsAsFactors = F)

Train_iim= read.csv("train.csv")
Test_iim= read.csv("test.csv")

mbatest[, ID :=NULL]
str(mbatest)

dim(mbaTestMatrix)


mbaTrainMatrix <- mbatrain[,lapply(.SD,as.numeric)] %>% as.matrix

mbaTestMatrix <- mbatest[,lapply(.SD,as.numeric)] %>% as.matrix


our_label = Train_iim$Placement
rm(train2016)
mbaTrainMatrix = as.matrix(mbatrain)
mbaTestMatrix = as.matrix(mbatest)

set.seed(144)
cv.nround <- 1000
cv.nfold <- 20
param= list("objective" = "binary:logistic",
            eta = 0.002,
            max_depth = 5,
            gamma = 0,
            subsample = 0.99,
            colsample_bytree=1,
            "eval_metric" = "error")


xgboost_cv = xgb.cv(param=param, data = as.matrix(mbatrain),
                    label = our_label, 
                    nfold = cv.nfold, nrounds = cv.nround, prediction = T,
                    showsd= T,early.stop.round = 20,
                    verbose = T, print.every.n = 1)



nround=1000
xgboost_fit = xgboost(param=param, data = mbaTrainMatrix,
                      label = our_label, 
                      nfold = cv.nfold, nrounds = cv.nround,
                      showsd= T,early.stop.round = 20,
                      verbose = T, print.every.n = 1)



features = dimnames(mbaTrainMatrix)[[2]]
features

importance_matrix = xgb.importance(features, model =xgboost_fit)


xgb.plot.importance(importance_matrix[1:20], 5)

xgboost_pred = predict(xgboost_fit, mbaTestMatrix)
table( xgboost_pred>0.5)

result = ifelse(xgboost_pred >0.5, 1, 0)


submit = data.frame(ID= Test_iim$ID, Placement = result)
write.csv(submit, "xgboost.csv", row.names = F)
