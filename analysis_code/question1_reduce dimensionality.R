#######data prepare###
setwd("E:\\MCM\\processed_data")
load("review_mat\\review_mat.Rdata")
library(readr)
microwave <- read_tsv("microwave.tsv")
microwave <- data.frame(microwave)
microwaveStar_Head <- data.frame(cbind(microwave$star_rating,microwaveHead))
colnames(microwaveStar_Head)[1] <- "star_rating"
a <- microwaveStar_Head$star_rating
microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
microwaveStar_Head$star_rating[a<=3] <- 0
####question1.logistic regression######
fold_pre <- glm(formula = star_rating ~ .,
                data = microwaveStar_Head,family = "binomial")##logistics regression model
coef_p <- summary(fold_pre)$coef[, 1:4]#get coefficients and p value
submat <- cbind(microwaveStar_Head$star_rating,microwaveStar_Head[,coef_p[,4]<0.95])
colnames(submat)[1] <- "star_rating"
logs <- glm(formula = star_rating ~ .,
                data = submat,family = "binomial")##logistics regression model

#step(fold_pre)
##/data prepare

######randomForest #######
##reference:https://blog.csdn.net/yawei_liu1688/article/details/78891050
library("randomForest")
train_data <- microwaveStar_Head
n<-length(names(train_data))     #计算数据集中自变量个数，等同n=ncol(train_data)
rate=1     #设置模型误判率向量初始值
##find the best 'mtry' value which is the number of variables. 寻找最优参数mtry，即指定节点中用于二叉树的最佳变量个数
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=i,ntree=100)
  rate[i]<-mean(rf_train$err.rate)   #计算基于OOB数据的模型误判率均值
  print(rf_train)    
}

rate     #展示所有模型误判率的均值
plot(rate)
###

##find the best 'ntree' value which is the number of trees  寻找最佳参数ntree，即指定随机森林所包含的最佳决策树数目
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=1000)
plot(rf_train)    #绘制模型误差与决策树数量关系图  
#legend(1,0.35,"star_rating=0",cex=0.9,bty="n")    
#legend(1,0.25,"total",cex=0.9,bty="n") 
###

##build randomForest model 随机森林模型搭建
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
###

##输出变量重要性:分别从精确度递减和均方误差递减的角度来衡量重要程度
importance<-importance(rf_train) 
#write.csv(importance,file="E:/模型搭建/importance.csv",row.names=T,quote=F)
barplot(rf_train$importance[,4],main="输入变量重要性测度指标柱形图")
box()
###

##
importance(rf_train,type=1)
varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance),main="输入变量重要性测度散点图")
#show information
print(rf_train)    #展示随机森林模型简要信息
hist(treesize(rf_train))   #展示随机森林模型中每棵决策树的节点数
max(treesize(rf_train));min(treesize(rf_train))
MDSplot(rf_train,train_data$star_rating,palette=rep(1,2),pch=as.numeric(train_data$star_rating))    #展示数据集在二维情况下各类别的具体分布情
#/show information
#check
test_data <- train_data
pred<-predict(rf_train,newdata=test_data)  
pred_out_1<-predict(object=rf_train,newdata=test_data,type="prob")  #输出概率
table <- table(pred,test_data$star_rating)  
sum(diag(table))/sum(table)  #预测准确率
plot(margin(rf_train,test_data$star_rating),main="观测值被判断正确的概率图")
#/check

a <- importance[order(importance[,4],decreasing = T),]
roc =0
for (i in seq(from=10,to=100,by=1)) {
  i=49
ind <- na.omit(match(rownames(a)[1:i],colnames(microwaveHead)))
data <- microwaveStar_Head[,c(1,ind)]
set.seed(100)
ind <- sample(1:dim(data)[1],ceiling(2/3*dim(data)[1]),replace = F)
train_data <- data[ind,] 
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
#check
test_data <- data
pred<-predict(rf_train,newdata=test_data)  
pred_out_1<-predict(object=rf_train,newdata=test_data,type="prob")  #输出概率
pred_out_1[1:10,]
table <- table(pred,test_data$star_rating) 
table
sum(diag(table))/sum(table)  #预测准确率
library(pROC)
roc1<-roc(test_data$star_rating,pred_out_1[,1],levels=c(1,0))
roc[i] <- roc1$auc 
}
plot(roc[2:length(roc)])
plot(roc1,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)

plot(margin(rf_train,test_data$star_rating),main="观测值被判断正确的概率图")
#/check
