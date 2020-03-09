########data prepare#####
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
######randomForest #######
##reference:https://blog.csdn.net/yawei_liu1688/article/details/78891050
library("randomForest")
train_data <- microwaveStar_Head
n<-length(names(train_data))     #计算数据集中自变量个数，等同n=ncol(train_data)
rate=1     #设置模型误判率向量初始值
##find the best 'mtry' value which is the number of variables. 寻找最优参数mtry，即指定节点中用于二叉树的最佳变量个数
rate1 <- rate
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=i,ntree=100)
  rate[i]<-mean(rf_train$err.rate)   #计算基于OOB数据的模型误判率均值
  print(rf_train)    
}

rate     #展示所有模型误判率的均值
plot(rate)
###
rate <- data.frame(rate)
save(rate,file = "rate_RF.Ddata")
##(3)plot
library(ggplot2)
p1 <- function(){
  ggplot(data = rate ,aes(x = 1:500, y = rate[1:500]))+ggtitle("relationship between variable number \nand error rate in random forest model")+
    #geom_point(aes(color = paste0("MHlen=",label+3)), size=2, alpha = 0.8) +
    geom_point(size=1.5, alpha = 0.8) +
    
    #geom_smooth(method='auto',formula=y~x,se=T, fullrange=FALSE, level=0.95, linetype = "solid") +
    labs(x="mtry(number of vairables)",y="error rate")+
    #guides(color = guide_legend(reverse=T))+
    #scale_y_log10(breaks=c(.001,.002,.003,.004,.005,.01,.02,.03,.04,.05,.1,1),labels=c(.001,.002,.003,.004,.005,.01,.02,.03,.04,.05,.1,1)
  #  scale_y_log10(breaks=c(.001,.01,.1,1),labels=c(.001,.01,.1,1)
   #               ,expand = c(0.2,0.00001)
                  #breaks = trans_breaks("log10", function(x) 10^x),
                  #labels = trans_format("log10", math_format(10^.x))
 #   ) +
    theme_bw() +
    
    theme(
      plot.title = element_text(lineheight=.8, size=18,hjust = 0.5),
      axis.title.x = element_text(color="black", size=18),
      axis.title.y = element_text(color="black", size=18),
      #delete background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      
      #???????????????
      axis.line = element_line(colour = "black",size=0.5),
      #?????????
      axis.ticks = element_line(size=0.5),
      axis.ticks.length=unit(-0.2,"cm"),
      
      #x?????????
      axis.text.x = element_text(size=18,color='black',margin = margin(t=0.3, unit = "cm")),
      #y?????????
      axis.text.y = element_text(size=18,color='black',margin = margin(r=0.3, unit = "cm")),
      #??????
      #legend.title = element_text(colour="black", size=14),
      legend.title = element_blank(),
      #legend.text = element_blank()
      legend.text = element_text(colour=c("black"), size=16)
      # remove legend
      ,legend.justification=c(1,0), legend.position=c(0.95,0.05),
      legend.background = element_rect(fill="white",
                                       size=0.5, linetype="solid", 
                                       colour ="black")
      #,legend.position="none"
    ) 
}
p1()
##/plot

##find the best 'ntree' value which is the number of trees  寻找最佳参数ntree，即指定随机森林所包含的最佳决策树数目
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=1000)
plot(rf_train)    #绘制模型误差与决策树数量关系图  
#legend(1,0.35,"star_rating=0",cex=0.9,bty="n")    
#legend(1,0.25,"total",cex=0.9,bty="n") 
###
err <- data.frame(rf_train$err.rate)
err_rate <- rbind(cbind(1:1000,err$OOB,"total"),cbind(1:1000,err$X0,"star_rating=0"),cbind(1:1000,err$X1,"star_rating=1"))
err_rate <- data.frame(as.numeric(err_rate[,1]),as.numeric(err_rate[,2]),as.character(err_rate[,3]))
colnames(err_rate) <- c("index","err","label")
##(3)plot
library(ggplot2)
p1 <- function(){
  ggplot(data = err_rate, aes(x = index, y = err))+ggtitle("randomForest error rate in different number \nof trees in microwaveHead review")+
    geom_point(aes(color = label), size=1, alpha = 0.8) +
    geom_smooth(data=err_rate,aes( colour = label),method='auto',se=T, fullrange=FALSE, level=0.95, linetype = "solid") +
    labs(x="ntree(number of trees)",y="error rate",fill = "MH length (bp)")+
    guides(color = guide_legend(reverse=T))+
    #scale_y_log10(breaks=c(.001,.002,.003,.004,.005,.01,.02,.03,.04,.05,.1,1),labels=c(.001,.002,.003,.004,.005,.01,.02,.03,.04,.05,.1,1)
    #scale_y_log10(breaks=c(.001,.01,.1,1),labels=c(.001,.01,.1,1)
     #             ,expand = c(0.2,0.00001)
                  #breaks = trans_breaks("log10", function(x) 10^x),
                  #labels = trans_format("log10", math_format(10^.x))
    #) +
    theme_bw() +
    
    theme(
      plot.title = element_text(lineheight=.8, size=16,hjust = 0.5),
      axis.title.x = element_text(color="black", size=16),
      axis.title.y = element_text(color="black", size=16),
      #delete background
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      
      #???????????????
      axis.line = element_line(colour = "black",size=0.5),
      #?????????
      axis.ticks = element_line(size=0.5),
      axis.ticks.length=unit(-0.2,"cm"),
      
      #x?????????
      axis.text.x = element_text(size=16,color='black',margin = margin(t=0.3, unit = "cm")),
      #y?????????
      axis.text.y = element_text(size=16,color='black',margin = margin(r=0.3, unit = "cm")),
      #??????
      #legend.title = element_text(colour="black", size=14),
      legend.title = element_blank(),
      #legend.text = element_blank()
      legend.text = element_text(colour=c("black"), size=16)
      # remove legend
      ,legend.justification=c(1,0), legend.position=c(0.95,0.72),
      legend.background = element_rect(fill="white",
                                       size=0.5, linetype="solid", 
                                       colour ="black")
      #,legend.position="none"
    ) 
}
p1()
##/plot

save(rf_train,file = "rf_train_ntree=1000in microwaveHead")

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
importance(rf_train)
varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/400,main="输入变量重要性测度散点图")
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


library("randomForest")
train_data <- microwaveStar_Head
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/100,main="输入变量重要性测度散点图")

importance<-importance(rf_train) 
importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
write.csv(importance,file="review_microwave_head_words_importance.csv",row.names=T,quote=F)

ind <- na.omit(match(rownames(importance)[1:500],colnames(microwaveHead)))#choose top 100 words as features
data <- microwaveStar_Head[,ind]
set.seed(100)
ind <- sample(1:dim(data)[1],ceiling(2/3*dim(data)[1]),replace = F)#choose 2/3 data as train data
train_data <- data[ind,] 
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/8,main="输入变量重要性测度散点图")
importance<-importance(rf_train) 
importance <- importance[order(importance[,4],decreasing = T),] #according to GINI value to order words
barplot(rf_train$importance[,4],main="输入变量重要性测度指标柱形图")
#show information
print(rf_train)    #展示随机森林模型简要信息
hist(treesize(rf_train))   #展示随机森林模型中每棵决策树的节点数
max(treesize(rf_train));min(treesize(rf_train))
#MDSplot(rf_train,train_data$star_rating,palette=rep(1,2),pch=as.numeric(train_data$star_rating))    #展示数据集在二维情况下各类别的具体分布情
#/show information
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
plot(roc1,print.auc=T, auc.polygon=T, grid=c(0.1, 0.2), grid.col=c("green","red"), max.auc.polygon=T, auc.polygon.col="skyblue",print.thres=T)


plot(margin(rf_train,test_data$star_rating),main="观测值被判断正确的概率图")
#/check


########randomForest final#####
setwd("E:\\MCM\\processed_data")
load("review_mat\\review_mat.Rdata")
library(readr)
microwave <- read_tsv("microwave.tsv")
microwave <- data.frame(microwave)
microwaveStar_Head <- data.frame(cbind(microwave$star_rating,microwaveBody))#combine star_rating and review matrix
colnames(microwaveStar_Head)[1] <- "star_rating"
a <- microwaveStar_Head$star_rating
microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
microwaveStar_Head$star_rating[a<=3] <- 0

library("randomForest")
negtive <- which(microwaveStar_Head$star_rating==0)
positive <- which(microwaveStar_Head$star_rating==1)
set.seed(1234)
positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
set.seed(100)
rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/100,main="the importance of hair_dryer review head's word features")
importance<-importance(rf_train) 
importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
write.csv(importance,file="randomForest\\review_microwave_body_words_importance.csv",row.names=T,quote=F)

#review_microwave_head_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("microwave.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,microwaveHead))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/40,main="the importance of microwave rview head's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  #write.csv(importance,file="randomForest\\review_pacifier_body_words_importance.csv",row.names=T,quote=F)
  save(rf_train, file="microwave_head_rf_train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin microwave_head rerview")    #绘制模型误差与决策树数量关系图  
  legend(240, 0.26, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  
}
#/review_microwave_head_words_importance

#review_microwave_body_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("microwave.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,microwaveBody))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/250,main="the importance of microwave review body's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  #write.csv(importance,file="randomForest\\review_pacifier_head_words_importance.csv",row.names=T,quote=F)
  save(rf_train, file="microwave_body_rf_train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin microwaveBody rerview")    #绘制模型误差与决策树数量关系图  
  legend(230, 0.35, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  
  
}
#/review_microwave_body_words_importance

#review_hair_dryer_head_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("hair_dryer.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,hair_dryerHead))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/100,main="the importance of hair_dryer rview head's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  #write.csv(importance,file="randomForest\\review_pacifier_body_words_importance.csv",row.names=T,quote=F)
  save(rf_train, file="hair_dryer_head_rf_train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin hair_dryer_head rerview")    #绘制模型误差与决策树数量关系图  
  legend(240, 0.24, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  
}
#/review_hair_dryer_head_words_importance

#review_hair_dryer_body_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("hair_dryer.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,hair_dryerBody))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/400,main="the importance of hair_dryer review body's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  write.csv(importance,file="randomForest\\review_hair_dryer_body_words_importance.csv",row.names=T,quote=F)
  save(rf_train,file = "hair_dryer_body_rf_train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin hair_dryer_body rerview")    #绘制模型误差与决策树数量关系图  
  legend(240, 0.35, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  }
#/review_hair_dryer_body_words_importance

##review_pacifier_body_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("pacifier.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,pacifierBody))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/500,main="the importance of pacifier review body's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  write.csv(importance,file="randomForest\\review_pacifier_body_words_importance.csv",row.names=T,quote=F)
  save(rf_train, file="pacifier_body_rf.train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin pacifierBody rerview")    #绘制模型误差与决策树数量关系图  
  legend(240, 0.4, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  
  
  }
#/review_pacifier_body_words_importance

#review_pacifier_head_words_importance
rf_model <- function(product){
  library(readr)
  microwave <- read_tsv("pacifier.tsv")
  microwave <- data.frame(microwave)
  microwaveStar_Head <- data.frame(cbind(microwave$star_rating,pacifierHead))#combine star_rating and review matrix
  colnames(microwaveStar_Head)[1] <- "star_rating"
  a <- microwaveStar_Head$star_rating
  microwaveStar_Head$star_rating[a>3] <- 1 #logistic regression requests 'y' is binary  
  microwaveStar_Head$star_rating[a<=3] <- 0
  
  library("randomForest")
  negtive <- which(microwaveStar_Head$star_rating==0)
  positive <- which(microwaveStar_Head$star_rating==1)
  set.seed(1234)
  positive_ind <- sample(positive,length(negtive),replace = T)#extract same number positive reviews
  train_data <- microwaveStar_Head[c(positive_ind,negtive),]#train data has same number positive and negtive reviews
  set.seed(100)
  rf_train<-randomForest(as.factor(train_data$star_rating)~.,data=train_data,mtry=50,ntree=400,importance=TRUE,proximity=TRUE)    
  varImpPlot(x=rf_train,sort=TRUE,n.var=nrow(rf_train$importance)/200,main="the importance of pacifier review head's word features")
  importance<-importance(rf_train) 
  importance <- importance[order(importance[,4],decreasing = T),c(3,4)] #according to GINI value to order words
  write.csv(importance,file="randomForest\\review_pacifier_head_words_importance.csv",row.names=T,quote=F)
  save(rf_train, file="pacifier_head_rf.train.Rdata")
  #write.csv(importance,file=paste0("randomForest\\review_",product,"_head_words_importance.csv"),row.names=T,quote=F)
  plot(rf_train,col=1:3,lwd=2,main= "randomForest error rate in different number of trees \nin pacifierHead rerview")    #绘制模型误差与决策树数量关系图  
  legend(230, 0.25, legend=c("total", "star_rating=1","star_rating=0"),
         col=1:3,  lty=3,lwd=2, cex=1)
  
  }
#/review_pacifier_head_words_importance
