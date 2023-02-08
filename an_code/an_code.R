#### 在跑该程序前，请先安装以下package,也可以直接跑一下的代码直接安装package
install.packages("Matrix")
install.packages("glmnet")
install.packages("data.table")
#### 如果已经安装，就不需要怕跑上述的内容了
################################################################

library(data.table)
get_coe <- function(the_fit,the_lamb){
  Coefficients <- coef(the_fit, s = the_lamb)
  Active.Index <- which(Coefficients != 0)
  Active.Coefficients <- Coefficients[Active.Index]
  re <- data.frame(rownames(Coefficients)[Active.Index],Active.Coefficients)
  re <- data.table('var_names'=rownames(Coefficients)[Active.Index],
                   'coef'=Active.Coefficients)
  re$expcoef <- exp(re$coef)
  return(re[order(expcoef)])
}

# filefolder <- "D:\\我的坚果云\\陈彤论文"
# setwd(filefolder)
RawData <- read.csv("an_code/列线图data.csv",sep="/", header=TRUE, na.strings="NA", encoding="UTF-8")
yinVar <- as.factor(RawData[,6]) #看因变量在哪一列就填什么数字
Data <- data.frame(yinVar,RawData[,-6])#将RawData的第十列去掉，再跟yinVar这一列合并成新表Data，在Data里，yinVar为第一列
summary(Data)
Data[,-c(1,2,3,6,7,8)]=lapply(Data[,-c(1,2,3,6,7,8)], as.numeric)
Data[,c(1,2,3,6,7,8)]=lapply(Data[,c(1,2,3,6,7,8)], as.factor)
summary(Data)
trainset <- Data
library("Matrix")
library("glmnet") 
pdf("lasso.pdf",height = 5,width=6)
x = as.matrix(trainset[, -1])
y = as.matrix(trainset[, 1])
fit<-glmnet(x,y,family="binomial")
plot(fit,xvar = "lambda",label = TRUE)
set.seed(12)
cv.fit<-cv.glmnet(x,y,family="binomial",type.measure="deviance") 
plot(cv.fit)
dev.off()
# cv.fit$lambda.min
# cv.fit$lambda.1se
#### 两个位置，一个是最大AUC的位置，一个是最佳AUC位置
get_coe(cv.fit,cv.fit$lambda.min)
get_coe(cv.fit,cv.fit$lambda.1se)

################################## 可视化 ##################
plot(fit,xvar = "lambda",label = TRUE)
plot(cv.fit)