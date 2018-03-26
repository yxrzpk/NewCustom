
##打开数据文件
rm(list=ls())
a=read.csv("新客逻辑回归数据.csv",header=T)  
a$公司名称=as.character(a$公司名称)
names=unique(a$公司名称)
a$月日均消费=as.numeric(a$月日均消费)
head(a)

a$有消费账户数二分=''
for (i in 1:length(a$有消费账户数)){
  if (a$有消费账户数[i]>=5) a$有消费账户数二分[i]=1
  else a$有消费账户数二分[i]=0
}
xtest=xtabs(~有消费账户数二分+是否流失, data=a) 
chisq.test(xtest)

dat=data.frame()
for (i in 1:length(names)){
  temp=a[a$公司名称==names[i],]
  for (j in 1:length(temp$公司名称)){
    temp$本月相比历史最高值[j] = temp$月日均消费[j]/max(temp$月日均消费[1:j])
  }
  dat=rbind(dat,temp)
}
head(dat$本月相比历史最高值,10)

##绘制箱线图
attach(dat)
head(dat)
par(mfrow=c(2,2))
boxplot(有消费账户数~是否流失,data=dat,main="有消费账户数")
boxplot(账户余额~是否流失,data=dat,main="账户余额")
boxplot(月日均消费~是否流失,data=dat,main="月日均消费")
boxplot(本月相比历史最高值~是否流失,data=dat,main="本月相比历史最高值")  

par(mfrow=c(2,2))
boxplot(搜索预算~是否流失,data=dat,main="搜索预算")	
boxplot(搜索预算变化~是否流失,data=dat,main="搜索预算变化")  
boxplot(搜索预算利用率~是否流失,data=dat,main="搜索预算利用率")
par(mfrow=c(2,2))
boxplot(移动预算~是否流失,data=dat,main="移动预算") 
boxplot(移动预算变化~是否流失,data=dat,main="移动预算变化")  
boxplot(移动预算利用率~是否流失,data=dat,main="移动预算利用率") 
par(mfrow=c(2,2))
boxplot(展示预算~是否流失,data=dat,main="展示预算")  	
boxplot(展示预算变化~是否流失,data=dat,main="展示预算变化")  
boxplot(展示预算利用率~是否流失,data=dat,main="展示预算利用率")  

par(mfrow=c(2,1))
boxplot(总续费次数~是否流失,data=dat,main="总续费次数")  
boxplot(总续费金额~是否流失,data=dat,main="总续费金额") 

par(mfrow=c(2,1))
boxplot(框架信息~是否流失,data=dat,main="框架信息")    
boxplot(是否在白名单~是否流失,data=dat,main="是否在白名单")  

##logit回归空模型和全模型比较	
unique(是否流失)
glm0.a=glm(是否流失~1,family=binomial(link=logit),data=dat)  			
glm1.a=glm(是否流失~有消费账户数+有消费账户数变化+有消费账户数二分+账户余额+月日均消费
           +日均消费变化+本月相比历史最高值+搜索预算+搜索预算变化+搜索预算利用率
           +移动预算+移动预算变化+移动预算利用率+展示预算+展示预算变化+展示预算利用率
           +累计投放月份数+总续费次数+总续费金额+框架信息
           +是否在白名单,family=binomial(link=logit),data=dat)
p<-predict(glm1.a,type='response')
dat$predict=p
write.csv(dat,'logit预测结果.csv')

##logit回归单个变量的显著性检验
library(car)
anova(glm1.a,type="III")
summary(glm1.a)

##剔除不显著变量的logit回归
glm2.a=glm(是否流失~有消费账户数+月日均消费+本月相比历史最高值+
          搜索预算+搜索预算利用率+展示预算利用率+
          总续费金额+框架信息+是否在白名单,family=binomial(link=logit),data=dat)
summary(glm2.a)

##AIC自动搜索最优的logit模型
logit.aic=step(glm1.a,trace=0)
summary(logit.aic)
p<-predict(logit.aic,type='response')
dat$predict=p
write.csv(dat,'logit预测结果2.csv')

##绘制ROC曲线
ngrids=100
TPR=rep(0,ngrids)
FPR=rep(0,ngrids)
for(i in 1:ngrids){
  p0=i/ngrids;
  LS.true=dat$是否流失
  LS.pred=1*(p>p0)
  TPR[i]=sum(LS.pred*LS.true)/sum(LS.true)
  FPR[i]=sum(LS.pred*(1-LS.true))/sum(1-LS.true)
}
plot(FPR,TPR,type="l",col=2) 
points(c(0,1),c(0,1),type="l",lty=2)

##找到最佳临界点
f=1+TPR-FPR
critical=which(f==max(f))/ngrids

##模型评估
p=predict(logit.aic,dat)
p=exp(p)/(1+exp(p)) 
dat$是否流失pre=1*(p>critical)    
xtabs(~是否流失pre+是否流失, data=dat) 


##namecount <- function(dat) {
##  return(data.frame(count = nrow(dat)))
##}
##num=ddply(a,.(a$公司名称),namecount)
#ddply(a,.(a$公司名称),nrow)

##计算本月相比历史最高值
#a$companynumber=apply(a$公司名称,1,count)

#plot(sort(p),col='blue')
#anova(glm0.a,glm1.a)  	
#1-pchisq(1326.7,5)

##BIC自动搜索最优的logit模型
n=length(a[,1])
logit.bic=step(glm1.a,k=log(n),trace=0)
summary(logit.bic) 

