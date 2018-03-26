
##�������ļ�
rm(list=ls())
a=read.csv("�¿��߼��ع�����.csv",header=T)  
a$��˾����=as.character(a$��˾����)
names=unique(a$��˾����)
a$���վ�����=as.numeric(a$���վ�����)
head(a)

a$�������˻�������=''
for (i in 1:length(a$�������˻���)){
  if (a$�������˻���[i]>=5) a$�������˻�������[i]=1
  else a$�������˻�������[i]=0
}
xtest=xtabs(~�������˻�������+�Ƿ���ʧ, data=a) 
chisq.test(xtest)

dat=data.frame()
for (i in 1:length(names)){
  temp=a[a$��˾����==names[i],]
  for (j in 1:length(temp$��˾����)){
    temp$���������ʷ���ֵ[j] = temp$���վ�����[j]/max(temp$���վ�����[1:j])
  }
  dat=rbind(dat,temp)
}
head(dat$���������ʷ���ֵ,10)

##��������ͼ
attach(dat)
head(dat)
par(mfrow=c(2,2))
boxplot(�������˻���~�Ƿ���ʧ,data=dat,main="�������˻���")
boxplot(�˻����~�Ƿ���ʧ,data=dat,main="�˻����")
boxplot(���վ�����~�Ƿ���ʧ,data=dat,main="���վ�����")
boxplot(���������ʷ���ֵ~�Ƿ���ʧ,data=dat,main="���������ʷ���ֵ")  

par(mfrow=c(2,2))
boxplot(����Ԥ��~�Ƿ���ʧ,data=dat,main="����Ԥ��")	
boxplot(����Ԥ��仯~�Ƿ���ʧ,data=dat,main="����Ԥ��仯")  
boxplot(����Ԥ��������~�Ƿ���ʧ,data=dat,main="����Ԥ��������")
par(mfrow=c(2,2))
boxplot(�ƶ�Ԥ��~�Ƿ���ʧ,data=dat,main="�ƶ�Ԥ��") 
boxplot(�ƶ�Ԥ��仯~�Ƿ���ʧ,data=dat,main="�ƶ�Ԥ��仯")  
boxplot(�ƶ�Ԥ��������~�Ƿ���ʧ,data=dat,main="�ƶ�Ԥ��������") 
par(mfrow=c(2,2))
boxplot(չʾԤ��~�Ƿ���ʧ,data=dat,main="չʾԤ��")  	
boxplot(չʾԤ��仯~�Ƿ���ʧ,data=dat,main="չʾԤ��仯")  
boxplot(չʾԤ��������~�Ƿ���ʧ,data=dat,main="չʾԤ��������")  

par(mfrow=c(2,1))
boxplot(�����Ѵ���~�Ƿ���ʧ,data=dat,main="�����Ѵ���")  
boxplot(�����ѽ��~�Ƿ���ʧ,data=dat,main="�����ѽ��") 

par(mfrow=c(2,1))
boxplot(�����Ϣ~�Ƿ���ʧ,data=dat,main="�����Ϣ")    
boxplot(�Ƿ��ڰ�����~�Ƿ���ʧ,data=dat,main="�Ƿ��ڰ�����")  

##logit�ع��ģ�ͺ�ȫģ�ͱȽ�	
unique(�Ƿ���ʧ)
glm0.a=glm(�Ƿ���ʧ~1,family=binomial(link=logit),data=dat)  			
glm1.a=glm(�Ƿ���ʧ~�������˻���+�������˻����仯+�������˻�������+�˻����+���վ�����
           +�վ����ѱ仯+���������ʷ���ֵ+����Ԥ��+����Ԥ��仯+����Ԥ��������
           +�ƶ�Ԥ��+�ƶ�Ԥ��仯+�ƶ�Ԥ��������+չʾԤ��+չʾԤ��仯+չʾԤ��������
           +�ۼ�Ͷ���·���+�����Ѵ���+�����ѽ��+�����Ϣ
           +�Ƿ��ڰ�����,family=binomial(link=logit),data=dat)
p<-predict(glm1.a,type='response')
dat$predict=p
write.csv(dat,'logitԤ����.csv')

##logit�ع鵥�������������Լ���
library(car)
anova(glm1.a,type="III")
summary(glm1.a)

##�޳�������������logit�ع�
glm2.a=glm(�Ƿ���ʧ~�������˻���+���վ�����+���������ʷ���ֵ+
          ����Ԥ��+����Ԥ��������+չʾԤ��������+
          �����ѽ��+�����Ϣ+�Ƿ��ڰ�����,family=binomial(link=logit),data=dat)
summary(glm2.a)

##AIC�Զ��������ŵ�logitģ��
logit.aic=step(glm1.a,trace=0)
summary(logit.aic)
p<-predict(logit.aic,type='response')
dat$predict=p
write.csv(dat,'logitԤ����2.csv')

##����ROC����
ngrids=100
TPR=rep(0,ngrids)
FPR=rep(0,ngrids)
for(i in 1:ngrids){
  p0=i/ngrids;
  LS.true=dat$�Ƿ���ʧ
  LS.pred=1*(p>p0)
  TPR[i]=sum(LS.pred*LS.true)/sum(LS.true)
  FPR[i]=sum(LS.pred*(1-LS.true))/sum(1-LS.true)
}
plot(FPR,TPR,type="l",col=2) 
points(c(0,1),c(0,1),type="l",lty=2)

##�ҵ�����ٽ��
f=1+TPR-FPR
critical=which(f==max(f))/ngrids

##ģ������
p=predict(logit.aic,dat)
p=exp(p)/(1+exp(p)) 
dat$�Ƿ���ʧpre=1*(p>critical)    
xtabs(~�Ƿ���ʧpre+�Ƿ���ʧ, data=dat) 


##namecount <- function(dat) {
##  return(data.frame(count = nrow(dat)))
##}
##num=ddply(a,.(a$��˾����),namecount)
#ddply(a,.(a$��˾����),nrow)

##���㱾�������ʷ���ֵ
#a$companynumber=apply(a$��˾����,1,count)

#plot(sort(p),col='blue')
#anova(glm0.a,glm1.a)  	
#1-pchisq(1326.7,5)

##BIC�Զ��������ŵ�logitģ��
n=length(a[,1])
logit.bic=step(glm1.a,k=log(n),trace=0)
summary(logit.bic) 
