
library(e1071)
library(caret)
file<-"~rkannan/data/heart.csv"

isOverfitting<-function(trcfm,tstcfm)
{
  # if the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  tracc=trcfm$overall['Accuracy']
  tstacc=tstcfm$overall['Accuracy']
  tracc > 2*tstacc 
}

isUnderfitting<-function(trcfm)
{
  # if the accuracy from training is much more than accuracy testing phase
  # then we conclude Model is overfitting 
  # unable to generalize
  tracc=trcfm$overall['Accuracy']
  0.5 >  tracc
}

file<-'c:/Users/rkannan/rk/data/heart.csv'
heart<-read.csv(file,head=T,sep=',',stringsAsFactors=F)
head(heart)
catheart<-heart[,c(2,3,6,7,9,11,12,13,14)]

head(catheart)
dim(catheart)
table(catheart$target)
sum(table(catheart$target))

set.seed(43)
trdidx<-sample(1:nrow(catheart),0.7*nrow(catheart),replace=F)
trcatheart<-catheart[trdidx,]
tstcatheart<-catheart[-trdidx,]

table(catheart$target)

dim(trcatheart)
table(trcatheart$target)
dim(tstcatheart)
table(tstcatheart$target)

nbtr.model<-naiveBayes(target~.,data=trcatheart)

nbtr.trpred<-predict(nbtr.model,trcatheart[,-c(9)],type='raw')
nbtr.trclass<-unlist(apply(round(nbtr.trpred),1,which.max))-1
nbtr.trtbl<-table(trcatheart[[9]], nbtr.trclass)
tr.cfm<-caret::confusionMatrix(nbtr.trtbl)
tr.cfm

nbtr.tspred<-predict(nbtr.model,tstcatheart[,-c(9)],type='raw')

roc.nbtr.tspred<-nbtr.tspred[,2]
nbtr.tsclass<-unlist(apply(round(nbtr.tspred),1,which.max))-1
nbtr.tstbl<-table(tstcatheart[[9]], nbtr.tsclass)
tst.cfm<-caret::confusionMatrix(nbtr.tstbl)
tst.cfm

ifelse(isUnderfitting(tr.cfm),"model is deficient",paste("There is no underfitting", "model is an effective learner@ [",tr.cfm$overall['Accuracy'],"] accuracy"))
ifelse(isOverfitting(tr.cfm,tst.cfm),"model is overfitting -- too complex",
       paste(" There is no overfitting, model is an effective learner@ [",tr.cfm$overall['Accuracy'],"] training accuracy vs testing accuracy=[",tst.cfm$overall['Accuracy'],']'))

trcathearttarget_0<-trcatheart[trcatheart$target==0,]

trcathearttarget_1<-trcatheart[trcatheart$target==1,]

tstcathearttarget_0<-tstcatheart[tstcatheart$heart==0,]
tstcathearttarget_1<-tstcatheart[tstcatheart$heart==1,]

c0freq<-nrow(trcathearttarget_0)
c1freq<-nrow(trcathearttarget_1)
p0<-c0freq/(c0freq+c1freq)
p1<-c1freq/(c0freq+c1freq)
c(p0,p1)

classProbabilities<-
  function(dsetc1,dsetc2,prop,val,c1prob,c2prob)
  {
    
    propdsetc1=dsetc1[dsetc1[prop]==val,]
    propdsetc2=dsetc2[dsetc2[prop]==val,]
    c(nrow(propdsetc1)/nrow(dsetc1),nrow(propdsetc2)/nrow(dsetc1))
  }

trcathearttarget_0[1,]

classProbabilities(trcathearttarget_0,trcathearttarget_1,'sex',1,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'cp',0,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'fbs',0,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'restecg',0,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'exang',1,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'slope',2,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'ca',2,p0,p1)
classProbabilities(trcathearttarget_0,trcathearttarget_1,'thal',3,p0,p1)

xx<-c(
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'sex',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'cp',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'fbs',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'restecg',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'exang',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'slope',2,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'ca',2,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'thal',3,p0,p1),
  c(p0,p1))
nr<-9
c(p0,p1)
mx<-matrix(xx,nr,ncol=2,byrow=T)
#mx
cp<-unlist(apply(mx,2,cumprod))
#cp[nr,]
pred_class<-which.max(cp[nr,])-1
trcathearttarget_0[1,]$target
ifelse(trcathearttarget_0[1,]$target==0,
       ifelse(pred_class==0,'TP','FN'),
       ifelse(pred_class==1,'TN','FP'))

tstcatheart[7,]

tstcathearttarget_0<-tstcatheart[tstcatheart$heart==0,]
tstcathearttarget_1<-tstcatheart[tstcatheart$heart==1,]

xx<-c(
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'sex',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'cp',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'fbs',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'restecg',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'exang',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'slope',2,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'ca',2,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'thal',2,p0,p1),
  c(p0,p1))

mx<-matrix(xx,nrow=9,ncol=2,byrow=T)

unlist(apply(mx,2,cumprod))
cp<-unlist(apply(mx,2,cumprod))
which.max(cp[9,])-1
pred_class<-which.max(cp[nr,])-1
pred_class
tstcatheart[7,]$target
ifelse(tstcatheart[7,]$target==0,
       ifelse(pred_class==0,'TP','FN'),
       ifelse(pred_class==1,'TN','FP'))

tstcatheart[23,]

xx<-c(
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'sex',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'cp',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'fbs',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'restecg',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'exang',1,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'slope',2,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'ca',0,p0,p1),
  classProbabilities(trcathearttarget_0,trcathearttarget_1,'thal',2,p0,p1),
  c(p0,p1))

mx<-matrix(xx,nrow=9,ncol=2,byrow=T)

unlist(apply(mx,2,cumprod))
cp<-unlist(apply(mx,2,cumprod))
which.max(cp[9,])-1
pred_class<-which.max(cp[nr,])-1
pred_class
tstcatheart[23,]$target
ifelse(tstcatheart[23,]$target==0,
       ifelse(pred_class==0,'TP','FN'),
       ifelse(pred_class==1,'TN','FP'))

evaluate_naiveBayes<-
function(nv,vv,targetclass,trdsetcl1,trdsetcl2,cl1prob,cl2prob)
{
nvl<-length(nv)
xx<-c()
for (idx in 1:nvl)
{
attr<-nv[idx]
value<-vv[[idx]]
#print(c(attr,value))
#xx<-c(xx,classProbabilities(trdsetcl1,trdsetcl2,nv[idx],vv[[idx]],cl1prob,cl2prob))
xx<-c(xx,classProbabilities(trdsetcl1,trdsetcl2,attr,value,cl1prob,cl2prob))
}
xx<-unlist(c(xx,  c(p0,p1)))
nvl<-nvl+1
mx<-matrix(xx,nrow=nvl,ncol=2,byrow=T)

unlist(apply(mx,2,cumprod))
cp<-unlist(apply(mx,2,cumprod))

pred_class<-which.max(cp[nvl,])-1
pred_class
targetclass
metric<-ifelse(targetclass==0,
       ifelse(pred_class==0,'TP','FN'),
       ifelse(pred_class==1,'TN','FP'))
c(targetclass,pred_class,metric)
}


tstcount<-nrow(tstcatheart)
tstcol<-ncol(tstcatheart)
featuremx<-tstcatheart[,-c(which(names(tstcatheart)=='target'))]
nv<-names(featuremx)
tstclass<-tstcatheart$target
tstpred<-c()
for(r in 1:tstcount)
{
  tstpred<-c(tstpred,
             evaluate_naiveBayes(nv,featuremx[r,],
                            tstclass[r],trcathearttarget_0,
                            trcathearttarget_1,p0,p1))
}
tstresults<-matrix(tstpred,nrow=tstcount,ncol=3,byrow=T,dimnames=
                     list(1:tstcount,c("GT",'Pred','metric')))
tstresults
table(tstresults[,3])
table(tstresults[,1],tstresults[,2])

nbtr.tstbl

table(tstresults[,1],tstresults[,2])

getMetrics<-function(actual_class,predicted_response)
{
  test.set=data.frame(target=actual_class,prediction=predicted_response)
  TPrates = c()
TNrates = c()
thresholds = seq(0, 1, by = 0.05)
total_positive=nrow(test.set[test.set$target==0 ,])
total_negative=nrow(test.set[test.set$target==1,])
for (threshold in thresholds) {
  TPrateForThisThreshold = nrow(test.set[test.set$target == 0 & test.set$prediction <= threshold,])/total_positive
  TNrateForThisThreshold = nrow(test.set[test.set$target == 1 & test.set$prediction > threshold,])/total_negative
  TPrates = c(TPrates, TPrateForThisThreshold)
  TNrates = c(TNrates, TNrateForThisThreshold)
}
if(!require(pracma))install.packages('pracma')
library(pracma)
AUC <- trapz(TPrates,TNrates)
X=list(fpr=1-TNrates,tpr=TPrates,auc=AUC)
X

}

L<-getMetrics(tstcatheart[[9]],roc.nbtr.tspred)
plot(L$fpr, L$tpr, type="l",main=" NB ROC Plot tpr vs fpr")
#plot(L$fpr,L$tpr,main=" NB ROC Plot tpr vs fpr")
print(paste(" NB AUC=",L$auc,sep=''))
