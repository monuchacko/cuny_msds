
heart<-read.csv('C:/Users/rkannan/rk/data/heart.csv',sep=',',head=T,
                stringsAsFactors=F)
unlist(lapply(names(heart),FUN=function(x,data=heart){c(cname=x,uvalfreq=length(unique(data[[x]])))}))

nvpairsdf3<-do.call('rbind',lapply(names(heart),FUN=function(x,data=heart){c(cname=x,uvalfreq=length(unique(data[[x]])))}))

categoricalFeatures<-function(dset,ncol=7)
{
  df4<-as.data.frame(do.call('rbind',lapply(names(dset),FUN=function(x,data=dset){c(cname=x,uvalfreq=length(unique(data[[x]])))})))
  df4$uvalfreq=as.numeric(df4$uvalfreq)
  dset[,df4$uvalfreq<=ncol]
}

X<-categoricalFeatures(heart)

table(heart$target)
dim(X)

heart0<-X[X$target==0,]
heart1<-X[X$target==1,]
names(heart0)

heart0mean<-apply(heart0[,1:(ncol(X)-1)],2,mean)
heart1mean<-apply(heart1[,1:(ncol(X)-1)],2,mean)

allclassmean<-apply(X[,1:(ncol(X)-1)],2,mean)

S1<-cov(heart0[,1:(ncol(X)-1)])
S2<-cov(heart1[,1:(ncol(X)-1)])

N1<-nrow(heart0)
N2<-nrow(heart1)

SWpooled<-((N1-1)*S1+(N2-1)*S2)/(N1+N2-2)

SB1<-N1*(heart0mean-allclassmean)*(heart0mean-allclassmean)
SB2<-N2*(heart1mean-allclassmean)*(heart1mean-allclassmean)
SB<-SB1+SB2

invSWpooled<-solve(SWpooled)
invSWSBpooled<-invSWpooled*SB
SWSBpooledeigendecomp<-eigen(invSWSBpooled)
lambdas<-SWSBpooledeigendecomp$vectors[,which.max(SWSBpooledeigendecomp$values)]
as.matrix(X[,1:(ncol(X)-1)])%*% lambdas->heart.projections.pooled

rounded.heart.projections.pooled<-round(heart.projections.pooled)
heart.lda.pooled<-cbind(X,classp=heart.projections.pooled,spectral=rounded.heart.projections.pooled)
heart.lda.pooled$predicted <-ifelse(heart.lda.pooled$spectral < -1,1,0)
heart.lda.confMat.pooled<-table(  heart.lda.pooled$target,heart.lda.pooled$predicted)
heart.lda.confMat.pooled

print(paste("Accuracy is::",
            as.numeric(sum(diag(heart.lda.confMat.pooled))/sum(heart.lda.confMat.pooled)),sep= " "))
print(paste("Error is::",
            1-as.numeric(sum(diag(heart.lda.confMat.pooled))/sum(heart.lda.confMat.pooled)),
            sep= " "))
plot(heart.lda.pooled$classp,col=as.factor(heart.lda.pooled$predicted),pch=16)

default_pos<-X[X[,9]==0,]
default_neg<-X[X[,9]==1,]
dim(default_pos)
mu_pos<-apply(default_pos[,1:8],2,mean)
mu_neg<-apply(default_neg[,1:8],2,mean)
sigma_pos<-cov(default_pos[,1:8])
sigma_neg<-cov(default_neg[,1:8])
n_pos<-dim(default_pos)[1]
n_neg<-dim(default_neg)[1]
sigma_all<-((n_pos-1)*sigma_pos+(n_neg-1)*sigma_neg)/(n_pos+n_neg-2) #pooled Cov matrix-
mu<-cbind(mu_pos,mu_neg)
pi.vec <- rep(0,2) # why not just pi?
pi.vec[1] <- sum((heart$target==0))/nrow(heart)
pi.vec[2] <- sum((heart$target==1))/nrow(heart)

my.lda <- function(pi.vec,mu,Sigma,x){
x.dims <- dim(x)
n <- x.dims[1]
Sigma.inv <- solve(Sigma) #Find inverse of Sigma
out.prod <- rep(0,n) #all items initiated to be negative
# equation 4.19 from ISLR Sixth Printing Page 157
discrim.pos <- apply(x,1,function(y) y %*% Sigma.inv %*% mu[,1]
- 0.5*t(mu[,1]) %*% Sigma.inv %*% mu[,1] + log(pi.vec[1])) # target == 0
discrim.neg <- apply(x,1,function(y) y %*% Sigma.inv %*% mu[,2]
- 0.5*t(mu[,2]) %*% Sigma.inv %*% mu[,2] + log(pi.vec[2]))# target == 1 
out.prod[discrim.neg >= discrim.pos] <- 1
out.prod.df<-data.frame(posp=discrim.pos,negp=discrim.neg,prod=out.prod)
return(out.prod.df)
}

heart_default_all<-my.lda(pi.vec,mu,sigma_all,X[,1:8])
hda<- heart_default_all
hda$posprob<-hda$posp/(hda$posp+hda$negp)
hda$negprob<-hda$negp/(hda$posp+hda$negp)
#produce confusion matrices
table(X$target,heart_default_all$prod)

cfmx<-caret::confusionMatrix(table(X$target,heart_default_all$prod))

require(MASS)
lda_model<-lda(target~.,data=X,type='response')
lda_pred<-predict(lda_model,X[,1:8])
table(X$target,lda_pred$class)
cfmx_mass<-caret::confusionMatrix(table(X$target,lda_pred$class))

dfcomp<-data.frame(sid=cfmx$byClass,mass=cfmx_mass$byClass)
dfcomp

require(ROCR)
pred <- prediction(lda_pred$posterior[,2], X$target)
AUC<-performance(pred,"auc")
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
text(0.8,0.2,paste("AUC=",round(AUC@y.values[[1]],4),sep=''))
