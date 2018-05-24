

library(caret)
library(tidyr)
library(plyr)
library(dplyr)
library(caTools)
library(reshape2)
library(gbm)
library(caTools)
library(randomForest)
##remove all objects in the previous run
rm(list=ls(all=TRUE))

  ## Read given files
 et=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\event_type1.csv")
 rt=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\resource_type1.csv")
 st=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\severity_type1.csv")
 lf=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\log_feature1.csv")
 train=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\train.csv")
 test=read.csv("E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\test.csv")
 
 ###making subsdiary files wide 
 
 ##Event type preprocessing
 
 et=et[order(et$id),]
 et_wide <- et %>% mutate(i=1) %>% spread(event_type,i,fill=0)
names(et_wide) <- gsub(pattern = "event_type ",replacement = "ET_",x = names(et_wide))

c_et <- aggregate(ind ~ id ,data = cbind(et,ind=1),FUN = sum)
et_wide <- cbind(et_wide,c_et$ind)
#-----------------------------------------------------------------------------------------
et.overall <- cbind(id=et$id,et_ov=as.numeric(gsub("event_type",replacement = "",x = et$event_type)))
a <- aggregate(et_ov~id , data = et.overall,FUN = sum)
et_wide <- cbind(et_wide,a$et_ov)
et_wide$et_ratio = et_wide$`a$et_ov`/et_wide$`c_et$ind`
#---------------------------------------------------------------------------------------------------

##Resource type preprocessing 

rt=rt[order(rt$id),]
rt_wide=rt %>%mutate(i=1) %>% spread(resource_type,i,fill=0)
names(rt_wide)=gsub(pattern="resource_type",replacement="RT_",x=names(rt_wide))

c_rt=aggregate(ind ~ id,data=cbind(rt,ind=1),FUN=sum)
rt_wide=cbind(rt_wide,c_rt$ind)
#---------------------------------------------------------------------------
rt.overall=cbind(id=rt$id,rt_ov=as.numeric(gsub("resource_type",replacement="",x=rt$resource_type)))
a=aggregate(rt_ov ~ id,data=rt.overall,FUN=sum)
rt_wide=cbind(rt_wide,a$rt_ov)
rt_wide$rt_ratio=rt_wide$`a$rt_ov`/rt_wide$`c_rt$ind`
#------------------------------------------------------------------------------


##severity type preprocessing 

st=st[order(st$id),]
st_wide=st %>% mutate(i=1) %>% spread(severity_type,i,fill=0)
names(st_wide)=gsub(pattern="resource_type",replacement="ST_",x=names(st_wide))

c_st=aggregate(ind ~ id,data=cbind(st,ind=1),FUN=sum)
st_wide=cbind(st_wide,c_st$ind)
#---------------------------------------------------------------------
st.overall=cbind(id=st$id,st_ov=as.numeric(gsub("severity_type",replacement="",x=st$severity_type)))
a=aggregate(st_ov ~ id,data=st.overall,FUN=sum)
st_wide=cbind(st_wide,a$st_ov)
st_wide$st_ratio=st_wide$`a$st_ov`/st_wide$`c_st$ind`


##Log feature type preprocessing 

c_lf <- aggregate(ind ~ id ,data = cbind(lf,ind=1),FUN = sum)
a<- cbind(lf[,-2],(lf$volume)*model.matrix( ~ -1 + log_feature,data=lf))
a<-a[,-2]
lf_wide <- aggregate(. ~ id,data = a,FUN = sum)
lf_wide <- cbind(lf_wide,c_lf$ind)
#-----------------------------------------------------------------------------------------
lf.overall <- cbind(id=lf$id,lf_ov=as.numeric(gsub("feature",replacement = "",x = lf$log_feature)))
a <- aggregate(lf_ov~id , data =lf.overall,FUN = sum)
lf_wide <- cbind(lf_wide,a$lf_ov)

lf_wide$lf_ratio = lf_wide$`a$lf_ov`/lf_wide$`c_lf$ind`
names(lf_wide)=gsub(pattern="log_featurefeature ",replacement="LF_",x=names(lf_wide))


####train test combining and adding more features 
train["ind"]="train"
test["ind"]="test"
test["fault_severity"]=NA
full=rbind(train,test)
###########m=merge(full,st)

##########full=merge(full,m[,c(1,7)],by="id")

##Raw merge with mutiple rows for each id 


all=merge(full,merge(rt,merge(et,merge(st,lf))))

## max avg volume group  by id et rt
a=aggregate(volume~id+event_type+resource_type,data=all,FUN = mean)
a=aggregate(volume~id,data=a,FUN = max)
names(a)[2]="c_stt"


### count of severity type,rt,lf,et per id 

aa=(as.matrix(table(all$id,all$severity_type)))
aa=cbind(aa,id=as.numeric(rownames(aa)))

bb=(as.matrix(table(all$id,all$resource_type)))
bb=cbind(bb,id=as.numeric(rownames(bb)))

cc=(as.matrix(table(all$id,all$event_type)))
cc=cbind(cc,id=as.numeric(rownames(cc)))

dd=(as.matrix(table(all$id,all$log_feature)))
dd=cbind(dd,id=as.numeric(rownames(dd)))


### merge by id single row for each id 

temp <- merge(x = et_wide,y = rt_wide,by = "id")
temp <- merge(x = temp,y = st_wide,by = "id")
temp <- merge(x = temp,y = lf_wide,by = "id")

#### getting supporting features for just ids present in train and test set
full <- merge(x = full,y = temp,by = "id")

###adding Total Count of st,rt,st,lf by id to the dataset

full=merge(full,aa,by="id")
# full=merge(full,bb,by="id")
# full=merge(full,cc,by="id")
# full=merge(full,dd,by="id")

### adding max(avg(volume)) group by id,et,rt only to the dataset

full=merge(full,a,by="id")

###Adding location count to the dataset -how many times a location appears in the dataset 
loc_table=(data.frame(table(full$location)))
names(loc_table)[1]="location"
full =merge(x = full,y = loc_table,by = "location")

full$location <- as.numeric(gsub("location",replacement = "",x = full$location))


charTofac <- function (x){
  if (class(x)=="character")
    x<- factor(x)
  return(x)}

intTonum <- function (x){
  if (class(x)=="integer")
    x<- as.numeric(x)
  return(x)}

full1 <- sapply(full[],charTofac)
full1 <- as.data.frame(full1)
full1 <- intTonum(full1)

### divide full set using clustering
cluster <- kmeans(x = full1[,-c(2,3,4)],iter.max = 1000,centers = 4)
full1["cluster"] <- cluster$cluster

###Multi collinearity matrix -remove those with correlation of more than 0.99
tmp <- cor(full1)
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
remove=names(which(sapply(as.data.frame(tmp),function(x) any(abs(x)>0.99))=="TRUE")) 


### taking out TRAIN SET from full set where fault_severity is known
X = full1[full1$ind==2,]

X = full1[full1$ind==2,-c(4)]

feature.namestrain = setdiff(names(X),c("id","ind",remove))
write.csv(feature.namestrain,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\featurenames.csv")

X$fault_severity=as.factor(make.names(X$fault_severity))

X$fault_severity=as.numeric(X$fault_severity)-1
y =X$fault_severity

#write.csv(X,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\X.csv")
write.csv(X,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\y.csv")

Final_Trainset=X[,feature.namestrain]

spltvar=sample.split(Final_Trainset$fault_severity,SplitRatio=0.7)

Actual_Trainset=subset(Final_Trainset,spltvar==TRUE)
Actual_Validateset=subset(Final_Trainset,spltvar==FALSE)


###taking out TEST SET 
X_test = full1[full1$ind==1,-c(3,4)]
feature.namestest = setdiff(names(X),c("id","ind","fault_severity",remove))

write.csv(X,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\X.csv")
write.csv(X_test,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\X_test.csv")





set.seed(1)


###gbmgrid=expand.grid(interaction.depth=c(1,3),shrinkage=seq(.0005,.005,.0005),n.minobsinnode=10,n.trees=(0:10)*20)
###tgrid=expand.grid(mtry=216)

##trctrlobj=trainControl(method="cv",number=3,verboseIter=FALSE,classProbs=TRUE)
###trControlobj=trainControl(method="cv",number=3)


##rf_model=train(fault_severity ~ .,data=Actual_Trainset,
##method="rf",
##trControl=trControlobj,
##metric="Accuracy"
##)
h=c(sample(which(y==0),956),sample(which(y==1),374),sample(which(y==2),145))
dval<-xgb.DMatrix(data=data.matrix(X[h,feature.namestrain]),label=y[h])
dtrain<-xgb.DMatrix(data=data.matrix(X[,feature.namestrain]),label=y)
watchlist<-list(val=dval,train=dtrain)

set.seed(1)
xgb_model<- xgb.train(data = dtrain,nfold=5,
                eta = 0.04,#0.04
                max_depth = 6,#6 
                nround=446, 
                subsample = 0.75,#0.65
                colsample_bytree = 0.75,#0.65
                eval_metric = "mlogloss",
                objective = "multi:softprob",
                num_class = 3,
                nthread = 16,
                num_parallel_trees=1000,
                early.stopping.rounds = 50,
                watchlist = watchlist,
                verbose=1 ,gamma=0
                )




#Printing model summary to a file 
out=capture.output(xgb_model)
cat("Summary of rf_model",out,file="E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\modelsummary.txt",sep="\n",append=TRUE)


##gbm_model=train(fault_severity ~ .,data=Actual_Trainset,
##method="gbm",metric="ROC",
##distribution="multinomial",
##trControl=trctrlobj,
##verbose=FALSE,
##tuneGrid=gbmgrid)

##Making predictions on validate set where results are known 

predknown=predict(xgb_model,newdata=Actual_Validateset,ntree=xgb_model$bestInd)

##Print accuracy
#acc=table(predknown,Actual_Validateset$fault_severity)
#write.csv(acc,"E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\final train and test set\\Accuracy.csv")




##making predictions (on unseen testdata set) with best model raw predicts class and prob gives probabilities
pred=predict(xgb_model,newdata=data.matrix(X_test[,feature.namestest]),ntree=xgb_model$bestInd)
predictions=as.data.frame(matrix(pred,ncol=3,byrow=T))
colnames(predictions)=c("predict_0","predict_1","predict_2")
submiss=NULL
submiss$id=X_test$id
submiss=cbind(submiss,predictions)


##making submission file
##mysub=data.frame(id=X_test$id,predclass=pred)
##ysub1=mysub %>% mutate(i=1) %>% spread(predclass,i,fill=0)
#colnames(mysub1)=c("id","predict_0","predict_1","predict_2")
mysub1=as.data.frame(submiss)
write.csv(mysub1,'E:\\AbhinavB\\Kaggle\\Telstra network disruptions\\submission files\\submission1_Feb16_xgboost.csv')
 
###END 



