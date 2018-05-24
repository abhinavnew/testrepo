

library(caret)
library(tidyr)
library(plyr)
library(dplyr)
library(caTools)
library(reshape2)
library(gbm)
library(caTools)
library(randomForest)
library(ggplot2)

##scientific notation off for the session
options(scipen = 999)



 ## Read given files
 app_event=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\app_events\\app_events.csv")
 app_labels=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\app_labels\\app_labels.csv")
 events=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\events\\events.csv",header = TRUE)
 label_categories=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\label_categories\\label_categories.csv")
 ph_bd_dev_model=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\phone_brand_device_model\\phone_brand_device_model.csv")
  tdtrain=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\gender_age_train\\gender_age_train.csv")
 tdtest=read.csv("E:\\AbhinavB\\Kaggle\\TalkingData\\gender_age_test\\gender_age_test.csv")
 
 head(app_event)
 head(app_labels)
 head(events)
 head(label_categories)
 head(ph_bd_dev_model)
 head(tdtrain)
 head(tdtest)
 
 
 colnames(ph_bd_dev_model)[colSums(is.na(ph_bd_dev_model))>0]
 colnames(events)[colSums(is.na(events))>0]
 colnames(app_event)[colSums(is.na(app_event))>0]
 colnames(app_labels)[colSums(is.na(app_labels))>0]
 colnames(label_categories)[colSums(is.na(label_categories))>0]
 colnames(tdtrain)[colSums(is.na(tdtrain))>0]
 colnames(tdtest)[colSums(is.na(tdtest))>0]
 
 table(app_event$is_installed)
 
 
 str(label_categories)
 
 unique(label_categories$category)
 dim(app_labels)
 
 dim(ph_bd_dev_model)
 

 

 ## converting factor brand and model to numeric values and re converting to factor
 
 ph_bd_dev_model$numbrand=as.numeric(factor(ph_bd_dev_model$phone_brand,levels=levels(ph_bd_dev_model$phone_brand)))
 ph_bd_dev_model$nummodel=as.numeric(factor(ph_bd_dev_model$device_model,levels=levels(ph_bd_dev_model$device_model)))
 ph_bd_dev_model$numbrand=as.factor(ph_bd_dev_model$numbrand)
 ph_bd_dev_model$nummodel=as.factor(ph_bd_dev_model$nummodel)
 label_categories$numCategories=as.numeric(factor(label_categories$category,levels = levels(label_categories$category)))
 label_categories$numCategories=as.factor(label_categories$numCategories)
 str(ph_bd_dev_model)
 str(label_categories)
  dim(tdtrain)
 dim(ph_bd_dev_model)
 
 ##removing duplicates from testset
 


tdtest1=distinct(tdtest)
 
 class(tdtest1)
 head(tdtest1)
 dim(tdtest1)
    
 

 ##Adding train + test set and preparing a full set
 tdtrain$ind="train"
 tdtest1$gender=NA
 tdtest1$age=NA
 tdtest1$group=NA
 tdtest1$ind="test"
 
 fullset=rbind(tdtrain,tdtest1)
 dim(fullset)
 head(fullset)
 dim(tdtest1)
 dim(tdtrain)
 
 
 ##Merging trainset+testset  with phonebrand and phoneModels
 
 TrainWithPh=merge(x=fullset,y=ph_bd_dev_model,by="device_id",all.x = TRUE)
 dim(fullset)
 dim(tdtrain)
 dim(tdtest1)
 dim(ph_bd_dev_model)
 dim(TrainWithPh)
 
 
 ## dealing with duplicates 
 
 ##a=TrainWithPh[duplicated(TrainWithPh),]
 ####head(a)
 ##dim(a)
 
 ##remove duplicates
 TrainWithph=distinct(TrainWithPh)
 dim(TrainWithph)
##Merging with Events to get app details 
 
 TrainWithEvents=merge(x=TrainWithPh,y=events,by="device_id",all.x = TRUE)
 dim(TrainWithEvents)
 
 #######################################################################################
 
 ##making relevant set where event id is Not NA
##TrainWithEvents_rel=subset(TrainWithEvents,is.na(TrainWithEvents$event_id)==FALSE)

##merging reduced set with app events
##TrainWithAppevents=merge(x=TrainWithEvents_rel,y=app_event,by="event_id",all.x = TRUE)

 ##merging with app events but full set as we dont want to loose any device id
 
TrainWithAppevents=merge(x=TrainWithEvents,y=app_event,by="event_id",all.x = TRUE)

##making relevant set where app id is not NA
##TrainWithAppevents_rel=subset(TrainWithAppevents,is.na(TrainWithAppevents$app_id)==FALSE)

## only getting records where apps are active

##TrainWithAppevents_rel2=subset(TrainWithAppevents_rel,TrainWithAppevents_rel$is_active==1)

##getting labels by merging labels master table
 
#############TrainWithLabels=merge(x=TrainWithAppevents_rel2,y=app_labels,by="app_id",all.x = TRUE)

##merges are failing for low memory,have to check the effect of timestamp/lat/long on Dependent variable
 

##removing timestamp/long/lat/is_installed/is_active columns from the final train set and then finding duplicates
##TrainWithAppevents_rel3=TrainWithAppevents_rel2[,-c(10,11,12)]
##TrainWithAppevents_rel3=TrainWithAppevents_rel3[,-c(11,12)]

##write.csv(TrainWithAppevents_rel3,"E:\\AbhinavB\\Kaggle\\TalkingData\\rel3file_chkforDuplicates.csv")

##TrainWithAppevents_rel3[,TrainWithAppevents_rel3$device_id==1946302634062518016]
##TrainWithAppevents_rel3[,TrainWithAppevents_rel3$event_id=="592"]
##subset(TrainWithAppevents_rel3,TrainWithAppevents_rel3$device_id==1946302634062518016 & TrainWithAppevents_rel3$event_id==592 & TrainWithAppevents_rel3$app_id==8693964245073640448)

###finding any duplicates 


##TrainWithAppevents_rel3[duplicated(TrainWithAppevents_rel3),]
##dim(TrainWithppevents_rel3)

##remove duplicates
##TrainWithAppevents_rel4=distinct(TrainWithAppevents_rel3)
TrainWithAppevents_rel4=distinct(TrainWithAppevents)
dim(TrainWithAppevents_rel4)

##getting labels by merging labels master table

##TrainWithLabels=merge(x=TrainWithAppevents_rel4,y=app_labels,by="app_id",all.x = TRUE)

### merges are still failing ...need to find another way 
##app_label_categories=merge(x=app_labels,y=label_categories,by="label_id",all.x = TRUE)

### merging using dplyr::filter

##temp<-filter(TrainWithAppevents_rel4, app_id %in% app_label_categories$app_id)
##TrainWithLabels=left_join(temp,app_label_categories,by="app_id")
library(dplyr)
##temp2=filter(TrainWithAppevents_rel4,app_id %in% app_labels$app_id)
##dropping unnecessary columns
##temp2_rel=temp2[,-c(1,7,8,11,12,13,15)]

temp2_rel=TrainWithAppevents_rel4[,-c(1,7,8,11,12,13,15)]

##removing duplicates now without losing any device id

temp2_rel2=distinct(temp2_rel)
dim(temp2_rel2)

##Now joining reduced set with labels

##tmp=filter(temp2_rel2,device_id %in% fullset$device_id )
##length(unique(tmp$device_id))

temp3=left_join(temp2_rel2,app_labels,by="app_id")
length(unique(temp3$device_id))

##temp4=temp3[,-c(1,6,7,10)]
##temp4_uniq=temp4[!duplicated(temp4),]

##find unique rows based on all columns using dplyr otherwise fails with duplicated
temp4=distinct(temp3)
length(unique(temp4$device_id))


## NOW joining to get label_categories (master table)  in numerical form 

 temp5=left_join(temp4,label_categories,by="label_id")
 length(unique(temp5$device_id))
 head(temp5)
 dim(temp5)
 
 ## how to remove unnecc colums without losing any uique device id 
 
 temp5_rel=temp5[,-c(10,11)]
 dim(temp5_rel)
 head(temp5_rel)
 length(unique(temp5_rel$device_id))
 
 

 
full_activeset=temp5_rel 

## add code to update NA where Is_active =0
   
full_activeset %>% mutate(full_activeset$numCategories=(ifelse(is_active==0,"NA",numCategories)))

##remove duplicates without losing any device id 

full_activeset1=distinct(full_activeset)
full_activeset2=full_activeset1[,-c(8,9)]
full_activeset3=distinct(full_activeset2)

### making categories wide so that there is one row per device id 


full_wide=full_activeset3 %>%mutate(i=1) %>% spread(numCategories,i,fill=0)

##failing due to memory issues

train_fin=full_activeset3[full_activeset3$ind=="train",]

train_wide=train_fin %>% mutate(i=1) %>% spread(numCategories,i,fill=0)













  
   
   
   ########################################################

> table(full_activeset$is_active)

##0        1 
13233464  5527760 
> nrow(full_activeset)
[1] 18917680
> (13233464+5527760)-18917680
[1] -156456##/###


   
    
 ## finding rows where lattitude and longitude of the data call/event is zero 
 a=subset(TrainWithAppevents_rel2,TrainWithAppevents_rel2$longitude==0 & TrainWithAppevents_rel2$latitude==0)
 dim(a)
 ## 3031846      15
 
 b=subset(TrainWithAppevents_rel2,TrainWithAppevents_rel2$longitude!=0 & TrainWithAppevents_rel2$latitude!=0)
 write.csv(b,"E:\\AbhinavB\\Kaggle\\TalkingData\\b.csv")
 
 ##Extracting the timepart and plotting against DV to see if there is any relation
 
 a$timepart=format(as.POSIXct(strptime(a$timestamp,"%Y-%m-%d %H:%M:%S",tz="")),format="%H:%M:%S")
 a$hourpart=format(as.POSIXct(strptime(a$timestamp,"%Y-%m-%d %H:%M:%S",tz="")),format="%H")
 a$Geoloc=paste(a$latitude,a$longitude,sep="_")
 aFemale=subset(a,a$gender=="F")
 aMale=subset(a,a$gender=="M")
 
 ggplot(aes(x=hourpart,y=group),data=aFemale)+geom_point()
 ggplot(aes(x=hourpart,y=group),data=aMale)+geom_point(alpha=0.3)
 ggplot(aes(x=Geoloc,y=group),data=aMale)+geom_point(alpha=0.3)
 ggplot(aes(x=Geoloc,y=group),data=aFemale)+geom_point(alpha=0.3)
 
 ggplot(data=deputies_small,aes(x=receipt_description,y=receipt_value))+geom_boxplot()+coord_flip()
 
 colnames(deputies_small)
 
