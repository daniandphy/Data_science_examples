library("tree")
library("randomForest")

load("samsungData.rda")
source("fixing_names.r")
fixed.name.mapping <- fixed.name.mapping.samsungData(samsungData)

# check for duplicates, should not find any and return character(0)
names(fixed.name.mapping)[duplicated(names(fixed.name.mapping))]

# update the names
names(samsungData) <- names(fixed.name.mapping)
samsungData$activity<-as.factor(samsungData$activity)
#initial_model<-lm(activity~.,data=samsungData)
#summary(initial_model)

#step(initial_model,direction="backward")
###making test and training sets
all_subjects<-unique(samsungData$subject)
#7,8,11,14,15,16,17,19,21 ,22 ,23 ,25,26,
test_subjects<-c(27,28,29,30)
train_subjects<-setdiff(all_subjects,test_subjects)

test_samsungData<-samsungData[is.element(samsungData$subject,test_subjects),]
train_samsungData<-samsungData[is.element(samsungData$subject,train_subjects),]
variables_list=names(samsungData)[names(samsungData)!="activity"]
fit_formula<-as.formula(paste("activity ~ ",paste(variables_list,collapse="+")))
#train_tree<-tree(activity~.,data=train_samsungData)
train_tree<-tree(fit_formula,data=train_samsungData)
#### decide the best tree size using cross validation method

par(mfrow=c(1,2))
plot(cv.tree(train_tree))
plot(cv.tree(train_tree,FUN=prune.tree,method="misclass"))
#######calculate misclassification for different tree sizes
for (i in 4:9){
prune_tree<-prune.tree(train_tree,best=i)
#predict_tree<-predict(prune_tree,test_samsungData)
pdf(file=paste0("prune_tree_",i,".pdf"))
plot(prune_tree,type="uniform") ##"uniform" makes the length of branch equal
text(prune_tree,col=rainbow(8)[1:20],srt=7,cex=0.8,font=2)
dev.off()
### train_misclass
train_misclass_result<-paste("train_misclass for best=",i,"is",misclass.tree(prune_tree))
print(train_misclass_result)
### test_misclass
test_expectation=predict(prune_tree,newdata=test_samsungData,type="class")
test_misclass<-sum(as.numeric(test_expectation!=test_samsungData$activity))
test_misclass_result<-paste("test_misclass for best=",i,"is",test_misclass)
print(test_misclass_result)
}
#table(test_samsungData$activity,predict(prune_tree,newdata=test_samsungData,type="class"))
#####random forest ignore for now
#randomf_fit<-randomForest(activity~.,data=samsungData)
#importance(randomf_fit)