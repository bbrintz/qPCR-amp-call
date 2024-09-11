#Load libraries

library(tidyverse)
library(xgboost)

#Set to your working directory
setwd('XXX')

# Used if running this on a cluster 
#each=as.numeric(commandArgs(T))

#Load training data 
# Save in github as .rds files for size considerations 
dat2=read_csv('qpcr_training_data.csv')
dat=read_csv('raw_training.csv')

#Format data for analysis 
dat=dat %>% gather(position,value,-cycle,-card,-dye) %>% pivot_wider(names_from=cycle,values_from=value)
dat=dat %>% left_join(dat2 %>% dplyr::select(card,position,dye,truth,ct),by=c("position","card","dye"))
dat = dat %>% filter(!is.na(truth))
dat = dat %>% mutate(truth=as.factor(truth))
dat = dat %>% mutate(ct=case_when(ct=="Undetermined"~40,TRUE~as.numeric(ct)))
dat %>% group_by(card,position) %>% mutate(row=row_number()) %>% summarize(duplex=sum(any(row==2))) %>% ungroup() %>% summarize(sum(duplex))
dat = dat %>% ungroup() %>% dplyr::select(-card,-position,-dye)
colnames(dat)[1:40]=paste0("Cyc",1:40)

#Read in additional niger data (unable to save in github due to size, please reach out to author for file)
newdat=read_csv('niger_final.csv')
dat=rbind(dat,newdat %>% select(starts_with("Cyc"),truth,ct=Ct))
dat=dat %>% filter(!is.na(truth))

#Normalize data: I want the minimum of the row, not of each element, using rowwise
dat=dat %>%
  rowwise() %>%
  mutate(min_value = min(c_across(starts_with("Cyc"))-1, na.rm = TRUE)) %>%
    ungroup() %>%
  mutate(across(starts_with("Cyc"),~.-min_value)) %>% select(-min_value)

#Training and testing split - saved previously and loaded here for reproducibility
idx=readRDS(file="main_idx_vNiger.rds")

train=dat[idx,]
test=dat[-idx,]  

# Hyperparameter tuning grid
m <- expand.grid(
    eta = c(.1,.3,.5,.7),
    max_depth = 10:18,
    min_child_weight = c(1,5,10),
    subsample = c(0.5, 0.7, 1),
    gamma = c(0, 0.1, 0.2),
    colsample_bytree = c(0.5, 0.7, 1)
)

# Function to run cross-validation on each hyperparameter set
runCV=function(x1,x2,x3,x4,x5,x6){
  eta=x1
  max_depth=x2
  min_child_weight=x3
  subsample=x4
  gamma=x5
  colsample_bytree=x6
  param <- list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "error")
  
  xgbcv <- xgb.cv(params = param,
                  data = train %>% select(starts_with("Cyc")) %>% as.matrix,
                  label = as.numeric(train$truth)-1,
                  nround = 50,
                  nfold = 10,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = T,
                  maximize = F,
                  nthread=1)


  param2 <- list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "mae")
  train2 = train %>% filter(truth==1)
  xgbcv2 <- xgb.cv(params = param2,
                  data = train2 %>% select(starts_with("Cyc")) %>% as.matrix,
                  label = as.numeric(train2$ct),
                  nround = 50,
                  nfold = 10,
                  prediction = TRUE,
                  early_stopping_rounds = 5,
                  verbose = T,
                  maximize = F,
                  nthread=1)#2)

best1=xgbcv$evaluation_log[xgbcv$best_iteration,]$test_error_mean
best2=xgbcv2$evaluation_log[xgbcv2$best_iteration,]$test_mae_mean
                  
return(c(best1,best2))
}

#Ran on cluster and split up for tasks but simplified here for local running 

out=1:2916 %>% purrr::map(function(iter){
   print(iter)
   x=m[iter,]
   out=runCV(x[1],x[2],x[3],x[4],x[5],x[6])
 })


###Analysis

# Final model fitting, re-ran CV to get best round for each model (could have been saved above)

eta=0.5
max_depth=13
min_child_weight=1
subsample=.7
gamma=0.1
colsample_bytree=.7
  param <- list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "error")
  
  xgbcv <- xgb.cv(params = param,
                  data = train %>% select(starts_with("Cyc")) %>% as.matrix,
                  label = as.numeric(train$truth)-1,
                  nround = 50,
                  nfold = 10,
                  prediction = F,
                  early_stopping_rounds = 5,
                  verbose = T,
                  maximize = F)#2)

eta=0.1
max_depth=16
min_child_weight=10
subsample=1
gamma=0.2
colsample_bytree=1

  param2 <- list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample,
    gamma = gamma,
    colsample_bytree = colsample_bytree,
    booster = "gbtree",
    objective = "reg:squarederror",
    eval_metric = "mae")
  train2 = train %>% filter(truth==1)
  xgbcv2 <- xgb.cv(params = param2,
                  data = train2 %>% select(starts_with("Cyc")) %>% as.matrix,
                  label = as.numeric(train2$ct),
                  nround = 50,
                  nfold = 10,
                  prediction = F,
                  early_stopping_rounds = 5,
                  verbose = T,
                  maximize = F)#2)


#nround=25
#nround=100

# Fit model on training data to test on test data 

mod1=xgb.train(param=param,data=xgb.DMatrix(train %>% select(starts_with("Cyc")) %>% as.matrix,label=as.numeric(train$truth)-1),
                nrounds=17,verbose=T)
mod2=xgb.train(param=param2,data=xgb.DMatrix(train2 %>% select(starts_with("Cyc")) %>% as.matrix,label=as.numeric(train2$ct)),
                nrounds=37,verbose=T)
saveRDS(mod1,"mod1_xgboost_v3.rds")
saveRDS(mod2,"mod2_xgboost_v3.rds")

dat_true = dat %>% filter(truth==1)

# Fit model on full data to test on external data 


mod3=xgb.train(param=param,data=xgb.DMatrix(dat %>% select(starts_with("Cyc")) %>% as.matrix,label=as.numeric(dat$truth)-1),
                nrounds=17,verbose=T)
mod4=xgb.train(param=param2,data=xgb.DMatrix(dat_true %>% select(starts_with("Cyc")) %>% as.matrix,label=as.numeric(dat_true$ct)),
                nrounds=37,verbose=T)
saveRDS(mod3,"mod3_xgboost_v3.rds")
saveRDS(mod4,"mod4_xgboost_v3.rds")


# Internal Validation Results

mod=readRDS("mod1_xgboost_v3.rds")#mods[[1]]
mod2=readRDS("mod2_xgboost_v3.rds")#mods[[2]]
pred=as.factor(as.numeric(predict(mod,test %>% select(starts_with("Cyc")) %>% as.matrix)>.5))
pred2=predict(mod2,test %>% select(starts_with("Cyc")) %>% as.matrix)
truth=as.factor(test$truth)
tab=table(truth,pred)
colnames(tab) = c("No amp.","Amp.")
row.names(tab) = c("No amp.","Amp.")
rownames(tab) <- paste("Truth:", rownames(tab))
colnames(tab) <- paste("Predicted:", colnames(tab))

# Assuming `tab` is your confusion matrix or table
# First, convert your matrix to a data frame if it's not already
tab_df <- as.data.frame.matrix(tab)

# Add an extra column at the beginning for the row labels (truth)
tab_df <- cbind(Truth = rownames(tab_df), tab_df)

# Adjust column names to include 'pred' as part of the name
colnames(tab_df)[-1] <- paste("Pred -", colnames(tab_df)[-1])

# External Validation Results

mod=readRDS(file="mod3_xgboost_v3.rds")
mod2=readRDS(file="mod4_xgboost_v3.rds")#readRDS(file="ranger_mod2_vF.rds")#

input_data=bind_rows(1:3 %>% purrr::map(function(x) read_csv(paste0("file",x,".csv")) %>% mutate(file=x)))
maled=bind_rows(1:3 %>% purrr::map(function(x) read_csv(paste0("EQA file",x," results.csv")) %>% mutate(file=x)))

dye_target=read_csv("dye_target_corrected.csv")

input_data=input_data %>% gather(dye,value,-Well,-`Well Position`,-Cycle,-ROX,-file) %>%
  mutate(value=value/ROX) %>% select(cycle=Cycle,well=Well,value,`Well Position`,dye,file) %>% 
  pivot_wider(names_from=cycle,values_from=value) #%>% select(-well,-`Well Position`)

colnames(input_data)[5:44] = paste0("Cyc",1:40)

input_data=input_data %>% na.omit()

input_data=input_data %>%
  rowwise() %>%
  mutate(min_value = min(c_across(starts_with("Cyc"))-1, na.rm = TRUE)) %>%
    ungroup() %>%
  mutate(across(starts_with("Cyc"),~.-min_value)) %>% select(-min_value)

prediction = as.factor(as.numeric(predict(mod, input_data %>% select(-well,-`Well Position`,-dye,-file) %>% as.matrix)>0.5))

idx_pred=which(prediction==1)
pred2=rep(NA,length(prediction))
pred2[idx_pred]=predict(mod2,input_data[idx_pred,] %>% select(starts_with("Cyc")) %>% as.matrix)

dyes=c("FAM","VIC")

maled=maled %>% mutate(dye=dye_target$corrected[match(`Target Name`,dye_target$`Target Name`)])

dat=input_data %>%
  right_join(maled) %>% mutate(truth=as.numeric(CT!="Undetermined"),trueCT=as.numeric(CT)) 

dat = dat %>% mutate(CT=case_when(CT=="Undetermined"~40,TRUE~as.numeric(CT)))
dat$prediction=prediction
dat$ctpred=pred2
dat=dat %>% filter(`Sample Name`!="Sample 07") # for file 1 only? 

##
malcomp=readxl::read_xlsx("MALED EQA analysis for BB.xlsx") %>% filter(`Sample ID`!="Sample 07")

colnames(malcomp)[42]="PhHV"
colnames(malcomp)[43]="PhHV_2"
colnames(malcomp)[37]="MS2"
colnames(malcomp)[38]="MS2_2"

match(dat$`Target Name`,colnames(malcomp))

datcomp=dat %>% select(well, `Well Position`,dye,file,Sample=`Sample Name`,Target=`Target Name`,CT,truth,trueCT,prediction,ctpred) %>% left_join(
malcomp %>% gather(Target,Study_ct,-`User ID`,-`Sample ID`) %>% rename(Sample=`Sample ID`), by=c("Sample","Target"))

rescomp=bind_rows(
  rbind(datcomp %>% filter(`User ID`!="UVA"),
        datcomp %>% filter(`User ID`=="UVA") %>% mutate(Study_ct=ctpred,`User ID`="Automated Analysis")) %>%  
    split(.$`User ID`) %>% purrr::map(function(x) {
  tab=table(x$truth,!is.na(x$Study_ct))
  res1=sum(diag(tab))/sum(tab)
  
  x2=x %>% filter(!is.na(x$Study_ct)) %>% mutate(trueCT=case_when(is.na(trueCT)~40,TRUE~trueCT))
  x3=x %>% filter(!is.na(x$Study_ct))
  res2=mean(abs(x2$Study_ct-x2$trueCT),na.rm=T)
  res3=mean(abs(x3$Study_ct-x3$trueCT),na.rm=T)
    resg2=mean(as.numeric(abs(x2$Study_ct-x2$trueCT)>2,na.rm=T))
data.frame(ID=x$`User ID`[1],Accuracy=round(res1,3),MAE=round(res2,3),MAE2=round(res3,3))
})) %>% arrange(Accuracy) %>%
  rename(`MAE (Misclassified Removed)`=MAE2) %>%
  mutate(`User ID`=c(paste0("Manual Analysis #",1:14),"Automated Analysis",paste0("Manual Analysis #",15:17))) 
complot=
  rbind(datcomp,#mutate(`User ID`=paste0("Manual Analysis #",as.numeric(factor(datcomp$`User ID`)))),
datcomp %>% filter(`User ID`=="UVA") %>% mutate(Study_ct=ctpred,`User ID`="Automated Analysis")) %>% 
  filter(`User ID`!="UVA",!(truth==0 & is.na(Study_ct))) %>%
  #filter(`User ID`!=18,!(truth==0 & is.na(Study_ct))) %>%

  mutate(Amplification = case_when(!is.na(Study_ct) & truth==1 ~ "Predicted: Yes, Truth: Yes",
                                        is.na(Study_ct) & truth==1 ~ "Predicted: No, Truth: Yes",
                                        !is.na(Study_ct) & truth==0 ~ "Predicted: Yes, Truth: No")) %>% 
  mutate(Study_ct=case_when(is.na(Study_ct)~40,TRUE~Study_ct),trueCT=case_when(is.na(trueCT)~40,TRUE~trueCT))# %>%
  #mutate(`User ID` = factor(`User ID`,levels=rescomp$ID)) %>%
  complot=complot %>% mutate(`User ID`=rescomp$`User ID`[match(`User ID`,rescomp$ID)]) %>% 
  mutate(`User ID`=factor(`User ID`,levels=c("Automated Analysis",paste0("Manual Analysis #",1:17)))) %>%
  ggplot(aes(x=trueCT,y=Study_ct,color=Amplification)) + geom_point(alpha=.5) + geom_abline() +  facet_wrap(~`User ID`,nrow=3) +
  scale_x_continuous(breaks=seq(0,40,by=5),labels=seq(0,40,by=5),limits=c(0,40)) + scale_y_continuous(breaks=seq(0,40,by=5),labels=seq(0,40,by=5),limits=c(0,40)) +
    ylab("Comparator Ct") + xlab("Manual (Gold Standard) Ct") + scale_color_viridis_d(end=.90)


dat$TN=dat$`Target Name`

datpred=dat %>% filter(prediction==1) %>% mutate(trueCT=case_when(is.na(trueCT)~40,TRUE~trueCT))
#tbl=table(dat$truth,dat$prediction,useNA="ifany");Accuracy=round(sum(diag(tbl))/sum(tbl),3);tbl;Accuracy
prederror=with(datpred,round(mean(abs(ctpred-trueCT),na.rm=T),3))
#with(dat %>% filter(`Target Name`!="Bacterial 16S"),round(mean(abs(ctpred-trueCT),na.rm=T),3))
prederror_g2=with(datpred,mean(as.numeric(abs(ctpred-trueCT)>=2),na.rm=T),3)



truth=as.factor(dat$truth)
pred=as.factor(dat$prediction)
tab=table(truth,pred)
colnames(tab) = c("No amp.","Amp.")
row.names(tab) = c("No amp.","Amp.")
rownames(tab) <- paste("Truth:", rownames(tab))
colnames(tab) <- paste("Predicted:", colnames(tab))