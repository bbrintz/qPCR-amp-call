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


