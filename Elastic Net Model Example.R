library(glmnet)
##Get the data----------------------------------------------------------------------------------------------------------------
child_data <- read.delim("P:/Darlene Dai/Data/microbiome_metadata/Metadata_dateofcollection_02Aug2018.txt")
resp.var <-  "A3YBin_1"  
child_data.columns = colnames(child_data)
rownames(child_data) <- child_data$SampleID
#note that environCat variables are just categories derived from environCont
child_data.environCont = c("walk_score","avgndvi250m" ,"no2_tm1","no2_preg_imp","popdens250m","popdens2500m", "pm25", "o3", 
                           "socialsupport","lesshs","near_water", "near_park")
child_data.environCat = c( "BinaryPark", "BinaryWater",
                           "Green250Q","TRAP_preg",
                           "TertilesSocSupport", "TertilesNO2", "TertilesWalkScore", "TertilesPopDens2500", "TertilesPM25" ,"TertilesO3")
child_data.baby = c("Sex","AgeAtVisit","MOD","OlderSibling","matopy","fatopy", "masthma", "fasthma", "race_m", "race_d",
                    "breastfeed_months","Prebirth_abs_yn", "Mother_abs_birth_yn", "Mom_3mo_abs_yn", "Mom_1Y_abs_yn",
                    "Child_birth_abs_yn","Child_3mo_abs_yn", "Child_6m_abs_total_yn", "Child_1Y_abs_yn", "Child_abs_yn", "Mom_abs_yn",
                    "CMV.status")
child_data.microbial = c("Observed","Chao1","Shannon","Simpson","PD"  ,"SR")

#Use Leave-one-out CV AUC to select the best model 
for(alpha.use in c(0.3,0.5,0.7,0.9)){
  ##Fit the raw model by using all the variables------------------------
  #Prepare the data
  data.deal <- child_data[,c(resp.var, child_data.environCont, child_data.baby, child_data.microbial)]
  data.input <- na.omit(data.deal)
  dim(data.input)
  x_all <- model.matrix( ~ .-1, data.input) # remove intercept to create matrix where cat.var become binary 
  cv.probs <- data.frame(SampleID=NA, Group=NA,probs=NA)
  for(i in 1:nrow(x_train)){
    #Get the training and test for LOOCV 
    x_train <- x_all[-i,]
    x_test <- x_all[i , ,drop=F]
    
    #Fit the model
    model <- glmnet(x=x_train[,-1], y=x_train[,1], family="binomial", alpha=alpha.use) #Hind, here the alpha can be changed to any number between 0 to 1
  
    #Estimate the best lambda using cv.glmnet
    set.seed(34209483)
    lm = cv.glmnet(x=x_train[,-1],y = x_train[,1], intercept=FALSE ,family =   "binomial", alpha=alpha.use, nfolds=10)
    best_lambda <- lm$lambda.min 
    probs <- predict(model, newx=x_test[,-1,drop=F], type="response",  s=best_lambda)
    cv.probs <- rbind(cv.probs, c(rownames(x_all)[i],x_all[i,1], probs))
  }
  cv.probs <- cv.probs[-1,]
  cv.probs$probs <- as.numeric(cv.probs$probs)
  cv.probs$Group <- as.numeric(cv.probs$Group)

  #Calculate AUC
  auc.per <- with(cv.probs, auc(y=Group,prob=probs))
  cat(auc.per)
  cat("\n")
}
# 0.8882682
# 0.8586162
# 0.8358401
# 0.8182209

#According to the above AUC,  alpha equals to 0.3 provides the best results.
#-------------------------------------------------------------------------------------------
#Fit the best model (alpha=0.3) using all the samples
data.deal <- child_data[,c(resp.var, child_data.environCont, child_data.baby, child_data.microbial)]
data.input <- na.omit(data.deal)
dim(data.input)
x_all <- model.matrix( ~ .-1, data.input)
model <- glmnet(x=x_all[,-1], y=x_all[,1], family="binomial", alpha=0.3) #Hind, here the alpha can be changed to any number between 0 to 1

#Estimate the best lambda using cv.glmnet
set.seed(34209483)
lm = cv.glmnet(x=x_all[,-1],y = x_all[,1], intercept=FALSE ,family =   "binomial", alpha=0.3, nfolds=10)
best_lambda <- lm$lambda.min 

#Get the coefficients of these features
res <- predict(model, s=best_lambda, type = "coefficients") 

select.features <- rownames(res)[res[,1]!=0][-1]
if(any(gsub("MOD","",select.features)!=select.features)){
  select.features <- c("MOD",select.features[gsub("MOD","",select.features)==select.features])
}
#--------------------------------------------------------------------------------------------------
##Re-fit the model by only using the selected variables - Amazing!!! Darlene allows the use of more samples this way!---------------
data.deal <- child_data[,c(resp.var, select.features)]
data.input <- na.omit(data.deal)
dim(data.input)
x_all <- model.matrix( ~ .-1, data.input)
model <- glmnet(x=x_all[,-1], y=x_all[,1], family="binomial", alpha=0.3) 
#Estimate the best lambda using cv.glmnet
set.seed(34209483)
lm = cv.glmnet(x=x_all[,-1],y = x_all[,1], intercept=FALSE ,family =   "binomial", alpha=0.3, nfolds=10)
best_lambda <- lm$lambda.min 

#Get the coefficients of these features
res <- predict(model, s=best_lambda, type = "coefficients") # More variables has coefficient equals to 0
res
select.features <- rownames(res)[res[,1]!=0][-1]
if(any(gsub("MOD","",select.features)!=select.features)){
  select.features <- c("MOD",select.features[gsub("MOD","",select.features)==select.features])
}
#--------------------------------------------------------------------------------------------------------------
##Re-fit the model by only using the selected variables again
data.deal <- child_data[,c(resp.var, select.features)]
data.input <- na.omit(data.deal)
dim(data.input)
x_all <- model.matrix( ~ .-1, data.input)
model <- glmnet(x=x_all[,-1], y=x_all[,1], family="binomial", alpha=0.3) 
res1 <- predict(model, s=best_lambda, type = "coefficients")
res1
#Get the standardized coefficients of these features
sds <- apply(x_all, 2, sd)
cs <- res1[,1]
std_coefs <- cs[-1]*sds[-1]

plot(model, xvar = "lambda", label = TRUE)
#--------------------------------------------------------------------------------------------------------------
#Final Model is as above
select.features
#Calculate its LOOCV AUC

#Use Leave-one-out CV AUC to select the best model 
for(alpha.use in c(0.3)){
  ##Fit the raw model by using all the variables------------------------
  #Prepare the data
  data.deal <- child_data[,c(resp.var, select.features)]
  data.input <- na.omit(data.deal)
  cat(dim(data.input))
  cat("\n")
  
  x_all <- model.matrix( ~ .-1, data.input) # remove intercept to create matrix where cat.var become binary 
  cv.probs <- data.frame(SampleID=NA, Group=NA,probs=NA)
  for(i in 1:nrow(x_train)){
    #Get the training and test for LOOCV 
    x_train <- x_all[-i,]
    x_test <- x_all[i , ,drop=F]
    
    #Fit the model
    model <- glmnet(x=x_train[,-1], y=x_train[,1], family="binomial", alpha=alpha.use) #Hind, here the alpha can be changed to any number between 0 to 1
    
    #Estimate the best lambda using cv.glmnet
    set.seed(34209483)
    lm = cv.glmnet(x=x_train[,-1],y = x_train[,1], intercept=FALSE ,family =   "binomial", alpha=alpha.use, nfolds=10)
    best_lambda <- lm$lambda.min 
    probs <- predict(model, newx=x_test[,-1,drop=F], type="response",  s=best_lambda)
    cv.probs <- rbind(cv.probs, c(rownames(x_all)[i],x_all[i,1], probs))
  }
  cv.probs <- cv.probs[-1,]
  cv.probs$probs <- as.numeric(cv.probs$probs)
  cv.probs$Group <- as.numeric(cv.probs$Group)
  
  #Calculate AUC
  auc.per <- with(cv.probs, auc(y=Group,prob=probs))
  cat(auc.per)
  cat("\n")
}
#LOOCV AUC equals to 0.92


# coefficients for each variable is given by res
# to compare the importance of each feature, 
# either compute a standardize 
# Or have a graph

