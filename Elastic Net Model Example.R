library(glmnet)
##Get the data----------------------------------------------------------------------------------------------------------------
child_data <- read.delim("C:/Users/Darlene.Dai/Documents/GitHub/BCCHR/Metadata_dateofcollection_25June2018.txt")
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
                    "cmvstatus")
child_data.microbial = c("Observed","Chao1","Shannon","Simpson", "FaithPD", "SR")

##Fit the raw model by using all the variables------------------------
#Prepare the data
data.deal <- child_data[,c(resp.var, child_data.environCont, child_data.baby, child_data.microbial)]
data.input <- na.omit(data.deal)
dim(data.input)

#Fit the model
x_train <- model.matrix( ~ .-1, data.input[,c(child_data.environCont, child_data.baby, child_data.microbial)])
model <- glmnet(x=x_train, y=data.input$A3YBin_1, family="binomial", alpha=0.5) #Hind, here the alpha can be changed to any number between 0 to 1

#Estimate the best lambda using cv.glmnet
lm = cv.glmnet(x=x_train,y = as.factor(data.input$A3YBin_1), intercept=FALSE ,family =   "binomial", alpha=0.5, nfolds=10)
best_lambda <- lm$lambda.min 

#Get the selected features from the model
res <- predict(model, s=best_lambda, type = "coefficients")
select.features <- rownames(res)[res[,1]!=0][-1]

##Re-fit the model by only using the selected variables---------------
data.deal <- child_data[,c(resp.var,select.features)]
data.input <- na.omit(data.deal)
dim(data.input)

#Fit the model
x_train <- model.matrix( ~ .-1, data.input[,c(select.features)])
model <- glmnet(x=x_train, y=data.input$A3YBin_1, family="binomial", alpha=0.5)

#Estimate the best lambda
lm = cv.glmnet(x=x_train,y = as.factor(data.input$A3YBin_1), intercept=FALSE ,family =   "binomial", alpha=0.5, nfolds=10)
best_lambda <- lm$lambda.min

#Get the coefficients of these features
res <- predict(model, s=best_lambda, type = "coefficients")

#Get the In-train AUC (Hind, I think for your script, you must have the Leave-one-out CV AUC)
probs <- predict(model, newx=x_train[,select.features], type="response",  s=best_lambda)
intrain.prob <- data.frame(SampleID=rownames(data.input),
                           Group=data.input$A3YBin_1,
                           Prob=probs[rownames(data.input),1])
with(intrain.prob, auc(y=Group,prob=Prob))
#[1] 0.7899393

