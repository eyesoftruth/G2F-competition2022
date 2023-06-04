
runMed = 5
days_interval = 5

#create data
train_wt_runmed=read.csv(paste0("../Processed_data/Train/Training_Weather_Data_2014_2021_runmed_",runMed,".csv"))

#keep only data from Julian Day = 60 to 300

index = train_wt_runmed$julian_day>=60 & train_wt_runmed$julian_day<=300

train_wt_runmed_filtered = train_wt_runmed[index,]

envs = unique(train_wt_runmed_filtered$Env)
e=1
subset_dt = train_wt_runmed_filtered[which(train_wt_runmed_filtered$Env==envs[e]),]
index = seq(1,nrow(subset_dt),by=days_interval)
subset_dt = subset_dt[index,]
c1  = data.frame(subset_dt$QV2M)
row.names(c1) = paste0("QV2M_",subset_dt$julian_day)
c1 = t(c1)

for(c in 6:ncol(subset_dt)){
  tmp = data.frame(subset_dt[,c])
  row.names(tmp) = paste0(colnames(subset_dt)[c],"_",subset_dt$julian_day)
  tmp = t(tmp)
  c1 = cbind(c1,tmp)
}
row.names(c1) = envs[e]
processed_train_weather_data = c1

for(e in 2:length(envs)){
  subset_dt = train_wt_runmed_filtered[which(train_wt_runmed_filtered$Env==envs[e]),]
  index = seq(1,nrow(subset_dt),by=days_interval)
  subset_dt = subset_dt[index,]
  c1  = data.frame(subset_dt$QV2M)
  row.names(c1) = paste0("QV2M_",subset_dt$julian_day)
  c1 = t(c1)
  
  for(c in 6:ncol(subset_dt)){
    tmp = data.frame(subset_dt[,c])
    row.names(tmp) = paste0(colnames(subset_dt)[c],"_",subset_dt$julian_day)
    tmp = t(tmp)
    c1 = cbind(c1,tmp)
  }
  row.names(c1) = envs[e]
  processed_train_weather_data = rbind(processed_train_weather_data,c1)
}

write.csv(processed_train_weather_data,paste0("../Processed_data/Train/processed_train_weather_data_2014_2021_runmed_",runMed,".csv"))

#Soil data
soil_data = read.csv("../Processed_data/Data From Monica/soil_imputed_MD.csv")
soil_data = data.frame(Env_year=paste0(soil_data$Env,"_",soil_data$Year),soil_data)

#genotype data
 geno_pca = readRDS("../Processed_data/PCA_from_GRM.rds")
 geno_pca_110 = data.frame(row.names(geno_pca),geno_pca[,1:110])
 
 #train trait data
 train_trait = read.csv("../Training_Data/1_Training_Trait_Data_2014_2021.csv")
 train_trait = train_trait[,c("Env", "Year","Hybrid","Hybrid_Parent1", "Hybrid_Parent2","Yield_Mg_ha")]
 train_trait = na.omit(train_trait)
 
 index = match(train_trait$Hybrid,row.names(geno_pca))
 train_trait_with_geno = train_trait[which(is.na(index)==F),]
 index = match(train_trait_with_geno$Hybrid,row.names(geno_pca))
 train_trait_with_geno = data.frame(train_trait_with_geno,geno_pca_110[index,])
 all.equal(train_trait_with_geno$Hybrid,train_trait_with_geno$row.names.geno_pca.)
 
 train_trait_with_geno = train_trait_with_geno[,-which(colnames(train_trait_with_geno)=="row.names.geno_pca.")]
 
 #Add soil data
 index = match(train_trait_with_geno$Env,soil_data$Env_year)
 train_trait_with_geno_soil = train_trait_with_geno[which(is.na(index)==F),]
 index = match(train_trait_with_geno_soil$Env,soil_data$Env_year)
 train_trait_with_geno_soil = data.frame(train_trait_with_geno_soil,soil_data[index,])
 all.equal(train_trait_with_geno_soil$Env,train_trait_with_geno_soil$Env_year) #TRUE
 train_trait_with_geno_soil = train_trait_with_geno_soil[,-which(colnames(train_trait_with_geno_soil) %in% c("Env_year", "Env.1",    "Year.1"))]
 dim(train_trait_with_geno_soil)
 
 #Add weather data
 
 index = match(train_trait_with_geno_soil$Env,row.names(processed_train_weather_data))
 train_trait_with_geno_soil_weather = train_trait_with_geno_soil[which(is.na(index)==F),]
 index = match(train_trait_with_geno_soil_weather$Env,row.names(processed_train_weather_data))
 train_trait_with_geno_soil_weather = data.frame(train_trait_with_geno_soil_weather,processed_train_weather_data[index,])
 dim(train_trait_with_geno_soil_weather)
 
 write.csv(train_trait_with_geno_soil_weather,paste0("../Processed_data/Train/processed_train_trait_with_geno_soil_weather_runmed_",runMed,".csv"))
 
 
 
 ##########prepare test data
 
 #create data
 test_wt_runmed=read.csv(paste0("../Processed_data/Test/Testing_Weather_Data_2022_runmed_",runMed,".csv"))
 
 #keep only data from Julian Day = 60 to 300
 
 index = test_wt_runmed$julian_day>=60 & test_wt_runmed$julian_day<=300
 
 test_wt_runmed_filtered = test_wt_runmed[index,]
 
 envs = unique(test_wt_runmed_filtered$Env)
 e=1
 subset_dt = test_wt_runmed_filtered[which(test_wt_runmed_filtered$Env==envs[e]),]
 index = seq(1,nrow(subset_dt),by=days_interval)
 subset_dt = subset_dt[index,]
 c1  = data.frame(subset_dt$QV2M)
 row.names(c1) = paste0("QV2M_",subset_dt$julian_day)
 c1 = t(c1)
 
 for(c in 6:ncol(subset_dt)){
   tmp = data.frame(subset_dt[,c])
   row.names(tmp) = paste0(colnames(subset_dt)[c],"_",subset_dt$julian_day)
   tmp = t(tmp)
   c1 = cbind(c1,tmp)
 }
 row.names(c1) = envs[e]
 processed_test_weather_data = c1
 
 for(e in 2:length(envs)){
   subset_dt = test_wt_runmed_filtered[which(test_wt_runmed_filtered$Env==envs[e]),]
   index = seq(1,nrow(subset_dt),by=days_interval)
   subset_dt = subset_dt[index,]
   c1  = data.frame(subset_dt$QV2M)
   row.names(c1) = paste0("QV2M_",subset_dt$julian_day)
   c1 = t(c1)
   
   for(c in 6:ncol(subset_dt)){
     tmp = data.frame(subset_dt[,c])
     row.names(tmp) = paste0(colnames(subset_dt)[c],"_",subset_dt$julian_day)
     tmp = t(tmp)
     c1 = cbind(c1,tmp)
   }
   row.names(c1) = envs[e]
   processed_test_weather_data = rbind(processed_test_weather_data,c1)
 }
 
 write.csv(processed_test_weather_data,paste0("../Processed_data/Test/processed_test_weather_data_20202_runmed_",runMed,".csv"))
 
 
 
 
 #train trait data
 test_trait = read.csv("../Testing_Data/1_Submission_Template_2022_Edited.csv")
 test_trait = data.frame(Hybrid=paste0(test_trait$Hybrid_Parent1,"/",test_trait$Hybrid_Parent2),test_trait)
 
 index = match(test_trait$Hybrid,row.names(geno_pca))
 test_trait_with_geno = data.frame(test_trait,geno_pca_110[index,])
 all.equal(test_trait_with_geno$Hybrid,test_trait_with_geno$row.names.geno_pca.)
 
 test_trait_with_geno = test_trait_with_geno[,-which(colnames(test_trait_with_geno)=="row.names.geno_pca.")]
 
 
 
 #Add soil data
 soil_data = read.csv("../Processed_data/Data From Monica/imputed_test_SOIL_090123.csv")
 index = match(test_trait_with_geno$Env,soil_data$Env)
 test_trait_with_geno_soil = data.frame(test_trait_with_geno,soil_data[index,])
 all.equal(test_trait_with_geno_soil$Env,test_trait_with_geno_soil$Env.1) #TRUE
 test_trait_with_geno_soil = test_trait_with_geno_soil[,-which(colnames(test_trait_with_geno_soil) %in% c("Env.1",   "Env_Loc", "Year"))]
 
 
 #Add weather data
 
 index = match(test_trait_with_geno_soil$Env,row.names(processed_test_weather_data))
 #train_trait_with_geno_soil_weather = train_trait_with_geno_soil[which(is.na(index)==F),]
 test_trait_with_geno_soil_weather = data.frame(test_trait_with_geno_soil,processed_train_weather_data[index,])
 dim(test_trait_with_geno_soil_weather)
 
 write.csv(test_trait_with_geno_soil_weather,paste0("../Processed_data/Test/processed_test_trait_with_geno_soil_weather_runmed_",runMed,".csv"))
 
 
 ####################################################################################################
 
 
 
 
 
 
 train_trait_with_geno_soil_weather=read.csv(paste0("../Processed_data/Train/processed_train_trait_with_geno_soil_weather_runmed_",runMed,".csv"),stringsAsFactors = F)
 #remove texture from the train data as this soil variable is not present in the test soil data for every Env
 train_trait_with_geno_soil_weather=train_trait_with_geno_soil_weather[,-which(colnames(train_trait_with_geno_soil_weather) %in% c("X","Texture","Texture.No"))]
 
 only_data = train_trait_with_geno_soil_weather[,-(1:116)]
 
 library(e1071)
 library(caret)
 library(corrplot)
 library(AppliedPredictiveModeling)
 x=nearZeroVar(only_data) #"Texture.No"
 correlations <- cor(only_data)
 corrplot(correlations, order = "hclust")
 
 highCorr <- findCorrelation(correlations, cutoff = .9)
 filtered_data = only_data[,-highCorr]
 filtered_data = data.frame(train_trait_with_geno_soil_weather[,1:116],filtered_data)
 dim(filtered_data)

 valid_data = train_trait_with_geno_soil_weather[which(train_trait_with_geno_soil_weather$Year==2021),]
 valid_data = valid_data[,-c(1:3)]

 train_data = train_trait_with_geno_soil_weather[which(train_trait_with_geno_soil_weather$Year!=2021),]
 train_data = train_data[,-c(1:3)]
 # 
 # valid_data = filtered_data[which(filtered_data$Year==2021),]
 # valid_data = valid_data[,-c(1:3)]
 # 
 # train_data = filtered_data[which(filtered_data$Year!=2021),]
 # train_data = train_data[,-c(1:3)]
 # 
 library(rpart)
 m5_model = rpart(Yield_Mg_ha ~ ., data = train_data)
 m5_predictions = predict(m5_model,valid_data)
 library(ranger)
 library(Metrics)
 rf.model = ranger(data=train_data,dependent.variable.name = "Yield_Mg_ha",num.trees = 500) 
 predictions_ = predict(rf.model,valid_data)$predictions
 #cor(predictions_,valid_data$Yield_Mg_ha)
  rmse(valid_data$Yield_Mg_ha,predictions_)
 
 
  #RF model with all the avaiable train data
  rf.model_all_train_set = ranger(data=train_trait_with_geno_soil_weather[,-(1:3)],dependent.variable.name = "Yield_Mg_ha",num.trees = 600) 
  rf.model_all_train_set
  
  test_data = read.csv("../Processed_data/Test/processed_test_trait_with_geno_soil_weather_runmed_5.csv")
 dim(test_data)
 test_data = test_data[,-(1:4)]
 
 final_predictions_400 = predict(rf.model_all_train_set,test_data)$predictions
 final_predictions_600 = predict(rf.model_all_train_set,test_data)$predictions
 final_predictions_600 = data.frame(test_data[,2:3],Yield_Mg_ha=final_predictions_600)
 
 write.csv(final_predictions_600,"../Processed_data/Test/final_predictions.csv")
 
 
 ##PCA from genotypes
 
 geno_pca = read.csv("../Processed_data/genotype_pca.csv")
 first_250pca = geno_pca[,1:251]

 
 index = match(train_trait_with_geno_soil_weather$Hybrid,first_250pca[,1]) 
 
 tmp = data.frame(train_trait_with_geno_soil_weather,first_250pca[index,-1])
 tmp = tmp[,-c(7:116)]
 valid_data = tmp[which(train_trait_with_geno_soil_weather$Year==2021),]
 valid_data = valid_data[,-c(1:3)]
 
 train_data = tmp[which(train_trait_with_geno_soil_weather$Year!=2021),]
 train_data = train_data[,-c(1:3)]
 
 
 
 rf.model = ranger(data=train_data,dependent.variable.name = "Yield_Mg_ha",num.trees = 500) 
 predictions_ = predict(rf.model,valid_data)$predictions
 #cor(predictions_,valid_data$Yield_Mg_ha)
 rmse(valid_data$Yield_Mg_ha,predictions_)
 
 
 ##Using pruned genotype
 geno = readRDS("../Processed_data/cleaned_pruned_geno.rds")
 geno = t(geno)
 

 index = match(train_trait_with_geno_soil_weather$Hybrid,row.names(geno))   
which(is.na(index)==T) 

tmp = readRDS("../Processed_data/Train/processed_train_trait_with_actual_geno_soil_weather_runmed_5.RDS")
valid_data = tmp[which(tmp$Year==2021),]
valid_data = valid_data[,-c(1:3)]

train_data = tmp[which(tmp$Year!=2021),]
train_data = train_data[,-c(1:3)]

rf.model = ranger(data=train_data,dependent.variable.name = "Yield_Mg_ha",num.trees = 200) 
predictions_ = predict(rf.model,valid_data)$predictions
#cor(predictions_,valid_data$Yield_Mg_ha)
rmse(valid_data$Yield_Mg_ha,predictions_)
