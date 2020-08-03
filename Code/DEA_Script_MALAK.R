library(rDEA)
library(tibble)
library(dplyr)
library(knitr)
# file:///C:/Users/gerid/Downloads/rDEA.pdf
#https://analyticsdefined.com/data-envelopment-analysis-in-r/

setwd("/Users/malakelkhalkhali/Desktop/Test")
getwd()
d<-read.csv("DEA_inputs_outputs.csv", header=TRUE)
# Set the output working directory to store in this folder
output_dir<-("./Model_Outputs")
setwd(output_dir)


#data<-tbl_df(d)
data<-as.data.frame(d)
head(data)
# This is trying to deal with inf for cross eff dont need here data[data==0] = 0.001
input_var1<- data %>% select(Station_Flow_Rank,Test_Scores,Staff..,Station_Duration)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms,VIF_Com)
                           #VIF_forms_unique,VIF_Com1)

str(input_var1)
str(out_var1)

library(Benchmarking)

################################## USING Benchmarking #################################

##################################### MODEL 1 (include slacks)###########################

###################################  CCR Model with Slacks#########################

### Inputs: Station Duration, Flow, Staff #
### Outputs: IRF From Com, VIF From Com, #VIFs, # IRFS

input_var1<- data %>% select(Station_Flow_Rank,Staff..,Station_Duration)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms,VIF_Com)
#VIF_forms_unique,VIF_Com1)

model1<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model1$eff,4)),round(model1$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model1_CCR.csv")
# Input savings potential for each firm
(1-eff(model1)) * input_var1


################################# remove Hetauda, Jogbani and Kakarvitta##################################

#'Hetauda','Jogbani','Kakarvitta'

input_var1<- data %>% 
  filter(!(Station %in% c('Hetauda') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var1<- data %>%
  filter(!(Station %in% c('Hetauda') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms,VIF_Com) 
data2<- data %>%
  filter(!(Station %in% c('Hetauda') ))

model1<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model1$eff,4)),round(model1$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data2$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model1_CCR2.csv")

##################################### MODEL 2 (include slacks)###########################

###################################  CCR Model with Slacks#########################
#'Hetauda','Jogbani','Kakarvitta'
#'
input_var1<- data %>% 
  filter(!(Station %in% c('Hetauda','Jogbani','Kakarvitta') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var1<- data %>%
  filter(!(Station %in% c('Hetauda','Jogbani','Kakarvitta') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms) 
data2<- data %>%
  filter(!(Station %in% c('Hetauda','Jogbani','Kakarvitta') ))


model2<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model2$eff,4)),round(model2$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data2$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model2_CCR2.csv")



####################### ALL STATIONS ###############

input_var1<- data %>% select(Station_Flow_Rank,Staff..,Station_Duration)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms)
#VIF_forms_unique,VIF_Com1)

model2<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model2$eff,4)),round(model2$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model2_CCR.csv")
# Input savings potential for each firm
(1-eff(model2)) * input_var1


##################################### MODEL 3 (include slacks)###########################

###################################  CCR Model with Slacks#########################
########################### Using the INDV Values as OUTPT for VIF COMP###############################################

####################################### ALL STATIONS ################################################################
### Inputs: Flow, Staff #, Test Scores
### Outputs: IRF From Com, #VIFs, # IRFS, Individual VIF from Completeness***
input_var1<- data %>% select(Station_Flow_Rank,Staff.., Test_Scores)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms,Average_VIF_com_INDV)
#VIF_forms_unique,VIF_Com1)

model3<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model3$eff,4)),round(model3$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model3_CCR.csv")



##################################### MODEL 4 (include slacks)###########################

###################################  CCR Model with Slacks#########################
########################### Using the MIN Values as OUTPT for VIF COMP###############################################

####################################### ALL STATIONS ################################################################
### Inputs: Station Duration, Flow, Staff #
### Outputs: IRF From Com, #VIFs, # IRFS, Individual VIF from Completeness***
input_var1<- data %>% select(Station_Flow_Rank,Staff..,Station_Duration)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms,Average.Min.Value)
#VIF_forms_unique,VIF_Com1)

model4<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model4$eff,4)),round(model4$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model4_CCR.csv")


##################################### MODEL 5 (include slacks)###########################

###################################  CCR Model with Slacks#########################
########################### Using the MAX Values as OUTPT for VIF COMP###############################################

####################################### ALL STATIONS ################################################################
### Inputs: Station Duration, Flow, Staff #
### Outputs: IRF From Com, #VIFs, # IRFS, Individual VIF from Completeness***
input_var1<- data %>% select(Station_Flow_Rank,Staff..,Station_Duration)
out_var1<- data %>% select(IRF_forms,IRF_Com,VIF_forms,AVERAGE.using.Max)
#VIF_forms_unique,VIF_Com1)

model5<-dea(X=input_var1, Y=out_var1, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model5$eff,4)),round(model5$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model5_CCR.csv")



################################################## Remove layer 1 from all Models ##################################

################################################## RUn Model 1 with layer 2 ONLY #######################################


# Stations on Eff Frontier:
# 'Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi'


input_var3<- data %>% 
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms,VIF_Com) 
data3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') ))

model1<-dea(X=input_var3, Y=out_var3, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model1$eff,4)),round(model1$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data3$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model1_CCR_Layer2.csv")

################################################## RUn Model 2 with layer 2 ONLY #######################################


# Stations on Eff Frontier:
# 'Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi'


input_var3<- data %>% 
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms) 
data3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') ))

model2<-dea(X=input_var3, Y=out_var3, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model2$eff,4)),round(model2$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data3$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model2_CCR_Layer2.csv")




###################################### Run Model 3 with layers, remove all 1's on the effint Frnt#############

# Stations on Eff Frontier:
# 'Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi'


input_var3<- data %>% 
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms,Average_VIF_com_INDV) 
data3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa', 'Birgunj', 'Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Narayangadh','Nawalparasi') ))

model3<-dea(X=input_var3, Y=out_var3, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model3$eff,4)),round(model3$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data3$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model3_CCR_Layer2.csv")


###################################### Run Model 4 with layers, remove all 1's on the effint Frnt#############

# Stations on Eff Frontier:
# 'Bhadrapur', 'Bhairawa','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Lahan','Mahendranagar','Narayangadh','Nawalparasi'

input_var3<- data %>% 
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Lahan','Mahendranagar','Narayangadh','Nawalparasi') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Lahan','Mahendranagar','Narayangadh','Nawalparasi') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms,Average.Min.Value) 
data3<- data %>%
  filter(!(Station %in% c('Bhadrapur', 'Bhairawa','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Lahan','Mahendranagar','Narayangadh','Nawalparasi') ))

model4<-dea(X=input_var3, Y=out_var3, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model4$eff,4)),round(model4$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data3$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model4_CCR_Layer2.csv")


###################################### Run Model 5 with layers, remove all 1's on the effint Frnt#############

# Stations on Eff Frontier:
# 'Bhadrapur','Bhairawa','Biratnagar','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Mahendranagar','Narayangadh','Nawalparasi','Nepalgunj'

input_var3<- data %>% 
  filter(!(Station %in% c('Bhadrapur','Bhairawa','Biratnagar','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Mahendranagar','Narayangadh','Nawalparasi','Nepalgunj') )) %>% 
  select(Station_Flow_Rank,Staff..,Station_Duration) 

out_var3<- data %>%
  filter(!(Station %in% c('Bhadrapur','Bhairawa','Biratnagar','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Mahendranagar','Narayangadh','Nawalparasi','Nepalgunj') )) %>% 
  select(IRF_forms,IRF_Com,VIF_forms,AVERAGE.using.Max) 
data3<- data %>%
  filter(!(Station %in% c('Bhadrapur','Bhairawa','Biratnagar','Birgunj','Chaunuta','Gaur','Hetauda','Jogbani','Kakarvitta','Mahendranagar','Narayangadh','Nawalparasi','Nepalgunj') ))

model5<-dea(X=input_var3, Y=out_var3, ORIENTATION = "out", RTS='vrs',SLACK = TRUE)
# get the relative efficiencies and lambdas##########
results<-cbind((1/round(model5$eff,4)),round(model5$lambda,4)) ## since this is a output oriented need to take recp. 
rownames(results)<-data3$Station#data[[1]]
colnames(results)<-c("Efficiency",rownames(results))
results
write.csv(results,"model5_CCR_Layer2.csv")











