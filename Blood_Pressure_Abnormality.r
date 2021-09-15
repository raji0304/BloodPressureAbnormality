# Determining factors affecting Blood Pressure Abnormalities


#Step:1 - Loading Data

library(rio)
link='Blood Presssure Abnormality.csv'
df<-import(link)


#Step:2 - Visualizing data to identify the misrepresentation/missing values and clean them

library(mice)
library(skimr)
library(summarytools)

#Step:2.1- Structure of the dataset
str(df)

#Step:2.2 -Use skimr to categorize variables and get info on dataset
skim(df)

#Some  variables are Categorical in nature but it is shown as integer variable as follows:
#Blood_Pressure_Abnormality, Sex,Pregnancy,Smoking,Level_of_Stress, Chronic_kidney_disease, Adrenal_and_thyroid_disorders

#Step:2.3 -Summarize your data with summary tools
summarytools::view(dfSummary(df))

#Step:2.4-Examine missing values
md.pattern(df,
           rotate.names = T)

#missing values present in 
#Genetic_Pedigree_Coefficient, Pregnancy, alcohol_consuption_per_day


#Step:3 - Cleaning the dataset

#After finding out the missing values and misrepresentation of datatype in the dataset, 
#follow steps are about fixing/cleaning 



#Step:3.1- Converting Interger Variable to Categorical Variable

# Converting Blood_Pressure_Abnormality to categorical variable
df$Blood_Pressure_Abnormality = as.factor(df$Blood_Pressure_Abnormality)

# Converting Sex to categorical variable
df$Sex = as.factor(df$Sex)

# Converting Pregnancy to categorical variable
df$Pregnancy = as.factor(df$Pregnancy)

# Converting Smoking to categorical variable
df$Smoking = as.factor(df$Smoking)

# Converting Level_of_Stress to categorical variable
df$Level_of_Stress = as.factor(df$Level_of_Stress)

# Converting Chronic_kidney_disease to categorical variable
df$Chronic_kidney_disease = as.factor(df$Chronic_kidney_disease)

# Converting Adrenal_and_thyroid_disorders to categorical variable
df$Adrenal_and_thyroid_disorders = as.factor(df$Adrenal_and_thyroid_disorders)


# Checking the Structure of the dataset, after conversion to verify
str(df)


#Step:3.2- Checking for level and recoding
unique(df$Blood_Pressure_Abnormality)
levels(df$Blood_Pressure_Abnormality) <- c('No_BP','BP')


unique(df$Sex)
levels(df$Sex ) <- c('M','F')


unique(df$ Pregnancy)
levels(df$ Pregnancy) <- c('No_Prg','Prg')


unique(df$ Smoking)
levels(df$ Smoking )<- c('No_smoking','smoking')


unique(df$ Level_of_Stress)
levels(df$ Level_of_Stress )<- c('low','high','med')

unique(df$ Chronic_kidney_disease)
levels(df$ Chronic_kidney_disease )<- c('No_chronic','Chronic')

unique(df$ Adrenal_and_thyroid_disorders)
levels(df$ Adrenal_and_thyroid_disorders )<- c('No_thyroid','Thyroid')

# Checking the Structure of the dataset, after conversion to verify
str(df)


#Step:3.3- Dealing with NA values -Replacing NA Values with "Median"

# Replacing all NA values in "Genetic_Pedigree_Coefficient" with "Median"
df$Genetic_Pedigree_Coefficient=ifelse(is.na(df$Genetic_Pedigree_Coefficient),median(df$Genetic_Pedigree_Coefficient,na.rm=T), df$Genetic_Pedigree_Coefficient)


# Replacing all the NA values in "alcohol_consumption_per_day" with "Median"
df$alcohol_consumption_per_day=ifelse(is.na(df$alcohol_consumption_per_day),median(df$alcohol_consumption_per_day, na.rm=T), df$alcohol_consumption_per_day)

#Removing "Pregnancy" Variable from the dataset 
# We have (Male & Female) in column "Sex", so we cannot replace NA with values so removing

df = df[,-8]

#Removing "Patient Number" Variable from the dataset as it has no value 
df = df[,-1]

# Checking the Structure of the dataset, after removing Pregnancy,and Patient Number column
str(df)


#Step: 4- Visualizing data to look at correlation

library(ggplot2)
library(lattice)
library(caret) 



#Step: 4.1- As we have Categorical Variables, creating dummies
#install.packages("dummies")
library(dummies)
df1 <- dummy.data.frame(df, sep = ".")
df1



#Step: 4.2- Correlation analysis
#install.packages("GGally")
library(GGally)
ggpairs(df1)



#Step:4.3- Using Corrgram
library(corrgram)
corrgram(df1,
         upper.panel = panel.cor)



#Step: 5 - Using Caret for analysis

#Step: 5.1-  Fixing the initial randomization
set.seed(100)


#Step: 5.2- Split the data into training and testing
#install.packages("caret")
library(caret)


train_positions <- createDataPartition(y = df$Blood_Pressure_Abnormality, #Target
                                       p = .8,        #Training %
                                       list = F)     #Avoid a list output

training <- df[train_positions,]
testing <- df[-train_positions,]



#Step: 5.3- Cross-validation and model tuning options
library(DMwR)
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random',
                            sampling = 'smote') #to account for unbalanced



#Step: 5.4- Fit an algorithm to your data
train(Blood_Pressure_Abnormality ~ .,
      data = training,
      method = 'ctree',
      preProcess = c(),
      tuneLength =10,
      trControl = fit_control)->model_fit



#Step: 5.5- Final model
model_fit$finalModel
plot(model_fit$finalModel)



#Step: 5.6- Model performance

#Predictions for testing values
testing_pred <- predict(model_fit, testing) 

#gives value of c in (c=ax+by) 
testing_pred <- as.data.frame(testing_pred)



#Step: 5.7-Checking performance
postResample(testing_pred, testing$Blood_Pressure_Abnormality)


#Step: 5.8-Variable Importance for training
varImp(model_fit, scale = T)
plot(varImp(model_fit, scale = T))



#Step: 6- Based on Classification, taking only the Five Variables which are of importance
#	1)Chronic_kidney_disease	
# 2)Adrenal_and_thyroid_disorders			
# 3)Age
# 4)Sex
# 5)Level_of_Hemoglobin	



#Creating a New dataset only with the choosen five Variables
data1<-df[,-3]
data1<-data1[,-6:-10]
data1<-data1[,-4]

data1


#Exporting the new dataset into CSV 
export(data1,"data.csv")



#Step: 7- Visualizing the new dataset (data1) to look at correlation

library(ggplot2)
library(lattice)
library(caret) 



#Step: 7.1- As we have Categorical Variables, creating dummies

#install.packages("dummies")
library(dummies)
dummy_data1 <- dummy.data.frame(data1, sep = ".")
dummy_data1
export(dummy_data1,"dummy_data1.csv")




#Step: 7.2- Correlation analysis
#install.packages("GGally")
library(GGally)
ggpairs(dummy_data1)


#Step:7.3- Using Corrgram
library(corrgram)
corrgram(dummy_data1,
         upper.panel = panel.cor)


#Step: 8 - Using Caret for analysis of new Dataset (data1)


#Step: 8.1-  Fixing the initial randomization
set.seed(100)


#Step: 8.2- Split the data into training and testing
#install.packages("caret")
library(caret)
train_positions <- createDataPartition(y = data1$Blood_Pressure_Abnormality, #Target
                                       p = .8,        #Training %
                                       list = F)     #Avoid a list output

training <- data1[train_positions,]
testing <- data1[-train_positions,]



#Step: 8.3- Cross-validation and model tuning options
library(DMwR)
fit_control <- trainControl(method = 'repeatedcv',
                            number = 10,
                            repeats = 2,
                            search = 'random',
                            sampling = 'smote') #to account for unbalanced



#Step: 8.4- Fit an algorithm to your data
train(Blood_Pressure_Abnormality ~ .,
      data = training,
      method = 'ctree',
      preProcess = c(),
      tuneLength =10,
      trControl = fit_control)->model_fit


#Step: 8.5- Final model
model_fit$finalModel
plot(model_fit$finalModel)



#Step: 8.6- Model performance

#Predictions for testing values
testing_pred <- predict(model_fit, testing) 

#gives value of c in (c=ax+by) 
testing_pred <- as.data.frame(testing_pred)


#Step: 8.7-Checking performance
postResample(testing_pred, testing$Blood_Pressure_Abnormality)

#Step: 8.8-Variable Importance for training
varImp(model_fit, scale = T)
plot(varImp(model_fit, scale = T))


#Step: 9- After you have settled on a model,
#train the model on the whole data (NEW DATASET Data1)

#Step: 9.1- Assuming linear is the best one
train_positions <- createDataPartition(y = data1$Blood_Pressure_Abnormality, #Target
                                       p = 1.0,        #Training %
                                       list = F)     #Avoid a list output

training <- data1[train_positions,]
testing <- data1[-train_positions,]

train(Blood_Pressure_Abnormality ~ .,
      data       = training,
      method     = 'glm',
      preProcess = c(), #'center','scale' | 'range', 'corr'
      tuneLength =  10,
      family = 'binomial',
      trControl  = fit_control) -> final_model


#Step:9.2 -Final result
summary(final_model)


#Step:9.3- Variable Importance
varImp(final_model, scale = T)
plot(varImp(final_model, scale = T))



#Step:9.4- Final Metric

#Predictions for whole data 
final_pred <- predict(final_model, data1) 
final_pred <- as.data.frame(final_pred)


#Step:9.5-Checking performance
postResample(final_pred, data1$Blood_Pressure_Abnormality)



#Step:9.6-Confusion Matrix
confusionMatrix(final_pred$final_pred, data1$Blood_Pressure_Abnormality)








