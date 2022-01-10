getwd()
setwd("/Users/college/Desktop/Semester_2/Data_mining/Project")





install.packages("randomForest")
devtools::install_github('araastat/reprtree')
install.packages("rpart")
install.packages("rpart.plot")
install.packages("psych")
install.packages("AppliedPredictiveModeling")

df <- read.csv("mushrooms.csv", header = TRUE)
print(sprintf("Number of data rows: %d",nrow(df)))



table(df$class)



str(df)

df_1 <- df

missing_values<- as.data.frame(table(is.na(df_1)))

library(dplyr)

df_1 <- df_1 %>%
        mutate_if(is.character,as.factor)

set.seed(9850)

partition = sample(nrow(df_1), nrow(df_1)*.7)

######Frequency Plot
# 
# library(caret)
# var_cols = df_1[,2:23]
# var_class = df_1[,1]
# var_cols <- sapply( var_cols, function (x) as.numeric(as.factor(x)))
# 
# scales <- list(x=list(relation="free"),y=list(relation="free"), cex=0.6)
# 
# ##########################################################################
# # cex is number indicating the amount by which plotting text and symbols # 
# # should be scaled relative to the default. 1=default, 1.5 is 50% larger,# 
# # 0.5 is 50% smaller, etc.                                               #
# ##########################################################################
# 
# 
# featurePlot(x=var_cols, y=var_class, plot="density",scales=scales,
#             layout = c(5,5), auto.key = list(columns = 2), pch = "|")
# 
# ##########

library(psych)
#description of all variables 
discrip_var <- describe(df_1)


#checking frequency of variables

freq_list <- mapply(table, df_1)

#splitting data in train and test sets

#partition = sort(sample(nrow(df_1), nrow(df_1)*.7))
df_train<-df_1[partition,]
df_test<-df_1[-partition,]



#df_train <- df_1[1:5686, ] 
#df_test <- df_1[5687:8124, ]

# RANDOM FOREST METHOD

#checking what variable are most important for predicting if a mushroom is edible

library(randomForest)




#checking class BIAS
table(df_1$class)


model_rF_1 = randomForest(class ~., ntree = 20, data = df_train)


#plotting random forest tree
library(reprtree)
reprtree:::plot.getTree(model_rF_1)

varImpPlot(model_rF_1, main ="Variable Importance", scale = TRUE)

varImpPlot(model_rF_1, main ="Variable Importance", n.var=min(5, nrow(model_rF_1$importance)), bg=3)


variable_importance_train <- as.data.frame(model_rF_1$importance)


#confusion matrix


t_pred1 = predict(model_rF_1,df_test,type="class")

confMat1 <- table(df_test$class,t_pred1)

#visualize confusion matrix
fourfoldplot(confMat1, color = c("#CC6666", "#99CC99"),
             +              conf.level = 0, margin = 1, main = "Confusion Matrix")

accuracy <- sum(diag(confMat1))/sum(confMat1)

accuracy


#decision tree for checking how to determine if a mushroom is edible

library(rpart)
library(rpart.plot)

model_dT_train <- rpart(class ~ ., data=df_train, method="class")


model_dT_train

rpart.plot(model_dT_train)



rpart.plot(model_dT_train, type=3, extra=101, fallen.leaves=T)


### confusion matrix for Decision Tree
t_pred2 = predict(model_dT_train,df_test,type="class")

confMat2 <- table(df_test$class,t_pred2)

#visualize confusion matrix
fourfoldplot(confMat2, color = c("#CC6666", "#99CC99"),
              conf.level = 0, margin = 1, main = "Confusion Matrix")



accuracy2 <- sum(diag(confMat2))/sum(confMat2)

accuracy2
