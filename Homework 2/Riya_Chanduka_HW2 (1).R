#Reading the training dataset in df
df=read.csv(file.choose())
View(df)
#Since state and county are categorical I'm telling R to treat them as categorical too
df$state=as.factor(df$state)
df$county=as.factor(df$county)
#Creating the linear model using only state
linearmodel=lm(price2013~state,df)
summary(linearmodel)
plot(linearmodel)

#Reading the test dataset in test_data
test_data=read.csv(file.choose())
View(test_data)
#Predicting the house values for 2013 using linearmodel
pred=predict.lm(linearmodel,test_data)
#Binding the predicted column to test_pred
test_predict<-cbind(test_data,round(pred))
#Displaying the final table containing the predicted values
head(test_predict)

#Creating the linear model using state and county
lm2=lm(price2013~state+county,df)
summary(lm2)
#To find the highest and least regression coefficient of US counties
a=summary(lm2)$coefficients
# Received an error when doing the prediction as some rows in the test dataset are not present in the training dataset
#Error in model: factor county has new levels bossier, gilpin, grafton, harrisonburg city, hill, hunt 
#Finding the rows which are not present in training dataset
sum(!test_data$county %in% df$county)
#Removing the rows from the test dataset which are not present in training
test_data$county[!test_data$county %in% df$county]<-NA
#Predicting the house values for 2013 using linearmodel lm2
pred2=predict.lm(lm2,test_data)
#Binding the predicted column to test_predict2
test_predict2<-cbind(test_data,round(pred2))
#Displaying the final table containing the predicted values
head(test_predict2)
View(test_predict2)

#Creating the linear model using all the columns which are improving the accuracy of prediction
lm3=lm(price2013~state+price2007+zip+poverty,df)
summary(lm3)
#Predicting the house values for 2013 using linearmodel lm3
pred3=predict.lm(lm3,test_data)
#Binding the predicted column to test_predict3
test_predict3<-cbind(test_data,round(pred3))
View(test_predict3)

#Creating a new dataframe containing only id and predicted values as columns
df_pred <- test_predict3[,c(1,7)]
View(df_pred)
library(plyr)
#Renaming the prediction column using rename function of library plyr
final_df=rename(df_pred, c("round(pred3)"="prediction"))
#Writing the final output in a csv file
write.csv(final_df, "Riya_Chanduka_Prediction_2013.csv")
