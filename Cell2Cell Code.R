# Clear memory
rm(list = ls()) # remove everything in your workspace (handy trick)

# Packages
install.packages("tidyverse")
library(tidyverse)

# Bring in data
getwd() 
cell <- read.csv("/Users/anu/Desktop/assignment-5/cell2cell.csv")
names(cell)

#Q1)

##i)
#a.
count(cell)
#equals 69626

#b.
#count of the rows in each set
sum(cell$calibrat==1)
sum(cell$calibrat==0)
#count in calibration set is 39186 and in validation set is 30440

#c.
#checking churn rate for calibration and validation set
cali_set = cell[cell$calibrat==1,] 
churn_rate_cali = sum(cali_set$churn) / nrow(cali_set); round(churn_rate_cali,4)
#equals 0.5
#d
vali_set = cell[cell$calibrat==0,]
churn_rate_vali = sum(vali_set$churn) / nrow(vali_set); round(churn_rate_vali,4)
#equals 0.0195
names(cell)


##ii)
#a.
#Dropping 3 columns in the data set to work with relevant columns
relevant_cols = cell[, c(3, 5:70, ncol(cell))] 
names(relevant_cols)

#b.
#splitiing the data into train and validation
training = relevant_cols[relevant_cols['calibrat']==1,]
validation = relevant_cols[relevant_cols['calibrat']==0,]
names(training)

#dropping the calibrat column
new_train = drop(training[,-1])
new_vali = drop(validation[,-1]) 
names(new_train)


library(dplyr)

##iii)
#running logistic regression
model_data <- select(new_train, churndep, revenue:retcall) #using select to work with logreg
logreg <- glm(churndep ~ ., data = model_data, family = binomial)
summary(logreg)

# Converting to Odds Ratio
exp(coef(logreg))
# Odds ratios and 95% Confidence Interval
OR = exp(cbind(OR = coef(logreg), confint.default(logreg)))

#The largest odds ratio is for retcall (OR=2.29)
#This indicates that as retcall increases, the odds of churn increase by 2.29 times
#The p-value is < 0.001 indicating this is a statistically significant positive relationship
#The next largest is for refurb (OR=1.26) 
#As the number of refurb increase, the odds of churn increase by 1.26 times
#The p-value is < 0.001, so it is a significant positive relationship

#The smallest odds ratio is for creditaa (OR=0.698) 
#As creditaa increases, the odds of churn decrease by a factor of 0.698 
#The p-value is < 0.001 showing a significant negative relationship

#The next smallest is for activesubs (OR=0.812)
#As activesubs increases, the odds of churn decrease by a factor of 0.812
#The p-value is < 0.001 showing a significant negative relationship

##iv)
#using predicted probabilities of attrition on validation set
new_vali$attrition <- logreg %>% predict(new_vali, type = "response")
head(new_vali$attrition,15)

#Sort the predictions in descending order
sorted_predictions <- new_vali$attrition[order(-new_vali$attrition)]
#Display the 5 highest attrition probabilities
head(sorted_predictions, 5)

#Q2)
#1
#a)
# Convert parameter coefficients to Odds Ratios
OR <- exp(coef(logreg))

#b
# Extract p-values
pvalues <- coef(summary(logreg))[,'Pr(>|z|)']

#c
# Combine into a DataFrame
df1 <- data.frame(OR, pvalues)
df1$VarName <- row.names(df1)

#d
#viewing the dataframe
head(df1)

#b)
# Assuming 'training' is your calibration dataset
sd_values <- sapply(training, function(x) sd(x, na.rm = TRUE))
df2 <- data.frame(StdDev = sd_values)
df2$VarName <- row.names(df2)

#c)
# Merge based on variable names
merged_df <- merge(df1, df2, by = "VarName", all = FALSE)

#d)
# Filter rows with p-values < 0.05
filtered_df <- merged_df[merged_df$pvalues < 0.05, ]

# Round only the numeric columns to 5 decimal points
filtered_df[,-1] <- round(filtered_df[,-1], 5)  # Assuming the first column is VarName

final_df <- filtered_df

#e)
# Write to CSV
write.csv(final_df, "Economic_Importance.csv", row.names = FALSE)
