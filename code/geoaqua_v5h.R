rm(list=ls())

getwd()
#setwd("C:/DevOps/geo_aqua")
setwd("C:/Users/Faiz/Desktop/2026/GeoAqua")

library(dplyr)
library(tidyr)
library(ggplot2)
library(sqldf)
library(psych)
library(corrplot)
library(writexl)
library(factoextra)
library(caTools)
library(caret)
library(Metrics)

set.seed(187)

#LOAD AND EDA

df <- read.csv('Train.csv')
#there are 963 observations!
colnames(df)
ncol(df)
#148 columns 1 ID, 1 lon, 1 lat, 1 label - the rest (144) are numerical features
head(df)
str(df)
glimpse(df)  #this is the best one
df %>% summarize(n_unique = n_distinct(ID))  #creates dataframe,
#there are 963 unique ID's

#make sure colnames are unique (i.e. not repeated)
df %>% summarize(n_unique = n_distinct(colnames(df))) #yes, 148 unique ones

#check for missing values
is.na(df)
sum(is.na(df))  #0, great!

#DATA VISUALIZATION


#STATS

#do some correlation studies
#first drop the ID, lon, and lat, and label
df1 <- df %>% select (-ID, -lon, -lat, -label)
cor <- round(cor(df1), digits=2)
#by analyzing cor, it looks like there a lot of highly correlated values
#or
corrplot(cor(df1), method = "number", type = "upper")  #some visualization here
#looks like there is a lot of correlation between somethings, and definitely 0 between others!
# i need to analyze this further and try to remove some dimensions from this dataset!
#pairs.panels(df1, gap=0, bg=c("red", "yellow", "blue"[training$label], pch=21)) #this is confusing (from psych package), come back to it
 
cor1 <- as.data.frame(cor, row.names = NULL) #force cor to a dataframe for exporting

#export as excel
write_xlsx(cor1, path = "cor1.xlsx")  #no!, both lost the first column with rows, deal with later

#try as csv
write.csv(cor1, file = "cor1.csv", row.names = TRUE)  #yes, worked! had to do row.names = TRUE

#calculate in R, the number of values that are greater than 0.9 or less than -0.9
sum(!is.na(cor1)) #20736

sum(cor1 >= 0.9)  #610   #ok, doesn't quite agree with excel though
sum(cor1 <= -0.9)  #0

sum(cor1 >= 0.7)  #3746  
sum(cor1 <= -0.7) #0

#lots of multicolinearity - i think only PCA will help
#before PCA, do StandardScaler like scikitlearn
#StandardScaler in caret package - do on original dataset
#
#
#
#
#
#PRINCIPAL COMPONENT ANALYSIS (on original data, not correlation matrix)
#AKA FEATURE ENGINEERING

pca = prcomp(df1, scale = TRUE)
head(pca$x)

# Access the loadings
pca.loadings <- pca$rotation
print(pca.loadings)
#visualization, screeplot
fviz_eig(pca)
#go with top 5 for now

#everything so far was unspervised, lets go supervised now

############################################################################
#MAKE A MODEL with Logistic REgression classifier as first pass 
#(after PCA and taking top 5)

#test train split on Train.csv (aka df in here)
#change label to factor
df2 <- df %>% select (-lon, -lat)
df2$label <- factor(df2$label)
# Split data: 70% train, 30% test
split <- sample.split(df2$label, SplitRatio = 0.7)

train_set <- subset(df2, split == TRUE)
test_set  <- subset(df2, split == FALSE)

#now model logistic classifier, with VH_01, VV_01, and blue_01 only as ind. variables
log_model <- glm(label ~ VH_01 + VV_01 + blue_01, data = train_set, family = binomial)

# Model summary
summary(log_model)

# Predict probabilities on test set
pred_probs <- predict(log_model, newdata = test_set, type = "response")

# Convert probabilities to class labels (threshold = 0.5)
pred_class <- ifelse(pred_probs > 0.5, "0", "1")
pred_class <- factor(pred_class, levels = c("0", "1"))

# Evaluate accuracy
accuracy <- mean(pred_class == test_set$label)
cat("Test Accuracy:", round(accuracy * 100, 2), "%\n")

# Confusion matrix
cat("\nConfusion Matrix:\n")
print(table(Predicted = pred_class, Actual = test_set$label))

