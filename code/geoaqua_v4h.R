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
write_xlsx(cor1, path = "cor1.xlsx")  #yes, both lost the first column with rows, deal with later


#calculate in R, the number of values that are greater than 0.9 or less than -0.9
sum(!is.na(cor1)) #20736

sum(cor1 >= 0.9)  #610   #ok, doesn't quite agree with excel though
sum(cor1 <= -0.9)  #0

sum(cor1 >= 0.7)  #3746  
sum(cor1 <= -0.7) #0

#lots of multicolinearity - i think only PCA will help


#PRINCIPAL COMPONENT ANALYSIS (on original data, not correlation matrix)
#AKA FEATURE ENGINEERING
library(factoextra)

pca = prcomp(df1, scale = TRUE)
head(pca$x)

# Access the loadings
pca.loadings <- pca$rotation
print(pca.loadings)
#visualization, screeplot
fviz_eig(pca)
#go with top 5 for now

#everything so far was unspervised, lets go supervised now

#MAKE A MODEL with Logistic REgression as first pass




######################################################################################################
#from farm to feed
# df %>% summarize(n_distinct(customer_id))  #this doesn't create a df, 141 resulting
# df %>% summarize(n_distinct(product_id))   #223 unique products
# df %>% summarize(n_unique = n_distinct(grade_name))  #7 unique grades
# df %>% summarize(n_unique = n_distinct(unit_name))  #11
# df %>% summarize(n_unique = n_distinct(customer_category))  #8
# 
# ####### restarted here 12/30/25 441 pm ########
# 
# #lets review 'glimpse' output and do some EDA.
# #first interested in qty_this_week histo
# ggplot(df, aes(x=qty_this_week)) + geom_histogram()
# ggplot(df, aes(x=num_orders_week)) + geom_histogram(colour = 4, fill = "white", bins=50)
# 
# #huh, they are all 0???
# #no!!! i had to set the xlim and ylim scales to see it better
# 
# ggplot(df, aes(x=qty_this_week)) + geom_histogram(colour = 4, fill = "white", bins=50) + xlim(0, 1000) + ylim(0,100)
# #can experiment with various limits because the units of each product id is different
# 
########################################################################################################
