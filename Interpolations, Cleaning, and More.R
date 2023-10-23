#Welcome to R
#June 2nd, 2023 
#Script
#Rishabh Singh
#
#
#
#Setting up environment
rm(list = ls()) #Cleans the environment; 
#
#The following two lines are if you are using the SOFTWARE version of R Studio, not the cloud version. 
#setwd("~/Desktop/Exchanges") #Sets the working directory within the script 
#
#Importing the needed packages
library("zoo")
library("dplyr") 
library("tidyverse") #Imports the "tidyverse" package. 
library("ggplot2") #Imports the "ggplot2" package. 
#
exchanges <- read.csv("exchange_dataset.csv") #Imports data of the foreign exchange rates of 
#38 different Asian countries in relation to the US Dollar over time. 
#
#Data from: https://www.kaggle.com/datasets/adilbhatti/dollar-exchange-rates-asian-countries?resource=download 
#
#exchanges <- read.csv("/Users/rishisingh/Desktop/Exchanges/exchange_dataset.csv", sep = ";", na.rm = TRUE) #How to import the dataset
#from the DESKTOP ITSELF; this is how you would do it if you are using the software version of R Studio. 
#
#You can rwad the dataset using the software version of R this way too. 
#exchanges <- read.csv("~/Desktop/Exchanges/exchange_dataset.csv", sep = ";", na.rm = TRUE)
#
#
#Exploratory Data Analysis: A way to understand the data; get the sense of it. It is important to note
#that the website/file from which you get the data from usually also tells a little about the data and its attributes/columns. 
#
#The "View" command. 
View(exchanges) #View the entire dataset; we can SEE that there are missing values in the dataset. 
#
#Welcome to the basic R commands 
class(exchanges) #What is the classification of the dataset? Answer: "Data frame"
#A data frame is a tabular-LIKE format for our data; data can come in many formats. This is just one. 
#
dim(exchanges) #How many rows and columns are there? Note: Rows come FIRST and columns come SECOND. To explain,
#there are 4956 rows in this dataset and 39 columns (one column being date and the other being the 38 exchange rates)
#
names(exchanges) #What are the names of the columns? 
#
head(exchanges) #First 5 rows 
tail(exchanges) #Last 5 rows 
#
summary(exchanges) #Summary of data. Note that the "summary" is PARTICULARLY helpful for 
#numerical columns. Character columns not so much. To investigate a nature of a column which does NOT
#contain numerical values, you want to try using the other commands to understand what values that column stores.
#
#
#Data Cleaning: Changing names as well as Linear and Cubic Interpolations
#
#Chaning any names
#While in this case the names of our columns ARE understandable, that is NOT always the case. Here is how you would change 
#the name of ONE column or multiple columns 
#
#Creates a new table; our current dataset "exchanges" doesn't NEED to have its column names changed.
exchanges_diff_col_names <- exchanges 
names (exchanges_diff_col_names) #Checks to see that the dataset correctly copied over by looking at the column names. 
#Note: You can use the "tab" button on your keyboard to refer to a variable/dataset quicker than typing it out. 
#
#
#Changing one specific column
colnames(exchanges_diff_col_names)[2] <- "$/Yuan" #This, for example, changes the column name of the 
#Chineese Yuan's exchange rates to its mathematical calculation rather. 
colnames(exchanges_diff_col_names)[2]#It is a good habit to always check to see that your code ran in the way you 
#intended for it too. 
#
#Changing the names of multiple columns 
#
smaller_exchanges <- select(exchanges_diff_col_names, c(1, 2, 3, 4)) #Selects the first 4 columns in
#"exchanges_diff_col_names" and assigns THAT data frame "smaller_exchanges". THAT smaller data frame is now
#referred to with the variable "smaller_exchanges". The "select" function comes from the "dplyr" package. 
#
head(smaller_exchanges) #Checks to see that "smaller_exchanges" has the right columns. First 5 rows
#of "smaller_exchanges". 
#
names(smaller_exchanges) <- c("Date", "$/Yuan", "$/Irani Rial", "$/Thai Baht") #The mathematical calculation that was performed
#behind the exchange rate was provided from Kaggle (the site where we got the dataset from). 
#
names(smaller_exchanges) #Checks to see that the correct modifications were made. It worked!
#
#
#Changing the classification of a column
#
#Sometimes there are cases where a column that is SUPPOSED to be classified as numerical (meaning that it contains numerical values)
#is classified as holding characters (a classification very similar to Python's classification of a string).
#Here is how to change it (in this case that you had to).
#
smaller_exchanges$Date <- as.numeric(smaller_exchanges$Date)
#
#
#Notice how you use the "$" sign to refer to a column in R. 
smaller_exchanges$`$/Thai Baht` #Shows me just that ONE column. 
#
#
#Interpolations
#
#
#For the purposes of simplicity, we are only going to work with a select number of columns. In reality, 
#because most data frames tend to have SO many different variables, researchers usually select only CERTAIN
#data frames of focus and work with them. When you get time, come back to this code, and try using different data frames and working from there. 
#
#We are going to work with the Chinese Yuan, Japanese Yen, Singapore Dollar, Indian Rupee, and UAE Dirham. 
exchanges_select <- exchanges %>% select("Date", "CNY.X", "JPY.X", "SGD.X", "INR.X", "AED.X") #The abbreviations for each country
#were obtained from the Kaggle website from which we downloaded the data. 
head(exchanges_select) #Checks to see that the correct changes were made. Notice how we are frequently
#using our basic 6 R commands to check whether our code worked correctly. 
#
#The Linear Interpolation
summary(exchanges_select) #Summary of the number of missing values in each column within the 
#dataset "exchanges_select". Almost every column has at least one missing value. 
#
#For the LESS variable columns, we will perform a linear interpolation. 
#Performing a Linear Interpolation for the Chinese Yuan, UAE Dirham, and Singapore Dollar using the "zoo" package
exchanges_select$CNY.X <- ifelse(is.na(exchanges_select$CNY.X), na.approx(exchanges_select$CNY.X), exchanges_select$CNY.X) #Performs a Linear Interpolation for all missing exchange rate values for all Chineese Yuan 
#values. Note: THIS is how the if/else statement looks in R. To learn more about the if else statment, refer back to the course videos or read some of the provided articles. 
#
exchanges_select$AED.X <- ifelse(is.na(exchanges_select$AED.X), na.approx(exchanges_select$AED.X), exchanges_select$AED.X) #Linear Interpolation for missing exchange rate values for the UAE Dirham
#
exchanges_select$SGD.X <- ifelse(is.na(exchanges_select$SGD.X), na.approx(exchanges_select$SGD.X), exchanges_select$SGD.X) #Linear Interpolation for all missing exchange rate values for the Singapore Dollar. 
#
summary(exchanges_select) #Checks to see that the linear interpolations were correctly made; there should be no missing values. 
#
#Performing a Cubic Spline Interpolation for the Japanese Yen and Indian Rupee, two columns which exhibited relatively GREATER ranges and thus MAY have
#greater variability than our other three columns. Note that a cubic spline interpolation tends to be more accurate than a linear interpolation. 
exchanges_select$JPY.X <- ifelse(is.na(exchanges_select$JPY.X), na.spline(exchanges_select$JPY.X), exchanges_select$JPY.X) #Uses the "na.spline" function
#from the "zoo" package to perform a cubic spline. Note that, here, we performed the DEFAULT cubic spline; there are different types of cubic splines. I recommend reading more into them
#when you can. 
exchanges_select$INR.X <- ifelse(is.na(exchanges_select$INR.X), na.spline(exchanges_select$INR.X), exchanges_select$INR.X) #Cubic Spline Interpolation for the Indian Rupee.
#
summary(exchanges_select) #Checks to see that the cubic splines worked properly; there are now no more missing values. 
#
#
#Regression Analysis
#
#Perform a Linear Regression between Two Points
#
#The following codes assesses whether there is a linear correlation between the exchange rates of the Chinese Yuan and the Japanese Yen. 
yuan.yen <- lm(exchanges_select$CNY.X ~ exchanges_select$JPY.X)
summary(yuan.yen) #Summary of the Linear Regression that was performed. 
#Notice how the adjusted the R-squared is significantly low. It is 0.08871 (adjusted). 
#That means the correlation coefficient (r) is about 0.298, a relatively weak correlation coefficient. 
#
yuan.singapore <- lm(exchanges_select$CNY.X~exchanges_select$SGD.X)
summary(yuan.singapore) #Notice that there is a HIGH r-squared value between 
#these two variables of 0.8686. Let's further explore this correlation. There seems to be a relationship between the 
#Chinese Yuan's exchange rate in relation to the US dollar and the Singapore Dollar's exchange rate. 
#
#Application of the ggplot2 package. 
#
#Note that, here, the Chinese Yuan is on the x-axis and the Singapore Dollar is on the y-axis
#The ggplot package allows for more diversification of our graphs; it allows for more sophistication and variety. 
ggplot(exchanges_select, aes(SGD.X, CNY.X)) + geom_point() + geom_smooth(model = "lm", se = TRUE) + 
  labs(title = "Singapore Dollar Exchange Rates Against the Chinese Yuan", 
       x = "Singapore Dollar Exchange Rates In Relation to the US $", 
       y = "Chinese Yuan Exchange Rates In Relation to the US $") 
#Creates a curve to represent the fluctuations within our data, using an intrinsically linear model. The low standard error of the graph created is a good indicator that
#a linear model is relatively appropriate to model the relationship between these points. 
#
#
#Let's try simplifying this curve
randomly_sampled <- sample_n(exchanges_select, 497) #Randomly sample 2500 points and plot THOSE points on our graph.
#This is to have fewer point (so it doesn't look like a jumble) but still have points that are representative of all 4956 rows.  
#Try to have a sample size is larger than 10% of our population; here our "population" is 4956 observations, so we need to select a number of rows GREATER than 10% of that.  
#This allows us to treat each observation as independent, regardless of whether they are or not. THIS assumed indepndence is important if you were to use this sample for hypothesis testing, which we talk more about in relation to
#Jupyter. 
#
ggplot(randomly_sampled, aes(CNY.X, SGD.X)) + geom_point() + ggtitle("A graph of our random sample")#A graph of our sample 
#
#
#
#Let's move to Jupyter
#
#Export our cleaned "exchanges_select" data frame to a csv file. 
#
#
write.csv(exchanges_select, "exchanges_select.csv") #Exports the data frame "exchanges_select" as a "comma-seperated value" file
#
#
#