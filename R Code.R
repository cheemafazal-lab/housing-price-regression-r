### Austin Housing Data. Assignment 1. #####

#### Loading the data using the Global Environment ###
library(readxl)
austinHousing <- read_excel("~/Desktop/AUSTIN HOUSING DATA/austinHousing.xlsx")


### Descriptive Statistics for the Data at Hand########

summary(austinHousing)
head(austinHousing)
tail(austinHousing)
str(austinHousing)

### using sapply to loop over columns and give comprehensive statistics ###
### Measuring Central Tendency of the data across multiple columns###
sapply(austinHousing, mean, na.rm = TRUE)

### sapply automatically ignores the variables that are not logical or numeric ###
### sapply automatically ignores NA values ###

### Measuring Central Tendency of the Data using sapply ###
sapply(austinHousing, sd, na.rm = TRUE)

### for the 9 colums that are NA we will get frequency tables ###

table(austinHousing$city)
table(austinHousing$association)
table(austinHousing$cooling)
table(austinHousing$garage)
table(austinHousing$heating)
table(austinHousing$spa)
table(austinHousing$view)
table(austinHousing$type)

### data quality step ###
### we can see that austinHousing$heating has inconsistent values ####
#### we fix this by replacing "n"s with "no"s and "y"s with "yes"s ####

austinHousing$heating[austinHousing$heating == "y"] <- "yes"
austinHousing$heating[austinHousing$heating == "n"] <- "no"

table(austinHousing$heating) ### this cleaned the column ###

### continuing with the descriptive stats ###
sapply(austinHousing, median)
sapply(austinHousing, IQR)
sapply(austinHousing, range)

### using the psych package from class handout for a clear table ###
install.packages("psych")
library(psych)
describe(austinHousing)

###############################################################################


### Addressing Data Quality issues #### Firstly here in this section, but also as we go along. ######

### To do this, we first replace outliers with NAs and later remove all rows with the NAs. If we see that not many observations are removed, we continue####
### otherwise we take a different approach ##### that is if there is a significant portion of the data removed #######

### For this we use the following function ####
### This function was taken from ChatGPT, when given the prompt "give me a function that replaces outliers in Base R"
### This follows on the work of Tukey (1977) ### Where 1.5 is the chosen number for the range of outliers ####
replace_outliers <- function(x) {
  if (!is.numeric(x)) return(x)
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr ### this is from the work of Tukey (1977)" "who also suggested that we should have outliers replaced with robust stats like the median"
  upper <- q3 + 1.5 * iqr ### Tukey also set 1.5 as an appropriate number to find the upper and lower limit to spot outliers ####
  
  x[x < lower | x > upper] <- NA
  
  x
}

### Function exapained: This function basically firstly ignores all columns that are not numeric,
###then creates the 1st, 3rd, and inter quartile range. ###
### it then sets the upper and lower limits using the upper and lower limit formula and then replaces any values that do not fall within those limits with NA####
### This makes it easier for us to remove outliers rather than having to create boxplots, spotting outliers and manually having to go in and remove or replace them with NA ###

### Once this function is defined, we apply it to our dataset ##### using sapply #####
### I will make a copy of my data so as to keep the original data intact, so I can come back to it whenever I want ####
austindata <- austinHousing

austindata <- as.data.frame(sapply(austindata, replace_outliers))

##### now we remove NAs from the data to see what we get####

sum(!complete.cases(austindata))
### this gives us a value of 7915, meaning 7915 rows have NAs in them. ### This is too much and we cannot remove all these rows ####

### Instead we will replace outlier values with the median ### This is from Tukey (1977), who recommended replacing outliers with robust stats like the median###
### we use the same function but instead of NAs we use the median and create a new function ###

replace_with_median <- function(x) {
  if (!is.numeric(x)) return(x)
  
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - 1.5 * iqr 
  upper <- q3 + 1.5 * iqr
  
  x[x < lower | x > upper] <- median(x, na.rm = TRUE)
  
  x
}
### Doing the same to replace the outliers with the median, here I will create another copy ####
austindataclean <- as.data.frame(sapply(austinHousing, replace_with_median))

summary(austindataclean)
str(austindataclean)

### for some reason all data has been turned into a character, will be formatting the data now ###

### Correct format####
### using lapply to convert selected columns to factor, and later some to numeric, as well as the ones which need to be left as characters ###
### going to do this manually without any function ###

austindataclean$tax_rate <- as.numeric(austindataclean$tax_rate)
austindataclean$garage_spaces <- as.factor(austindataclean$garage_spaces)
austindataclean$association <- as.factor(austindataclean$association)
austindataclean$cooling <- as.factor(austindataclean$cooling)
austindataclean$garage <- as.factor(austindataclean$garage)
austindataclean$heating <- as.factor(austindataclean$heating)
austindataclean$spa <- as.factor(austindataclean$spa)
austindataclean$view <- as.factor(austindataclean$view)
austindataclean$type <- as.factor(austindataclean$type)
austindataclean$parking <- as.factor(austindataclean$parking)
austindataclean$year_built <- as.POSIXct(austindataclean$year_built) ### this gives an error ###
austindataclean$accessibility_features <- as.factor(austindataclean$accessibility_features)
austindataclean$appliances <- as.factor(austindataclean$appliances)
austindataclean$appliances <- as.factor(austindataclean$appliances)
austindataclean$parking_features <- as.factor(austindataclean$parking_features)
austindataclean$patio_porch_features <- as.factor(austindataclean$patio_porch_features)
austindataclean$security_features <- as.factor(austindataclean$security_features)
austindataclean$waterfront_features <- as.factor(austindataclean$waterfront_features)
austindataclean$window_features <- as.factor(austindataclean$window_features)
austindataclean$community_features <- as.factor(austindataclean$community_features)
austindataclean$sqft_lot <- as.numeric(austindataclean$sqft_lot)
austindataclean$sqft_living <- as.numeric(austindataclean$sqft_living)
austindataclean$primary_schools <- as.factor(austindataclean$primary_schools)
austindataclean$elementary_schools <- as.factor(austindataclean$elementary_schools)
austindataclean$middle_schools <- as.factor(austindataclean$middle_schools)
austindataclean$high_schools <- as.factor(austindataclean$high_schools)
austindataclean$school_dist <- as.numeric(austindataclean$school_dist)
austindataclean$school_rating <- as.numeric(austindataclean$school_rating)
austindataclean$school_size <- as.numeric(austindataclean$school_size)
austindataclean$student_teacher <- as.numeric(austindataclean$student_teacher)
austindataclean$bathrooms_num <- as.numeric(austindataclean$bathrooms_num)
austindataclean$bedrooms_num <- as.numeric(austindataclean$bedrooms_num)
austindataclean$stories_num <- as.numeric(austindataclean$stories_num)
austindataclean$price_changes <- as.factor(austindataclean$price_changes)
austindataclean$sale_date <- as.numeric(austindataclean$sale_date)
austindataclean$sale_month <- as.numeric(austindataclean$sale_month)
austindataclean$sale_year <- as.numeric(austindataclean$sale_year)
austindataclean$price_source <- as.character(austindataclean$price_source)
austindataclean$photos_num <- as.character(austindataclean$photos_num)
austindataclean$sale_price <- as.numeric(austindataclean$sale_price)

### there is an error while converting the date to POSIXlt, or the date format, we will leave the dates as they are for now ####

str(austindataclean)

### now we check for the NA values ####

colSums(is.na(austindataclean))

###theres 12 missing values in price_source ####
### we omit these ####

austindataclean <- na.omit(austindataclean)
colSums(is.na(austindataclean))

########### THIS CONCLUDES INITAL DATA CLEANING #########################
#############################################################################################################################################################
#############################################################################################################################################################


########### THE 5 HYPOTHESES #############################################

### 1st Hypothesis is that sqft_living is positively associated with sale_price ###########
sqft_living #### Positive correlation Prediction
sale_price

### 2nd Hypothesis is that bathroom_num is positively associated with sale_price ##########
bathrooms_num #### Positive correlation Prediction
sale_price

### 3rd Hypothesis is that school_rating is positively associated with sale_price #########
school_rating #### Positive correlation Prediction
sale_price

### 4th Hypothesis is that garage_spaces are positively associated with sale_price ########
garage_spaces ### Positive correlation Prediction
sale_price

### 5th Hypothesis is tax_rate is negatively associated with sale_price ###################
tax_rate ### Negative correlation Prediction
sale_price

### We arrive at these through consulting the wider literature and making inferences ########
### The next step is to visualise these #######

### First we will also inspect each variable to be used in the visualization to check for further data quality issues ########
######################################
### HYPOTHESIS 1 #####################
######################################

describe(austindataclean$sqft_living)
summary(austindataclean$sqft_living)

#### first lets plot the dependent variable itself in a histogram #####
install.packages("ggplot2")
library(ggplot2)

### The ggplot code is taken from the DataCamp Course "Introduction to Data Visualization to ggplot 2" #####
### https://app.datacamp.com/learn/courses/introduction-to-data-visualization-with-ggplot2 #####

ggplot(austindataclean, aes(x = sale_price)) +
  geom_histogram(bins= 30) + #### bins are added to declutter the graph #####
labs(title = "Sale Price Distribution", x= "Sale Price (USD)") 

### We can see that the data is right-skewed and most houses lie in the range of 300k to 600k ######
### There are few houses that are pushing the price as they are expensive hence the right-skewnes ######
### Overall good data to work with lol ####

### More than 2000 houses are around 400k #####
mean(austindataclean$sale_price)
### The mean is 466,964.4 USD ################

### Now we check the second variable ###########

ggplot(austindataclean, aes(x = sqft_living)) +
  geom_histogram(bins = 30) +
  labs(title = "Living Area Distribution",
       x = "Living Area (SQFT)")
mean(austindataclean$sqft_living)

#### This is rightly skewed as well ######### 

#### Now we plot these variables together and the method is linear regression in the ggplot ####
#### This will automatically plot the regression line ############

ggplot(austindataclean, aes(x = sqft_living, y= sale_price)) +
  geom_point(alpha = 0.2, size = 0.1) + ### we use alpha(transparency) and size(size of the points) to declutter the plot so that there is less noise. 
  geom_smooth(method = "lm", na.rm = TRUE) + ### we also add a title to the plot ####
labs(title = "Relationship between Living Area and Sale Price", x= "Living Area (SQFT)", y = "Price (USD)")
#### The line shows a positive linear trend ##########


############################################################################################################
########### HYPOTHESIS 2 ###################################################################################
############################################################################################################

#### Lets inspect bathroom_num with a frequency table ######
table(austindataclean$bathrooms_num)
####this should be numeric####
### confirming if the variable is numeric####
austindataclean$bathrooms_num <- as.numeric(austindataclean$bathrooms_num)

table(austindataclean$bathrooms_num)

### Histogram of bathroom numbers ####

ggplot(austindataclean, aes(x = bathrooms_num)) +
  geom_bar(bins = 30) +
  labs(title = "Bathroom Number Distribution",
       x = "Number of Bathrooms")

### Plotting with sale price ###### We plot a boxplot because data seems to be CATEGORICAL #######

ggplot(austindataclean, aes(x = factor(bathrooms_num), y= sale_price)) + ### For a boxplot we need the data to be a factor, but we can not run regression on a factor ########
geom_boxplot() +
  labs(title = "Relationship between Bathroom Number and Sale Price", x= "Number of Bathrooms", y = "Price (USD)")


### Here we can see that we still have a positive relationship #### Although a bit less pronounced #####
#### Lets check the Pearsons Correlation ##### Although we will come to the bivariate relationships later ####

cor(austindataclean$bathrooms_num, austindataclean$sale_price, method = "pearson")
### The answer is 0.33 ### Its a weak positive relationship#### 


################################################################################
######################### HYPOTHESIS 3 #########################################
################################################################################

#### Lets plot the school rating ############
ggplot(austindataclean, aes(x = school_rating)) +
  geom_bar(bins = 50) +
  labs(title = "School Rating Bar Plot", x = "School Rating")

#### This gives a Multi-Modal Distribution ############
#### Most schools are either rated 3-5 or 6.5 - 7.5 ##############

#### Not going to plot this with price ####### Would be repetitive ########### And I cant find an appropriate graph ######


################################################################################
############################ HYPOTHESIS 4 ######################################
################################################################################

##### Now we have garage spaces ############

summary(austindataclean$garage_spaces)

#### Seems like a CATEGORICAL variable #####
#### So we plot a Bar plot #######

ggplot(austindataclean, aes(x = garage_spaces)) +
  geom_bar() +
  labs(title = "Bar Plot of Garage Spaces", x= "Garage Spaces")

######## Garage Spaces shows that most houses have upto 5 garages (we removed extreme outliers at the beginning), with most having either non and or  2 ######


################################################################################
################################### HYPOTHESIS 5 ###############################
################################################################################


##### Tax Rate ##########

tax_rate

summary(austindataclean$tax_rate)

ggplot(austindataclean, aes(x = tax_rate)) +
  geom_histogram() +
  labs(title = "Tax Rate", x= "Tax Rate")

class(austindataclean$tax_rate)

median(austindataclean$tax_rate)

##### Theres an issue here because the Tax Rate has only one value #### Will check this manaully #######

summary(austindataclean)

################################################################################
########$$$4u###################################################################

##### Tax rate is the same accross all houses ####### So going to exclude this ###
#### I am also limiting myself to house price and relationships with house price ##### 
#### Going to add in other factors#############
##### The 4 predictors of house price that we looked at are enough #######


########################################################################################################
##### Now that we have looked at sale price, I want to also add some variation to the analysis##########
########################################################################################################

str(austindataclean)

### The 6th Hypothesis is that the predicted price can predict its lot area ##########
### lets look at the year_built column #########

summary(austindataclean$sqft_lot)

ggplot(austindataclean, aes(x= sqft_lot)) +
  geom_boxplot() +
  labs(title= "Boxplot Lot Area", x= "Lot Area (SQFT)")

class(austindataclean$sqft_lot)


###### We will conclude the plotting here and move on to the confirming the relationships and regression models#########
###### We will test and plot the 6th Hypothesis at the end again once we have confirmed our regression model for price #######


#############################################################################################################
#############################################################################################################
#############################################################################################################

##### Testing Hypothesis 1 ####################################

#### This code is taken from the datacamp course Intermediate R #######
### https://app.datacamp.com/learn/courses/intermediate-r #####
#### 2 continuous variables being tested ######
cor.test(austindataclean$sqft_living, austindataclean$sale_price, method = "pearson")
#### A moderate positive relationship is suggested with a value of 0.47 #### Will include this in the regression #####
#### The p-value is less than the conventional 0.05 meaning we can safely reject the null hypothesis ######

###### Testing Hypothesis 2 ###################################

cor.test(austindataclean$bathrooms_num, austindataclean$sale_price, method = "pearson")
##### A value of 0.33 suggests a moderate positive correlation and the p-value is again much lower than the cutoff, so the null hypothesis is rejeced here as well ####


####### Testing Hypothesis 3 ##################################

cor.test(austindataclean$school_rating, austindataclean$sale_price, method ="pearson")
##### This is again a moderate positive correlation that is statistically significant enough for us to reject the null hypothesis #######


######## Testing Hypothesis 4 ##################################

#### We need garage spaces to be numerical in order for us to run the Pearsons Correlation test ############
austindataclean$garage_spaces <- as.numeric(austindataclean$garage_spaces)
cor.test(austindataclean$garage_spaces, austindataclean$sale_price, method="pearson")

#### This is a weak positive bivariate relationship that is again statistically significant ##### The p-value is again within the limits which means that
#### Although the relationship is weak we will include this in our regression ###########################


########### Testing Hypothesis 6 #################################

cor.test(austindataclean$sale_price, austindataclean$sqft_lot, method = "pearson")

############ This is again a 0.306 value that is statistically significant and we will be using this in our regression model ############
############ Detailed explaination of the correlations performed will be included in the written report #################################



##########################################################################################################################################
##########################################################################################################################################
##########################################################################################################################################

######## Multiple Linear Regression to predict House Price ########################################

install.packages("caret")
library(caret)

set.seed(40465466)
index <- createDataPartition(austindataclean$sale_price, times = 1, p= 0.8, list = FALSE)
train <- austindataclean[index, ]
test <- austindataclean[-index, ]

model <- lm(sale_price ~ sqft_living + bathrooms_num + school_rating + garage_spaces,
            data = train)
summary(model)

########################### Testing the Accuracy of the Model ######################################

confint(model)

### Factoring in all the other variables we can see that bathroom numbers exceeds the cutoff p-value of 0.05#####
#### We might need to exclude bathroom numbers from our model to make it more robust and accurate ###############

############ Making predictions using the model at hand ############################################

predictions <- predict(model, newdata = test)

################ Evaluating accuracy ###############################################################

postResample(predictions, test$sale_price)

######## This model is not very accurate ############# We will need to improve model accuracy #################
##### A very high RMSE and low Rsquared suggest that there is a very high prediction error ####################
########### This cannot be used :"[ #############################################################################


###### We will make tweaks to the model until we get the required accuracy #####################################
######## The first change is removing bathroom_nums ############################################################
######## We repeat the entire process over again ##############################################################


install.packages("caret")
library(caret)

set.seed(40465466)
index <- createDataPartition(austindataclean$sale_price, times = 1, p= 0.8, list = FALSE)
train <- austindataclean[index, ]
test <- austindataclean[-index, ]

model <- lm(sale_price ~ sqft_living + school_rating + garage_spaces,
            data = train)
summary(model)

confint(model)

predictions <- predict(model, newdata = test)

postResample(predictions, test$sale_price)

################### The improvements are not significant and are minor ##########################################

mean(test$sale_price)
mean(train$sale_price)


########################### Lets add variables as a last resort ############

str(austindataclean)

############ For selecting my variables I will use this code snippet taken from ChatGPT to produce a correlation heatmap and pick out the highly correlated variables ########

##### This code snippet is taken directly from Chat GPT #######################################

library(ggplot2)
library(reshape2)

# Select numeric columns
num_df <- austindataclean[sapply(austindataclean, is.numeric)]

# Compute correlation matrix
corr <- cor(num_df, use = "complete.obs")

# Melt for ggplot
corr_melt <- melt(corr)

# Plot heatmap
ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_blank()) +
  labs(title = "Correlation Heatmap")

###################### Going to add student_teacher, bedroom_num, stories_num, and also keep garage_spaces############################


install.packages("caret")
library(caret)

set.seed(40465466)
index <- createDataPartition(austindataclean$sale_price, times = 1, p= 0.8, list = FALSE)
train <- austindataclean[index, ]
test <- austindataclean[-index, ]

model <- lm(sale_price ~ sqft_living + school_rating + garage_spaces + student_teacher + bedrooms_num + stories_num + garage_spaces,
            data = train)
summary(model)

confint(model)

predictions <- predict(model, newdata = test)

postResample(predictions, test$sale_price)


############################################################# Very poor accuracy even after adding all these variables ####################
################ :( #################################### Going to try something else #################################################


############# Lets go back to data cleaning ###################
class(austindataclean$garage_spaces)
class(austindataclean$school_rating)
class(austindataclean$sqft_living)

#################
boxplot(austindataclean$garage_spaces)
boxplot(austindataclean$school_rating)
boxplot(austindataclean$sqft_living)   ############# HUGE OUTLIERS FOUND!!!!!!!################


############################# Removing these outliers with code taken 
Q1 <- quantile(austindataclean$sqft_living, 0.25)
Q3 <- quantile(austindataclean$sqft_living, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers
austindataclean <- subset(austindataclean, 
                          sqft_living >= lower_bound & sqft_living <= upper_bound)

boxplot(austindataclean$sqft_living) 


################### Running regression now ######################

install.packages("caret")
library(caret)

set.seed(40465466)
index <- createDataPartition(austindataclean$sale_price, times = 1, p= 0.8, list = FALSE)
train <- austindataclean[index, ]
test <- austindataclean[-index, ]

model <- lm(sale_price ~ sqft_living + school_rating + bathrooms_num,
            data = train)
summary(model)

confint(model)

predictions <- predict(model, newdata = test)

postResample(predictions, test$sale_price)

######################## Can not improve model accuracy ############# This is the best that can be done ###############

##################### Second model ##################################################

set.seed(40465466)
index <- createDataPartition(austindataclean$sale_price, p = 0.8, list = FALSE)
train <- austindataclean[index, ]
test  <- austindataclean[-index, ]

model2 <- lm(sqft_lot ~ sale_price , data = train)

summary(model2)

confint(model2)

prediction2 <- predict(model2, newdata = test)

postResample(prediction2, test$sqft_lot)


################ Again very poor accuracy ###########################################
################ No idea on how to improve it further ###############################
######### Maybe it is the limitation of the dataset, since we have variables that only give weak to moderate positive correlation#######

############### Checking assumptions for Model 1 and Model 2 ##################################################

library(car)
vif(model)
mean(vif(model))

###### No Multicolinearity #########

####### There can be no multicolinearity in the second model as theres only one predictor #######

plot(model)

plot(model2)

########################################

library(lmtest)
dwtest(model)

dwtest(model2)









############################################################## END ###############################################################################################################################


















