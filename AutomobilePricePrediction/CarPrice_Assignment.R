#------------------------------------------------------------------------------------ 
#                         CAR PRICE ASSIGNMENT- LINEAR REGRESSION
#------------------------------------------------------------------------------------ 

#-------------------------------- Loading Libraries ---------------------------------
library(ggplot2)
library(MASS)
library(car)
library(tidyr)
library(corrplot)
library(gridExtra)

#--------------------------- Fetching data from csv to R ----------------------------- 

cars <- read.csv("CarPrice_Assignment.csv")
View(cars)


#------------------------------------------------------------------------------------- 
#                        Data understanding, preparation and EDA 
#------------------------------------------------------------------------------------- 


# The given data has 26 variables and 205 observations 
nrow(cars)
# > 205
ncol(cars)
# > 26


#-------------------
  

str(cars)
# If we observe there are 10 factor variables and 16 numeric/integer variables
# But on close observation Symboling should be a categorical variable. So converting it into factor
cars$symboling <- as.factor(cars$symboling)

str(cars)
# Now we got 
# 11 factor/categorical variable
# 15 numeric/integer variables


#---------------------


summary(cars)
# Glancing at the Numerical and categorical values summary which gives insight about distribution and 
# types of values present in the dataset.

# EXample: 
# >   fueltype
# >   diesel: 20
# >   gas   : 185
# there are 20 diesel cars and 185 gas cars

# similarly for numerical variable we can see the distribution
# Example:
# >    citympg        
# >  Min.   :13.00    
# >  1st Qu.:19.00    
# >  Median :24.00    
# >  Mean   :25.22    
# >  3rd Qu.:30.00    
# >  Max.   :49.00    


#---------------------


# Understading business of Automibile industry, which factors/variables effects more for the pricing
#   - Generally there will be different kind of people where few people will be looking for more mileage and 
#     few give importance to the power of car and few also look for safety features in car
#   - So If we look at the people interest we may say that mileage, horsepower, safety features play an important role
#   - So now coming back to our dataset, we can blindly say that few variables may be significant 
#     in pricing the car like - symbling, drivewheel, horsepower, citympg, highwaympg
#   - But now let us try to see if this assumtion is true by performing linear regression.


# --------------------


#  Check for duplicates
sum(duplicated(cars))
# > 0 
# no duplicate present in dataset


#---------------------


# Check for NA
sum(is.na(cars))
# > 0
# no NA values present


#---------------------


# Converting CarName column into Company and carModel
cars <- separate(data = cars, col = CarName, into = c("Company", "CarModel"), sep = " ", extra = "merge")

sum(is.na(cars$CarModel))
# > 2
# after converting there are 2 NA values in CarModel 
cars[which(is.na(cars$CarModel)), c("Company")]
# >  "subaru" "subaru"
# subaru has 2 instances where it doesnt have model name 

# Anyways we will not consider CarModel as independent variable for model building, hence removing this column
cars <- cars[, -4]

# Also removing carId column from the dataset
cars <- cars[,-1]

# ----------------------


# Checking for spelling mistakes in car make i.e. company
unique(cars$Company)
table(cars$Company)
# >  alfa-romero        audi         bmw       buick   chevrolet       dodge       honda       isuzu      jaguar 
# >   3                   7           8           8           3          9          13           4          3 
# >  maxda       mazda     mercury  mitsubishi      nissan      Nissan     peugeot    plymouth    porcshce 
# >   2          15           1          13          17           1          11           7           1 
# >  porsche     renault        saab      subaru      toyota     toyouta   vokswagen  volkswagen       volvo 
# >    4           2             6          12          31           1           1           9          11 
# >  vw 
# >  2 

# After looking into this i have found following violations
# 1. maxda has been miss spelled, it should be mazda
# 2. nissan and Nissan are considering as different company
# 3. porcshce has been miss spelled, it should be porsche
# 4. toyouta  has been miss spelled, it should be toyota
# 5. vokswagen has been miss spelled, it should be volkswagen
# 6. short form of volkswagen has been used, i.e. vw, which should be converted to volkswagen

# converting car make to upper
cars$Company <- toupper(cars$Company)

# Replacing Wrong spellings with the rite one
cars$Company <- as.character(cars$Company)
cars$Company[which(cars$Company == "MAXDA")] <- "MAZDA"
cars$Company[which(cars$Company == "PORCSHCE")] <- "PORSCHE"
cars$Company[which(cars$Company == "TOYOUTA")] <- "TOYOTA"
cars$Company[which(cars$Company == "VOKSWAGEN")] <- "VOLKSWAGEN"
cars$Company[which(cars$Company == "VW")] <- "VOLKSWAGEN"


# ------------------------

# EDA 

#Univariate Analysis

# 1. Plot by Number of vehicles by make
ggplot(cars, aes(x = cars$Company)) + 
  geom_bar(stat= "count", fill = "blue") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of vehicles by make", x = "Company", y = "no. of vehicles") 
# Observation:
#   1. Toyata has the highest number of cars where as Mercury has the lowest


# 2. plot by Insurance risk rating
ggplot(cars, aes(x = cars$symboling)) + 
  geom_bar(stat= "count", fill = "Red") + 
  labs(title = "Insurance risk rating", x = "Rating", y = "no. of vehicles") +
  theme_minimal()
# Observation:
#   1. Surplisingly there is not a single vehicle with -3 rating
#   2. There are many unsafe vehicles 


# 3. Plot by fuel type 
ggplot(cars, aes(x = cars$fueltype)) + 
  geom_bar(stat= "count", fill = "Green") + 
  labs(title = "Fuel Type", x = "Type", y = "no. of vehicles") +
  theme_minimal()
# Observation:
#   1. There are more no. of gas vehicles


# 4. plot by Door number
ggplot(cars, aes(x = cars$doornumber)) + 
  geom_bar(stat= "count", fill = "maroon") + 
  labs(title = "Number of doors in cars", x = "Door", y = "no. of vehicles") +
  theme_minimal()
# Observation:
#   1. many vehicles have 4 doors than 2 doors


# 5. plot by Door number
ggplot(cars, aes(x = cars$carbody)) + 
  geom_bar(stat= "count", fill = "yellow") + 
  labs(title = "Number of car body type", x = "car body", y = "no. of vehicles") +
  theme_minimal()
# Observation:
#   1. It seems sedan is the famous type of vehicle, followed by hatchback


# 6. plot by Cylinder news
ggplot(cars, aes(x = cars$cylindernumber)) + 
  geom_bar(stat= "count", fill = "black") + 
  labs(title = "Number of cylinders", x = "Cylinder", y = "no. of vehicles") +
  theme_minimal()
# Observation:
#   1. Many cars have 4 no. of cylinder


# 7. Plot by Horse power
ggplot(cars, aes(x = cars$horsepower)) +
  geom_histogram(binwidth = 20, fill = "red")+
  theme_minimal() +
  labs(title = "Horse power histogram", x = "Horse power", y = "count")
# There are some outliers in horse power


# 8. Plot mielage
ggplot(cars, aes(x = cars$citympg)) +
  geom_histogram(binwidth = 5, fill = "blue")+
  theme_minimal() +
  labs(title = "Mielage", x = "City mielage", y = "count")

ggplot(cars, aes(x = cars$highwaympg)) +
  geom_histogram(binwidth = 5, fill = "blue")+
  theme_minimal() +
  labs(title = "Mielage", x = "Highway mielage", y = "count")


# 9 . Plog by Car weight

ggplot(cars, aes(x = cars$curbweight)) +
  geom_histogram(binwidth = 500, fill = "yellow")+
  theme_minimal() +
  labs(title = "Mielage", x = "Highway mielage", y = "count")



# Bivariate Analysis

# 10. Relation between price and all numeric variable.

Plot1 <- ggplot(cars, aes(x = price, y = horsepower)) + geom_point() + 
  xlab('Price') + ylab('Horsepower') + 
  ggtitle('Relationship btwn Price & Horsepower') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot2 <- ggplot(cars, aes(x = price, y = wheelbase)) + geom_point() + 
  xlab('Price') + ylab('Wheel Base') + 
  ggtitle('Relationship btwn Price & Wheel Base')+
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot3 <- ggplot(cars, aes(x = price, y = carlength)) + geom_point() + 
  xlab('Price') + ylab('Length') + 
  ggtitle('Relationship btwn Price & Car Length')+
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot4 <- ggplot(cars, aes(x = price, y = carwidth)) + geom_point() + 
  xlab('Price') + ylab('Width') + 
  ggtitle('Relationship btwn Price & Car Width') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot5 <- ggplot(cars, aes(x = price, y = carheight)) + geom_point() + 
  xlab('Price') + ylab('Height') + 
  ggtitle('Relationship btwn Price & Car Height') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot6 <- ggplot(cars, aes(x = price, y = curbweight)) + geom_point() + 
  xlab('Price') + ylab('curbweight') + 
  ggtitle('Relationship btwn Price & curbweight') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot7 <- ggplot(cars, aes(x = price, y = enginesize)) + geom_point() + 
  xlab('Price') + ylab('enginesize') + 
  ggtitle('Relationship btwn Price & enginesize') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot8 <- ggplot(cars, aes(x = price, y = stroke)) + geom_point() + 
  xlab('Price') + ylab('stroke') + 
  ggtitle('Relationship btwn Price & stroke') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot9 <- ggplot(cars, aes(x = price, y = compressionratio)) + geom_point() + 
  xlab('Price') + ylab('compressionratio') + 
  ggtitle('Relationship btwn Price & compressionratio') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot10 <- ggplot(cars, aes(x = price, y = peakrpm)) + geom_point() + 
  xlab('Price') + ylab('peakrpm') + 
  ggtitle('Relationship btwn Price & peakrpm') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot11 <- ggplot(cars, aes(x = price, y = citympg)) + geom_point() + 
  xlab('Price') + ylab('citympg') + 
  ggtitle('Relationship btwn Price & citympg') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

Plot12 <- ggplot(cars, aes(x = price, y = highwaympg)) + geom_point() + 
  xlab('Price') + ylab('highwaympg') + 
  ggtitle('Relationship between Price and highwaympg') +
  theme(plot.title = element_text(size = 6.5, face = "bold"))+
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE)

grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6, Plot7, Plot8, Plot9, Plot10, Plot11, Plot12, ncol = 3)



# 11. Correlation between variables
cor_matrix <- cor(cars[, sapply(cars, is.numeric)])
plot_cor_matrix  <- corrplot(cor_matrix, method = "number", title = "Correlation Map", 
                             order = "FPC", number.cex = 0.7)
# Observations:
#   1. Some pairs of variables are weakly correlated, with small magnitudes. For example, wheel base 
#       and horsepower are weakly correlated, whereas engine size and curb weight are strongly correlated.


# ------------------------

# Checking for Outliers

# Wheelbase --
quantile(cars$wheelbase, seq(0,1,0.01))
# Changing the outliers value
cars$wheelbase[which(cars$wheelbase > 115.544)]<-115.544

# carlength --
quantile(cars$carlength, seq(0,1,0.01))
# Changing the outliers value
cars$carlength[which(cars$carlength > 192.700)]<-192.700

# carwidth --
quantile(cars$carwidth, seq(0,1,0.01))
# There are no outliers

# carheight --
quantile(cars$carheight, seq(0,1,0.01))
# There are no outliers

# curbweight --
quantile(cars$curbweight, seq(0,1,0.01))
# Changing the outliers value of 0%
cars$curbweight[which(cars$curbweight < 1819.72)]<-1819.72

# enginesize --
quantile(cars$enginesize, seq(0,1,0.01))
# Changing the outliers value of below 3% and above 96%
cars$enginesize[which(cars$enginesize < 90.00)]<-90.00
cars$enginesize[which(cars$enginesize > 209.00)]<-209.00

# boreratio --
quantile(cars$boreratio, seq(0,1,0.01))
# Changing the outliers value of 0%
cars$boreratio[which(cars$boreratio < 2.9100)]<-2.9100

# stroke --
quantile(cars$stroke, seq(0,1,0.01))
# Changing the outliers value below 2%
cars$stroke[which(cars$stroke < 2.6400)]<-2.6400

# compressionratio --
quantile(cars$compressionratio, seq(0,1,0.01))
# This variable has a huge outliers
# Changing the outliers value above 90%
cars$compressionratio[which(cars$compressionratio > 10.9400)]<-10.9400

# horsepower --
quantile(cars$horsepower, seq(0,1,0.01))
# Changing the outliers value above 90%
cars$horsepower[which(cars$horsepower > 207.00)]<-207.00

# peakrpm --
quantile(cars$peakrpm, seq(0,1,0.01))
# Changing the outliers value above 99%
cars$peakrpm[which(cars$peakrpm > 6000)]<-6000

# citympg --
quantile(cars$citympg, seq(0,1,0.01))
# Changing the outliers value above 98%
cars$citympg[which(cars$citympg > 38.00)]<-38.00

# highwaympg --
quantile(cars$highwaympg, seq(0,1,0.01))
# Changing the outliers value above 99%
cars$highwaympg[which(cars$highwaympg > 49.88)]<-49.88


# ------------------------


# Creatig Dummy variables for all categorical variable

# symboling
dummy_symb <- model.matrix(~symboling, data= cars)
dummy_symb <- dummy_symb[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_symb)


# Company
dummy_make <- model.matrix(~Company, data= cars)
dummy_make <- dummy_make[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_make)


# fueltype
dummy_fuel <- model.matrix(~fueltype, data= cars)
dummy_fuel <- dummy_fuel[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_fuel)

# aspiration
dummy_asp <- model.matrix(~aspiration, data= cars)
dummy_asp <- dummy_asp[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_asp)

# doornumber
dummy_doorNo <- model.matrix(~doornumber, data= cars)
dummy_doorNo <- dummy_doorNo[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_doorNo)

# carbody
dummy_carBody <- model.matrix(~carbody, data= cars)
dummy_carBody <- dummy_carBody[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_carBody)

# drivewheel
dummy_driveWheel <- model.matrix(~drivewheel, data= cars)
dummy_driveWheel <- dummy_driveWheel[,-1]
# Adding4 dummy variables to dataset
cars <- cbind(cars[,-1], dummy_driveWheel)


# enginelocation
dummy_engLocation <- model.matrix(~enginelocation, data= cars)
dummy_engLocation <- dummy_engLocation[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-1], dummy_engLocation)


# enginetype
dummy_engType <- model.matrix(~enginetype, data= cars)
dummy_engType <- dummy_engType[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-6], dummy_engType)


# cylindernumber
dummy_cylNo <- model.matrix(~cylindernumber, data= cars)
dummy_cylNo <- dummy_cylNo[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-6], dummy_cylNo)


# fuelsystem
dummy_fuelSystem <- model.matrix(~fuelsystem, data= cars)
dummy_fuelSystem <- dummy_fuelSystem[,-1]
# Adding dummy variables to dataset
cars <- cbind(cars[,-7], dummy_fuelSystem)


# Checking if i have all numeric vaiables
str(cars)

# ------------------------


#------------------------------------------------------------------------------------- 
#                        Model Building and evaluation 
#------------------------------------------------------------------------------------- 

# Since the given data is very less i.e. it has just 205 observations,
# I will not create TRAINING and TEST dataset from this.
# I will use same dataset for both training and testing.

# lets crate initial model
model_1 <- lm(price~., data = cars)
summary(model_1)


# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2
step <- stepAIC(model_1, direction="both")

step



model_2 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + peakrpm + highwaympg + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_2)


## Let us check for multicollinearity 
vif(model_2)
# There are many highly collinear variables. But before removing collinear variable, lets check for p value


# On observation we see highwaympg has 0.177986 p value and 9.794645 VIF value  | 
#                and enginetypeohc has 0.174273 p value and 10.861081 VIF value
# Which indicates that these variables are insignificant

# Lets remove highwaympg and create a model
model_3 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + peakrpm + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohc + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_3)
# Observation:
#     If we observe compression ratio which was not present in the coefficients of the model_2, now
#     started appearing in model_3. 
# Now lets see the vif
vif(model_3)
# On observation we see compressionratio has 0.148198 p value and 4.816816 VIF value  | 
#                     and enginetypeohc  has 0.174273 p value and 10.861081 VIF value
# Which indicates that these variables are insignificant

# Lets remove enginetypeohc and create a model
model_4 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + peakrpm + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_4)
#  Now lets see the vif
vif(model_4)
# On observation we see compressionratio has 0.148198 p value and 4.816816 VIF value  | 
#                     and carbodyhardtop has 0.148588 p value and 2.804952 VIF value
# Which indicates that these variables are insignificant


# Lets remove carbodyhardtop and create a model
model_5 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + peakrpm + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                carbodysedan + carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_5)
#  Now lets see the vif
vif(model_5)
# On observation we see carbodysedan has 0.114867 p value and 8.056854 VIF value   
# Which indicates that these variables are insignificant


# Lets remove carbodysedan and create a model
model_6 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + peakrpm + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_6)
#  Now lets see the vif
vif(model_6)
# On observation we see carbodysedan peakrpm, symboling0, symboling1, carbodywagon all p values and greater
# Which indicates that these variables are insignificant


# lets remove peakrpm variable which has high p value and high VIF
model_7 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + compressionratio + horsepower + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_7)
#  Now lets see the vif
vif(model_7)
# On observation we see compressionratio has high p values and greater VIF
# Which indicates that these variables are insignificant

# lets remove compressionratio variable which has high p value and high VIF
model_8 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + horsepower + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                carbodywagon + dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_8)
#  Now lets see the vif
vif(model_8)
# On observation we see dummy_fuel, carbodywagon, fuelsystem2bbl has high p values and greater VIF
# Which indicates that these variables are insignificant


# lets remove carbodywagon variable which has high p value and high VIF
model_9 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + horsepower + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                fuelsystem2bbl, data = cars)
summary(model_9)
#  Now lets see the vif
vif(model_9)
# On observation we see fuelsystem2bbl has high p values and greater VIF
# Which indicates that these variables are insignificant


# lets remove fuelsystem2bbl variable which has high p value and high VIF
model_10 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                curbweight + horsepower + 
                symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + dummy_fuel + carbodyhatchback + 
                dummy_engLocation + enginetypedohcv + 
                enginetypel + enginetypeohcv + enginetyperotor + 
                cylindernumberfive + cylindernumberfour + cylindernumbersix
                , data = cars)
summary(model_10)
#  Now lets see the vif
vif(model_10)
# On observation we see dummy_fuel has high p values but its VIF is below 2
# Which indicates that these variables are insignificant


# lets remove dummy_fuel variable which has high p value and high VIF
model_11 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 symboling0 + symboling1 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + enginetypedohcv + 
                 enginetypel + enginetypeohcv + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_11)
#  Now lets see the vif
vif(model_11)
# On observation we see symboling1, CompanyVOLVO, carbodyhatchback has high p values
# Which indicates that these variables are insignificant

# lets remove symboling1 variable which has high p value and high VIF
model_12 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 symboling0 + CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + enginetypedohcv + 
                 enginetypel + enginetypeohcv + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_12)
#  Now lets see the vif
vif(model_12) 
# On observation we see symboling0, CompanyVOLVO, carbodyhatchback has high p values compared to others
# Which indicates that these variables are insignificant


# lets remove symboling0 variable which has high p value
model_13 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + enginetypedohcv + 
                 enginetypel + enginetypeohcv + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_13)
#  Now lets see the vif
vif(model_13) 
# On observation we see , enginetypeohcv has high p values compared to others
# Which indicates that these variables are insignificant


# lets remove enginetypeohcv variable which has high p value
model_14 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + enginetypedohcv + 
                 enginetypel + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_14)
#  Now lets see the vif
vif(model_14) 
# On observation we see enginetypedohcv has high p values compared to others
# Which indicates that these variables are insignificant



# lets remove enginetypedohcv variable which has high p value
model_15 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + 
                 enginetypel + enginetyperotor + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_15)
#  Now lets see the vif
vif(model_15) 

# lets remove enginetyperotor 
model_16 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + 
                 enginetypel  + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix
               , data = cars)
summary(model_16)
#  Now lets see the vif
vif(model_16) 


# lets remove cylindernumbersix 
model_17 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + 
                 enginetypel  + 
                 cylindernumberfive + cylindernumberfour
               , data = cars)
summary(model_17)
#  Now lets see the vif
vif(model_17) 


# lets remove cylindernumberfour 
model_18 <- lm(formula = price ~ wheelbase + carlength + carwidth + carheight + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + 
                 enginetypel  + 
                 cylindernumberfive
               , data = cars)
summary(model_18)
#  Now lets see the vif
vif(model_18) 

# lets remove carheight 
model_19<- lm(formula = price ~ wheelbase + carlength + carwidth  + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO + carbodyhatchback + 
                 dummy_engLocation + 
                 enginetypel  + 
                 cylindernumberfive
               , data = cars)
summary(model_19)
#  Now lets see the vif
vif(model_19) 


# lets remove wheelbase 
model_20 <- lm(formula = price ~  carlength + carwidth  + 
                curbweight + horsepower + 
                CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                CompanyVOLVO + carbodyhatchback + 
                dummy_engLocation + 
                enginetypel  + 
                cylindernumberfive
              , data = cars)
summary(model_20)
#  Now lets see the vif
vif(model_20) 

# lets remove wheelbase 
model_21 <- lm(formula = price ~  carlength + carwidth  + 
                 curbweight + horsepower + 
                 CompanyBMW + CompanyBUICK + CompanyCHEVROLET + 
                 CompanyDODGE + CompanyHONDA + CompanyISUZU + CompanyMAZDA + 
                 CompanyMERCURY + CompanyMITSUBISHI + CompanyNISSAN + CompanyPLYMOUTH + 
                 CompanyRENAULT + CompanySUBARU + CompanyTOYOTA + CompanyVOLKSWAGEN + 
                 CompanyVOLVO  + 
                 dummy_engLocation + 
                 enginetypel  + 
                 cylindernumberfive
               , data = cars)
summary(model_21)
#  Now lets see the vif
vif(model_21) 



# NOW ITS A BUSINESS CALL TO CONSIDER THE CAR MAKE i.e SHOULD WE CONSIDER ALL COMPANY AS WELL IN THE MODEL 
# BUILDING. BECAUSE DIFFERENT BRANDS MAY PRICE THEIR CAR DIFFERENTLY FOR SAME TYPE OF FACTORS.
# HENCE IF BUSINESS TAKES A DECISION TO CONSIDER ALL CAR MAKES FOR OUR PEICE MODEL, THEN THIS model_21 
# WOULD BE FINAL MODEL TO PREDICT THE PRICES.

# R-squared:  0.9386,	Adjusted R-squared:  0.9309 


# Now lets test our model with the same data

#predicted shows
cars$Predicted_price <- predict(model_21, cars)
cars$error <-  cars$price - cars$Predicted_price

# Now, we need to test the r square between actual and predicted prices 
r <- cor(cars$price,cars$Predicted_price)
rsquared <- cor(cars$price,cars$Predicted_price)^2
rsquared

#Now lets visualize the actual and predicted value

# adding id to all rows
cars$id <- c(1:205)

# Plot - Actual vs Predicted prices
ggplot(cars, aes(id, price)) + geom_line(aes(colour = "actual" )) +
   geom_line(aes(x=id, y=Predicted_price, colour="predicted"))


# If we look at the plot, prediction almost follows the actual prices.





