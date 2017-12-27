# Installing & Loading Libraries

install.packages("caret"); install.packages("lattice"); install.packages("tidyr")
install.packages("dplyr") ; install.packages("stringr") ; install.packages("MASS")
install.packages("car") ; install.packages("e1071"); install.packages("GGally")
install.packages("cowplot") ; install.packages("caTools") ; install.packages("ROCR")
install.packages("ggplot2") ; install.packages("ggthemes") ; install.packages("corrplot");
install.packages("scales"); 

library(tidyr);library(dplyr);library(stringr);library(readxl); library(lattice)
library(MASS);library(car);library(e1071);library(GGally)
library(caret);library(cowplot);library(caTools);library(ROCR)
library(ggplot2);library(ggthemes);library(corrplot); library(scales) 

#------------------------------------------------------------------------------------# 
#                                    LOADING DATA                                    #
#------------------------------------------------------------------------------------# 

# Loading Data

emp_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data <- read.csv("general_data.csv", stringsAsFactors = F)
manager_survey_data <- read.csv("manager_survey_data.csv" , stringsAsFactors = F)
in_time <- read.csv("in_time.csv", stringsAsFactors = F)
out_time <- read.csv("out_time.csv", stringsAsFactors = F)

# Loading Data Dictionary for Data Understanding

data_dictionary <- read_excel("data_dictionary.xlsx")

#------------------------------------------------------------------------------------# 
#               Data understanding of emp_survey_data dataframe                      #
#------------------------------------------------------------------------------------#

##1. Seeing the summary & structure & checking other aspects of 
# emp_survey_data dataframe

summary(emp_survey_data)
str(emp_survey_data)
head(emp_survey_data)
nrow(emp_survey_data) # 4410
ncol(emp_survey_data) # 4
colnames(emp_survey_data)

##2. Checking for duplicate values in the EmployeeID of the emp_survey_data dataframe

anyDuplicated(emp_survey_data$EmployeeID) # 0
sum(duplicated(emp_survey_data$EmployeeID)) # 0

##3. Checking if the number of unique EmployeeID is equal to the number of rows in 
#the emp_survey_data dataframe

length(unique(emp_survey_data$EmployeeID)) == nrow(emp_survey_data) # TRUE

##4. Checking total number of NA values in the EmployeeID of the emp_survey_data dataframe

sum(is.na(emp_survey_data$EmployeeID)) # 0 ,There are no NA values in the EmployeeID variable

##4a. Checking number of NA values per each columns in the emp_survey_data dataframe

colSums(is.na(emp_survey_data), na.rm = FALSE) 

# EmployeeID EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance 
#          0                      25                      20                      38 

##4b. Checking the total number of NA values the emp_survey_data dataframe

sum(is.na(emp_survey_data)) #83

##5. Checking total number of NULL values in the emp_survey_data dataframe

sum(is.null(emp_survey_data)) # 0

##6. Checking total number of empty values in the emp_survey_data dataframe

length(which(emp_survey_data == "")) # 0, No empty values
length(which(emp_survey_data == ""|emp_survey_data == "-"|emp_survey_data == " ")) # 0, No empty values

##7. Checking the class of each variable in the emp_survey_data dataframe

sapply(emp_survey_data,class)
# The class of all variables is "integer

##8. Checking the range of each variable in the emp_survey_data dataframe

sapply(emp_survey_data,range, na.rm = T)
#      EmployeeID EnvironmentSatisfaction JobSatisfaction WorkLifeBalance
# [1,]          1                       1               1               1
# [2,]       4410                       4               4               4

##9. Checking the number of employees against each Environment Satisfaction Level

table(emp_survey_data$EnvironmentSatisfaction, exclude = NULL)
#   1    2    3    4 <NA> 
# 845  856 1350 1334   25

##9.a.  Checking the Percentage of employees against each Environment Satisfaction Level

round(prop.table(table(emp_survey_data$EnvironmentSatisfaction, exclude = NULL))*100,2)
#     1     2     3     4  <NA> 
# 19.16 19.41 30.61 30.25  0.57

##10. Checking the number of employees against each Job Satisfaction level

table(emp_survey_data$JobSatisfaction, exclude = NULL)
#   1    2    3    4 <NA> 
# 860  840 1323 1367   20

##10.a.  Checking the Percentage of employees against each Job Satisfaction level

round(prop.table(table(emp_survey_data$JobSatisfaction, exclude = NULL))*100,2)
#     1     2     3     4  <NA> 
# 19.50 19.05 30.00 31.00  0.45 

##11. Checking the number of employees against each Work Life Balance level

table(emp_survey_data$WorkLifeBalance, exclude = NULL)
#   1    2    3    4 <NA> 
# 239 1019 2660  454   38

##11.a.  Checking the Percentage of employees against each Work Life Balance level

round(prop.table(table(emp_survey_data$WorkLifeBalance, exclude = NULL))*100,2)
#    1     2     3     4  <NA> 
# 5.42 23.11 60.32 10.29  0.86

## DATA UNDERSTANDING of emp_survey_data

# 1. The emp_survey_data gives details of the Work Environment Satisfaction Level, Job Satisfaction Level
#   and Work life balance level for each employee of the organisation.
#
# 2. There are 4 variables (4 columns) & 4410 observations (4410 rows) in the emp_survey_data dataset.
#
# 3. EmployeeID: A unique ID to represent each employee of the organisation.
#   There are a total of 4410 unique employees in the emp_survey_data is given.
#
# 4. EnvironmentSatisfaction: Represents the Work Environment Satisfaction level of the employee.
#   It an Independent - Categorical Variable with four Levels.
#   Each of the are four levels of Work Environment Satisfaction is represented by a number as shown below:
#   Work Environment Satisfaction levels - Number representation for the Work Environment Satisfaction level
#                                    Low - 1
#                                 Medium - 2
#                                   High - 3
#                              Very High - 4
#
# 5. JobSatisfaction: Represents the Job Satisfaction level of the employee.
#   It an Independent - Categorical Variable with four Levels.
#   Each of the four levels of Job Satisfaction is represented by a number as shown below:
#       Job Satisfaction levels - Number representation for the Job Satisfaction level
#                           Low - 1
#                        Medium - 2
#                          High - 3
#                     Very High - 4   
#
# 6. The variable 'WorkLifeBalance' represents the Work life balance level.
#   It an Independent - Categorical Variable with four Levels.
#   Each of the four levels of Work life balance is represented by a number as shown below:
#        Work life balance levels - Number representation for the Job Satisfaction level
#                             Bad - 1
#                            Good - 2
#                          Better - 3
#                            Best - 4
#
# 7. There are a total of 83 'NA' values in the emp_survey_data dataset
#                       Variable Name - Number of NA Values
#                         EmployeeID  - 0
#            EnvironmentSatisfaction  - 25       
#                    JobSatisfaction  - 20       
#                    WorkLifeBalance  - 38
#
# 8. There are no NULL or empty values in the dataset.
#
# 9. The number & percentage of employees against each Environment Satisfaction level is as follows:
#   Environment Satisfaction levels - Number of employees  - Percentage of employees
#                         Low(1)    -        845           -     19.16%
#                      Medium(2)    -        856           -     19.41%
#                        High(3)    -        1350          -     30.61%
#                   Very High(4)    -        1334          -     30.25%
#
# 10. The number & percentage of employees against each Job Satisfaction level is as follows:
#           Job Satisfaction levels    - Number of employees  - Percentage of employees
#                            Low(1)    -        860           -     19.50%
#                         Medium(2)    -        840           -     19.05%
#                           High(3)    -        1323          -     30.00%
#                      Very High(4)    -        1367          -     31.00% 
#
# 11. The number & percentage of employees against each Work Life Balance level is as follows:
#           Work Life Balance levels   - Number of employees  - Percentage of employees
#                              Bad(1)  -        239           -     5.42%
#                             Good(2)  -        1019          -     23.11%
#                           Better(3)  -        2660          -     60.32%
#                             Best(4)  -        454           -     10.29% 


#------------------------------------------------------------------------------------# 
#           Data understanding of manager_survey_data dataframe                      #
#------------------------------------------------------------------------------------#

##1. Seeing the summary & structure & checking other aspects of 
# manager_survey_data dataframe

summary(manager_survey_data)
str(manager_survey_data)
head(manager_survey_data)
nrow(manager_survey_data) # 4410
ncol(manager_survey_data) # 3
colnames(manager_survey_data) 
# "EmployeeID"        "JobInvolvement"    "PerformanceRating"

##2. Checking for duplicate values in the EmployeeID of the manager_survey_data dataframe

anyDuplicated(manager_survey_data$EmployeeID) # 0
sum(duplicated(manager_survey_data$EmployeeID)) # 0
length(unique(manager_survey_data$EmployeeID)) # 4410

##3. Checking if the number of unique EmployeeID is equal to the number of rows in 
#the manager_survey_data dataframe

length(unique(manager_survey_data$EmployeeID)) == nrow(manager_survey_data) # TRUE

##4. Checking total number of NA values in the EmployeeID of the manager_survey_data dataframe

sum(is.na(manager_survey_data$EmployeeID)) # 0 ,There are no NA values in the EmployeeID variable

##4a. Checking number of NA values per each columns in the manager_survey_data dataframe

colSums(is.na(manager_survey_data), na.rm = FALSE) # 0 ,There are no NA values 

# EmployeeID    JobInvolvement PerformanceRating 
#          0                 0                 0 

##4b. Checking the total number of NA values the manager_survey_data dataframe

sum(is.na(manager_survey_data)) # 0

##5. Checking total number of NULL values in the manager_survey_data dataframe

sum(is.null(manager_survey_data)) # 0

##6. Checking total number of empty values in the manager_survey_data dataframe

length(which(manager_survey_data == "")) # 0
length(which(manager_survey_data == ""|manager_survey_data == "-"|manager_survey_data == " "))

##7. Checking the class of each variable in the manager_survey_data dataframe

sapply(manager_survey_data,class) # The class of all variables is "integer

##8. Checking the range of each variable in the manager_survey_data dataframe

sapply(manager_survey_data,range, na.rm = T)
#      EmployeeID JobInvolvement PerformanceRating
# [1,]          1              1                 3
# [2,]       4410              4                 4

##9. Checking the number of employees against each Performance Rating

table(manager_survey_data$PerformanceRating, exclude = NULL)
#   3    4 
#3732  678

##9.a. Checking the percentage of employees against each Performance Rating level

round(prop.table(table(manager_survey_data$PerformanceRating, exclude = NULL))*100,2)
#     3     4 
# 84.63 15.37

##10. Checking the number of employees against each Job involvement level

table(manager_survey_data$JobInvolvement, exclude = NULL)
#   1    2    3    4 
# 249 1125 2604  432

##10.a. Checking the percentage of employees against each Job involvement level

round(prop.table(table(manager_survey_data$JobInvolvement, exclude = NULL))*100,2)
#    1     2     3     4 
# 5.65 25.51 59.05  9.80

## DATA UNDERSTANDING of manager_survey_data

# 1. The manager_survey_data gives details of the Job Involvement Level & Performance rating for last year
#   as given by the manager of each employee of the organisation.
#
# 2. There are 3 variables (3 columns) & 4410 observations (4410 rows) in the manager_survey_data dataset.
#
# 3. EmployeeID: A unique identifier to represent each employee of the organisation.
#   There are a total of 4410 unique employees for which the manager_survey_data is given.
#
# 4. JobInvolvement: Represents the Job Involvement Level as given by the manager of the employee.
#   It a Independent - Categorical variable with four levels.
#   Each of the four levels  of Work Environment Satisfaction is represented by a number as shown below:
#                 Job Involvement levels - Number representation for the Job Involvement level
#                                    Low - 1
#                                 Medium - 2
#                                   High - 3
#                              Very High - 4
# 5. PerformanceRating: Represents the Performance rating for last year as given by the manager of the employee.
#   It a Independent - Categorical variable with four levels.
#    Each of the four levels of Performance rating  is represented by a number as shown below
#             Performance rating levels - Number representation for the Performance rating
#                                   Low - 1
#                                  Good - 2
#                             Excellent - 3
#                           Outstanding - 4   
# 6. The number & percentage of employees against each Performance Rating is as follows:
#             Performance rating levels - Number of employees - Percentage of employees
#                                   Low -         0           -       0%
#                                  Good -         0           -       0%
#                             Excellent -         3732        -       84.63%
#                           Outstanding -         678         -       15.37%
#   Most of the 4410 employees (around 84.63%) have received Excellent performance rating & 
#   some employees (around 15.37%) have received Outstanding performance rating.
#   None of the 4410 employees have received Good or Low performance rating.
#
# 7. The number & percentage of employees against each Job involvement level is as follows:
#   Job Involvement levels - Number of employees  - Percentage of employees
#                   Low    -        249           -     5.65%
#                Medium    -        1125          -     25.51%
#                  High    -        2604          -     59.05%
#             Very High    -        432           -     9.80%
#
# 8. There are no 'NA' or 'NULL' or empty values in the dataset.

#------------------------------------------------------------------------------------# 
#                  Data understanding of general_data dataframe                      #
#------------------------------------------------------------------------------------#

##1. Seeing the summary & structure & checking other aspects of 
# general_data dataframe

summary(general_data)
str(general_data)
head(general_data)
nrow(general_data) # 4410
ncol(general_data) # 24
colnames(general_data) 
# [1] "Age"                     "Attrition"               "BusinessTravel"         
# [4] "Department"              "DistanceFromHome"        "Education"              
# [7] "EducationField"          "EmployeeCount"           "EmployeeID"             
# [10] "Gender"                  "JobLevel"                "JobRole"                
# [13] "MaritalStatus"           "MonthlyIncome"           "NumCompaniesWorked"     
# [16] "Over18"                  "PercentSalaryHike"       "StandardHours"          
# [19] "StockOptionLevel"        "TotalWorkingYears"       "TrainingTimesLastYear"  
# [22] "YearsAtCompany"          "YearsSinceLastPromotion" "YearsWithCurrManager"   

##2. Checking for duplicate values in the EmployeeID of the general_data dataframe

anyDuplicated(general_data$EmployeeID) # 0
sum(duplicated(general_data$EmployeeID)) # 0
length(unique(general_data$EmployeeID)) # 4410

##3. Checking if the number of unique EmployeeID is equal to the number of rows in 
#the general_data dataframe

length(unique(general_data$EmployeeID)) == nrow(general_data) # TRUE

##4. Checking total number of NA values in the EmployeeID of the general_data dataframe

sum(is.na(general_data$EmployeeID)) # 0,There are no NA values in the EmployeeID variable

##4a. Checking number of NA values per each columns in the general_data dataframe

colSums(is.na(general_data), na.rm = FALSE) 

#                   Age               Attrition          BusinessTravel              Department 
#                     0                       0                       0                       0 
#      DistanceFromHome               Education          EducationField           EmployeeCount 
#                     0                       0                       0                       0 
#            EmployeeID                  Gender                JobLevel                 JobRole 
#                     0                       0                       0                       0 
#         MaritalStatus           MonthlyIncome      NumCompaniesWorked                  Over18 
#                     0                       0                      19                       0 
#     PercentSalaryHike           StandardHours        StockOptionLevel       TotalWorkingYears 
#                     0                       0                       0                       9 
# TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager 
#                     0                       0                       0                       0 


##4b. Checking the total number of NA values the general_data dataframe

sum(is.na(general_data)) # 28

##5. Checking total number of NULL values in the general_data dataframe

sum(is.null(general_data)) # 0

##6. Checking total number of empty values in the general_data dataframe

length(which(general_data == "")) # 0 , No empty values
length(which(general_data == ""|general_data == "-"|general_data == " ")) # 0 , No empty values

##7. Checking the class of each variable in the general_data dataframe

sapply(general_data,class) 

##7a. Checking the variables with the class as 'integer'

which(sapply(general_data,class) == "integer")

#                   Age        DistanceFromHome               Education           EmployeeCount 
#                     1                       5                       6                       8 
#            EmployeeID                JobLevel           MonthlyIncome      NumCompaniesWorked 
#                     9                      11                      14                      15 
#     PercentSalaryHike           StandardHours        StockOptionLevel       TotalWorkingYears 
#                    17                      18                      19                      20 
# TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion    YearsWithCurrManager 
#                    21                      22                      23                      24 

##7b. Checking the variables with the class as 'character'

which(sapply(general_data,class) == "character")

# Attrition BusinessTravel     Department EducationField 
#         2              3              4              7 
#    Gender        JobRole  MaritalStatus         Over18 
#        10             12             13             16 

##7c. Checking the values of Categorical character columns

table(general_data$Attrition, exclude = NULL)
table(general_data$BusinessTravel, exclude = NULL)
table(general_data$Department, exclude = NULL)
table(general_data$EducationField, exclude = NULL)
table(general_data$Gender, exclude = NULL)
table(general_data$JobRole, exclude = NULL)
table(general_data$Gender, exclude = NULL)
table(general_data$MaritalStatus, exclude = NULL)

##7d. Checking the values of Categorical  columns

table(general_data$Education, exclude = NULL)
table(general_data$JobLevel, exclude = NULL)
table(general_data$StockOptionLevel, exclude = NULL)
table(general_data$StandardHours, exclude = NULL)

##7d. Checking the range of each integer variable in the general_data dataframe

sapply(general_data[,c(1,5,6,8,9,11,14,15,17:24)],range, na.rm = T)


## DATA UNDERSTANDING of the variables in general_data dataset

# 1. EmployeeID: It is a unique identifier to represent each employee of the organisation.
#    There are a total of 4410 unique employees for which the general_data is given.
# 2. Age: Age of the employee.
#        The range of 'Age' Variable is from 18 to 60.
#        (Independent - Continuous Variable)
# 3. Attrition: This variable tells whether the employee left in the previous year or not.
#        Possible values are Yes & No.
#        (Dependent - Categorical Variable with two levels)
# 4.BusinessTravel: Defines how frequently the employees have travelled for business purposes in the last year.
#        There are three possible values: Non-Travel; Travel_Frequently; Travel_Rarely.
#        (Independent - Categorical Variable with three levels)
# 5. Department: Defines Employee belongs to which Department in company.
#       There are three possible values: Human Resources; Research & Development; Sales  
#       (Independent - Categorical Variable with three levels)
# 6. DistanceFromHome: Distance from employee home & the office in kilometers.
#        The range of 'DistanceFromHome' Variable is from 1 Km to 29 Kms.
#        (Independent - Continuous Variable)
# 7. Education: Education Level of the employee.
#       There are five possible values: 1 to 5. 
#       1-Below College; 2-College; 3-Bachelor; 4-Master; 5-Doctor.
#       (Independent - Categorical Variable with five levels)
# 8. EducationField: Field of education of the employee.
#       There are six possible values:Human Resources;Life Sciences;Marketing;Medical;Other;Technical Degree  
#       (Independent - Categorical Variable with six levels)
# 9. EmployeeCount: Count of the Employee. There is one possible value: 1
#       (Independent - Categorical Variable with one level)
# 10. Gender: Gender of employee. 
#        Possible values: Male ; Female
#       (Independent - Categorical Variable with two levels)
# 11. JobLevel: Job level of the employee at company on a scale of 1 to 5
#        Possible values:1;2;3;4;5
#       (Independent - Categorical Variable with five levels)   
# 12. JobRole: Name of job role of the employee in company.
#        Possible values:Healthcare Representative;Human Resources;Research Director;Sales Representative;
#        Laboratory Technician;Manager;Manufacturing Director;Research Scientist;Sales Executive
#       (Independent - Categorical Variable with nine levels) 
# 13. MaritalStatus: Marital status of the employee.
#        Possible values:Divorced; Married; Single
#       (Independent - Categorical Variable with three levels) 
# 14. MonthlyIncome: Employee's Monthly income in rupees per month.
#       The range of 'MonthlyIncome' Variable is from Rs.10090 to Rs.199990
#       (Independent - Continuous Variable)
# 15. NumCompaniesWorked: Total number of companies the employee has worked for.
#       The range of 'NumCompaniesWorked' Variable is from 0 to 9
#       (Independent - Continuous Variable)
# 16. Over18: Whether the employee is above 18 years of age or not.There is one possible value: Y
#       (Independent - Categorical Variable with one level)
# 17. PercentSalaryHike: Percent salary hike for last year.
#       The range of 'PercentSalaryHike' Variable is from 11 to 25
#       (Independent - Continuous Variable)
# 18. StandardHours: Standard hours of work for the employee.There is one possible value: 8
#       (Independent - Continuous Variable with one level)
# 19. StockOptionLevel: Stock option level of the employee.
#        Possible values:Divorced; 0;1;2;3
#       (Independent - Categorical Variable with four levels)
# 20. TotalWorkingYears: Total number of years the employee has worked so far 
#       The range of 'TotalWorkingYears' Variable is from 0 to 40
#       (Independent - Continuous Variable)
# 21. TrainingTimesLastYear: Number of times training was conducted for this employee last year.
#        Possible values:Divorced; 0;1;2;3;4;5;6
#       (Independent - Continuous Variable with seven levels)
# 22. YearsAtCompany: Total number of years spent at the company by the employee
#       The range of 'YearsAtCompanys' Variable is from 0 to 40
#       (Independent - Continuous Variable)
# 23. YearsSinceLastPromotion: Number of years since last promotion.
#       The range of 'YearsSinceLastPromotion' Variable is from 0 to 15
#       (Independent - Continuous Variable)
# 24. YearsWithCurrManager: Number of years under current manager.
#       The range of 'YearsSinceLastPromotion' Variable is from 0 to 17
#       (Independent - Continuous Variable)


## DATA UNDERSTANDING of general_data dataset

# 1. The general_data gives details of a number of attributes for each employee of the organisation.
# 2. There are 24 variables (24 columns) & 4410 observations (4410 rows) in the general_data dataset.
# 3. There are 10 Continuous Variables & 14 Categorical Variables in the general_data dataset.

#------------------------------------------------------------------------------------# 
#                  Data understanding of in_time & out_time dataframes               #
#------------------------------------------------------------------------------------#
##1. Checking Summary & Structure & other aspects of in_time 
summary(in_time)
str(in_time)
head(in_time)
nrow(in_time) # 4410 number of rows
ncol(in_time) # 262 number of columns
colnames(in_time)

##1.a. Checking Summary & Structure & other aspects of out_time

summary(out_time)
str(out_time)
head(out_time)
nrow(out_time) # 4410 number of rows
ncol(out_time) # 262 number of columns
colnames(out_time)

##2. Checking for duplicate values in the column 'X' of the in_time & out_time dataframe

anyDuplicated(in_time$X) # 0
anyDuplicated(out_time$X) # 0

length(unique(in_time$X)) # 4410
length(unique(out_time$X)) # 4410

##3. Checking if the number of unique identifier is equal to the number of rows in 
#the in_time & out_time dataframe

length(unique(in_time$X)) == nrow(in_time) # TRUE
length(unique(out_time$X)) == nrow(out_time) # TRUE

##4. Checking total number of NA values in the unique identifier of the in_time & out_time dataframe

sum(is.na(in_time$X)) # There are no NA values
sum(is.na(out_time$X)) # There are no NA values

##4a. Checking number of NA values per each columns in the in_time & out_time dataframe

colSums(is.na(in_time), na.rm = FALSE) 
colSums(is.na(out_time), na.rm = FALSE) 

##4b. Checking the total number of NA values of in_time & out_time datasets

sum(is.na(in_time)) #109080
sum(is.na(out_time)) #109080

##5. Checking total number of empty values in the in_time & out_time datasets

length(which(in_time == "")) # There are no empty values
length(which(in_time == ""|in_time == "-"|in_time == " ")) # There are no empty values

length(which(out_time == "")) # There are no empty values
length(which(out_time == ""|out_time == "-"|out_time == " ")) # There are no empty values

##6. Checking the class of each variable in the in_time & out_time datasets

sapply(in_time,class) 
# Column 'X'  is of "integer" class
# Columns with company marked holidays are of "Logical" class
# Remaining variables are of "character" class

sapply(out_time,class) 
# Column 'X'  is of "integer" class
# Columns with company marked holidays are of "Logical" class
# Remaining variables are of "character" class


#7. Checking total number of company marked holidays
#(or columns with complete NA) in in_time dataset
length(which(colSums(is.na(in_time)) == nrow(in_time))) # 12 

#8. Checking total number of company marked holidays
#(or columns with complete NA) in out_time dataset
length(which(colSums(is.na(in_time)) == nrow(in_time))) # 12 



## DATA UNDERSTANDING of in_time & out_time dataset

# 1. The in_time dataset gives the date & time when employee enters the office, for each employee.
# 2. The in_time dataset gives the date & time when employee leaves the office, for each employee.
# 3. The in_time & out_time data spreads for the complete year of 2015.
# 4. There are 262 variables (262 columns) & 4410 observations (4410 rows) 
#   in the in_time & out_time datasets.
# 5. Each observation of the in_time & out_time datasets is linked 
#   to a unique identifier,which is represented by 'X' is the  "EmployeeID" 
# 6. Each Column of the in_time & out_time datasets represents a single day
#   of the year which is marked as the column name.
# 7. There are 12 columns which are company marked holidays and have NA throught all rows
#   in both in_time & out_time 
# 8. The other NA values imply that the employee has taken a leave of absence from work.
# 9. The average number of working hours per month for each employee can be calculated
#   using the in_tome & the out_time datasets.



#------------------------------------------------------------------------------------# 
#                            Data Preparation of in_time & out_time                  #
#           and calculation of average monthly working hours for each employee       #
#------------------------------------------------------------------------------------#

##1. Checking the number of NA values of in_time dataset
sum(is.na(in_time)) #109080

##2. Checking the number of NA values of out_time dataset
sum(is.na(out_time)) #109080


##3. Checking if 'X' is identical across in_time & out_time datasets
setdiff(in_time$X,out_time$X) #Values of column 'X' are identical for both datasets

##4. Copying  to other Dataframe
emp_id <- in_time[1]
colnames(emp_id) <- "EmployeeID" # Renaming the copied column to "EmployeeID"

##5. Converting in_time dataset to POSIXLT format

#5.a. Removing the unique identifier, column 'X' from in_time
in_time <- in_time[-1]

#5.b. Converting all columns of in_time into posixlt date format
in_time <- data.frame(sapply(in_time, function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))

#5.c. converting each column of in_time into minutes
in_time <- data.frame(sapply(in_time, function(x) as.numeric(format(x, "%H")) * 60 + as.numeric(format(x, "%M")))) 

##6. Converting out_time dataset to POSIXLT format

#6.a. Removing the unique identifier, column 'X' from out_time
out_time <- out_time[-1]

#6.b. Converting all columns of out_time into posixlt date format
out_time<- data.frame(sapply(out_time, function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M:%S")))

#6.c. converting each column of out_time into minutes
out_time <- data.frame(sapply(out_time, function(x) as.numeric(format(x, "%H")) * 60 + as.numeric(format(x, "%M")))) 

##7. Re-checking the number of NA values after conversion of in_time & out_time dataset to POSIXLT format.

sum(is.na(in_time)) #109080 , NA values same as before conversion of format
sum(is.na(out_time)) #109080 , NA values same as before conversion of format

##8. Removing holidays from in_time and out_time i.e. removing all columns with complete NA 

#8.a. Removing Columns with with complete NA values in the in_time dataset
in_time <- in_time[, colSums(is.na(in_time)) != nrow(in_time)]

#8.b. Removing Columns with with complete NA values in the out_time dataset
out_time <- out_time[, colSums(is.na(out_time)) != nrow(out_time)]

##9. Getting Number of minutes worked per day
work_minutes_per_day <- out_time - in_time

##10. Getting Number of hours worked per day
work_hours_per_day <- work_minutes_per_day/60

#10.a. Replacing all NA values with 0, since NA indicates employee's absence from work.
# So '0' implies the employee has not worked for the day
work_hours_per_day[is.na(work_hours_per_day)] <- 0

##11. Creating dummy matrix to store work hours per month

work_hours_per_month <- matrix(ncol = 12, nrow = nrow(work_hours_per_day))

work_hours_per_month[is.na(work_hours_per_month)] = 0

## 12.Calculating the total work hours for each month

for(i in names(work_hours_per_day))
{
  #JAN
  if(gsub(".*\\.(.*)\\..*","\\1",i) == "01") 
  {
    work_hours_per_month[,1] <- work_hours_per_month[,1] + work_hours_per_day[[i]]
  }
  #FEB
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "02") 
  {
    work_hours_per_month[,2] <- work_hours_per_month[,2] + work_hours_per_day[[i]]
  }
  #MAR
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "03") 
  {
    work_hours_per_month[,3] <- work_hours_per_month[,3] + work_hours_per_day[[i]]
  }
  #APR
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "04") 
  {
    work_hours_per_month[,4] <- work_hours_per_month[,4] + work_hours_per_day[[i]]
  }
  #MAY
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "05") 
  {
    work_hours_per_month[,5] <- work_hours_per_month[,5] + work_hours_per_day[[i]]
  }
  #JUN
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "06") 
  {
    work_hours_per_month[,6] <- work_hours_per_month[,6] + work_hours_per_day[[i]]
  }
  #JUL
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "07") 
  {
    work_hours_per_month[,7] <- work_hours_per_month[,7] + work_hours_per_day[[i]]
  }
  #AUG
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "08") 
  {
    work_hours_per_month[,8] <- work_hours_per_month[,2] + work_hours_per_day[[i]]
  }
  #SEP
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "09") 
  {
    work_hours_per_month[,9] <- work_hours_per_month[,9] + work_hours_per_day[[i]]
  }
  #OCT
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "10") 
  {
    work_hours_per_month[,10] <- work_hours_per_month[,10] + work_hours_per_day[[i]]
  }
  #NOV
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "11") 
  {
    work_hours_per_month[,11] <- work_hours_per_month[,11] + work_hours_per_day[[i]]
  }
  #DEC
  else if(gsub(".*\\.(.*)\\..*","\\1",i) == "12") 
  {
    work_hours_per_month[,12] <- work_hours_per_month[,12] + work_hours_per_day[[i]]
  }
}

##13. Calculating average work hours for each employee throughout the year

avg_month_hours <- data.frame(AvgMonthHours = round
                              (sapply(1:nrow(work_hours_per_month),
                                      function(i) mean(work_hours_per_month[i,]))))

##14. Combining the avg_month_hours & emp_id

avg_month_hours <- cbind(emp_id, avg_month_hours)

#------------------------------------------------------------------------------------# 
#                                    Data Preparation                                #
#------------------------------------------------------------------------------------#

##1.Collating the emp_survey_data,manager_survey_data & general_data together 
# into one single file

length(unique(emp_survey_data$EmployeeID)) # 4410, confirming the EmployeeID
length(unique(manager_survey_data$EmployeeID)) # 4410, confirming the EmployeeID
length(unique(general_data$EmployeeID)) # 4410, confirming the EmployeeID
length(unique(avg_month_hours$EmployeeID)) # 4410, confirming the EmployeeID

# Checking if 'EmployeeID' is identical across general_data & emp_survey_data datasets
setdiff(general_data$EmployeeID,emp_survey_data$EmployeeID) # Identical

# Checking if 'EmployeeID' is identical across general_data & manager_survey_data datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical

# Checking if 'EmployeeID' is identical across general_data & avg_month_hours datasets
setdiff(general_data$EmployeeID,avg_month_hours$EmployeeID) # Identical

# Merging general_data & emp_survey_data datasets
employee_data<- merge(general_data,emp_survey_data, by="EmployeeID", all = F)

# Merging employee_data & manager_survey_data datasets
employee_data<- merge(employee_data,manager_survey_data, by="EmployeeID", all = F)

# Merging employee_data & avg_month_hours datasets
employee_data<- merge(employee_data,avg_month_hours, by="EmployeeID", all = F)

##2. Checking the structure & summary of the employee_data
str(employee_data)
summary(employee_data)

##3. Converting the 'PerformanceRating' Variable from numeric to character(to correct format)

employee_data$PerformanceRating <- gsub(pattern = 3 , replacement = "excellent",
                                        employee_data$PerformanceRating )

employee_data$PerformanceRating <- gsub(pattern = 4 , replacement = "outstanding",
                                        employee_data$PerformanceRating )

##4. Converting the 'JobInvolvement' Variable from numeric to character(to correct format)

employee_data$JobInvolvement <- gsub(pattern = 1 , replacement = "low",
                                     employee_data$JobInvolvement )

employee_data$JobInvolvement <- gsub(pattern = 2 , replacement = "medium",
                                     employee_data$JobInvolvement )

employee_data$JobInvolvement <- gsub(pattern = 3 , replacement = "high",
                                     employee_data$JobInvolvement )

employee_data$JobInvolvement <- gsub(pattern = 4 , replacement = "very high",
                                     employee_data$JobInvolvement )

##5. Converting the 'WorkLifeBalance' Variable from numeric to character(to correct format)

employee_data$WorkLifeBalance <- gsub(pattern = 1 , replacement = "bad",
                                      employee_data$WorkLifeBalance )

employee_data$WorkLifeBalance <- gsub(pattern = 2 , replacement = "good",
                                      employee_data$WorkLifeBalance )

employee_data$WorkLifeBalance <- gsub(pattern = 3 , replacement = "better",
                                      employee_data$WorkLifeBalance )

employee_data$WorkLifeBalance <- gsub(pattern = 4 , replacement = "best",
                                      employee_data$WorkLifeBalance )

##6. Converting the 'JobSatisfaction' Variable from numeric to character(to correct format)

employee_data$JobSatisfaction <- gsub(pattern = 1 , replacement = "low",
                                      employee_data$JobSatisfaction )

employee_data$JobSatisfaction <- gsub(pattern = 2 , replacement = "medium",
                                      employee_data$JobSatisfaction )

employee_data$JobSatisfaction <- gsub(pattern = 3 , replacement = "high",
                                      employee_data$JobSatisfaction )

employee_data$JobSatisfaction <- gsub(pattern = 4 , replacement = "very high",
                                      employee_data$JobSatisfaction )

##7. Converting the 'EnvironmentSatisfaction' Variable from numeric to character(to correct format)

employee_data$EnvironmentSatisfaction <- gsub(pattern = 1 , replacement = "low",
                                              employee_data$EnvironmentSatisfaction )

employee_data$EnvironmentSatisfaction <- gsub(pattern = 2 , replacement = "medium",
                                              employee_data$EnvironmentSatisfaction )

employee_data$EnvironmentSatisfaction <- gsub(pattern = 3 , replacement = "high",
                                              employee_data$EnvironmentSatisfaction )

employee_data$EnvironmentSatisfaction <- gsub(pattern = 4 , replacement = "very high",
                                              employee_data$EnvironmentSatisfaction )

##8. Converting the 'Education' Variable from numeric to character(to correct format)

employee_data$Education <- gsub(pattern = 1 , replacement = "below college",
                                employee_data$Education )

employee_data$Education <- gsub(pattern = 2 , replacement = "college",
                                employee_data$Education )

employee_data$Education <- gsub(pattern = 3 , replacement = "bachelor",
                                employee_data$Education)

employee_data$Education <- gsub(pattern = 4 , replacement = "master",
                                employee_data$Education)

employee_data$Education <- gsub(pattern = 5 , replacement = "doctor",
                                employee_data$Education)

##9. Removing the variables 'EmployeeCount','Over18' & 'StandardHours' and 
#Creating a new data frame & converting all character columns of employee_data to lower case

employee_new <- mutate_if(employee_data[,-c(9,16,18)], is.character,tolower)

str(employee_new) # Checking the structure of the employee_new dataframe

##10. Converting character columns into factors

employee_new <- mutate_if(employee_new, is.character,as.factor)

##10.a. Converting variables 'JobLevel' & 'StockOptionLevel' into factors

employee_new$JobLevel <- as.factor(employee_new$JobLevel)

employee_new$StockOptionLevel <-as.factor((employee_new$StockOptionLevel))

##10.b Converting all contiunous variables from intiger to numeric

employee_new[-1] <- mutate_if(employee_new[-1],is.integer,as.numeric)

sapply(employee_new,class)  # Checking the class of each variable of employee_new

##10.c. Rearranging the columns of the employee_new dataframe

employee_new <- employee_new[,c(1,2,6,13:15,17:21,27,3:5,7:12,16,22:26)]

str(employee_new) 

##11. Checking the total number of NA values in the employee_new

sum(is.na(employee_new)) # 111

# Percentage of NA values is 111/(4410*27)*100 = 0.093%

##11.a. Checking the column-wise NA values in the employee_new

colSums(is.na(employee_new), na.rm = FALSE) 

#           EmployeeID                     Age        DistanceFromHome           MonthlyIncome      NumCompaniesWorked 
#                    0                       0                       0                       0                      19 
#    PercentSalaryHike       TotalWorkingYears   TrainingTimesLastYear          YearsAtCompany YearsSinceLastPromotion 
#                    0                       9                       0                       0                       0 
# YearsWithCurrManager               Attrition          BusinessTravel              Department               Education 
#                    0                       0                       0                       0                       0 
#       EducationField                  Gender                JobLevel                 JobRole           MaritalStatus 
#                    0                       0                       0                       0                       0 
#     StockOptionLevel EnvironmentSatisfaction         JobSatisfaction         WorkLifeBalance          JobInvolvement 
#                    0                      25                      20                      38                       0 
#    PerformanceRating 
#                    0 


##12. Removing the rows with NA values as it does not affect the rate of attrition much

##12.a. Attrition numbers before removing NA values from the dataset
table(employee_new$Attrition, exclude = NULL)
#   no  yes 
# 3699  711

# Rate of Attrition before removing NA values from the dataset
round(prop.table(table(employee_new$Attrition, exclude = NULL))*100,2)
#    no   yes 
# 83.88 16.12


##12.b. Creating a new datafram 'emp_data_1' by removing the NA values from the 'employee_new' dataframe
# as the Percentage of NA values is 111/(4410*27) = 0.093% and the rate of attrition is not affected much(0.04%)

emp_data_1 <- na.omit(employee_new) # 4300 obs. with 27 Variables in emp_data_1

##12.b. Attrition numbers after removing NA values from the dataset
table(emp_data_1$Attrition, exclude = NULL)
#   no  yes 
# 3605  695

# Rate of Attrition after removing NA values from the dataset
round(prop.table(table(emp_data_1$Attrition, exclude = NULL))*100,2)
#    no   yes 
# 83.84 16.16

sum(is.na(emp_data_1)) # 0 , Confirming the number of NA values

#------------------------------------------------------------------------------------# 
#               EDA - Univariate Analysis of emp_data_1 dataframe                    #
#------------------------------------------------------------------------------------#

##1.  Univariate analysis of the dependent 'Attrition' variable 

table(emp_data_1$Attrition, exclude = NULL)
#  no  yes 
#3605  695

##1.a. Checking the percentage of employees against 'Attrrition' variable

round(prop.table(table(emp_data_1$Attrition, exclude = NULL))*100,2)
#    no   yes 
# 83.84 16.16

Plot_Attrition <- ggplot(emp_data_1,aes(x = Attrition, fill = Attrition)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.1) +
  scale_y_continuous(labels=percent,limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  labs(title = "  Attrition Rate",
       x = "Attrition", y = "Employee Percentage")

print(Plot_Attrition)

## OBSERVARIONS w.r.t. 'Attrition' Variable

# The number of employees who have left the organisation last year is 695 out of a total of 4300
# The percentage of attrition is 16.16%

##2. Frequency of the variable 'BusinessTravel'

table(emp_data_1$BusinessTravel, exclude = NULL)
which.max(table(emp_data_1$BusinessTravel, exclude = NULL)) # travel_rarely is highest in numbers
which.min(table(emp_data_1$BusinessTravel, exclude = NULL)) # non-travel is lowest in numbers

##2.a. Checking the percentage of employees w.r.t. 'BusinessTravel' variable

round(prop.table(table(emp_data_1$BusinessTravel, exclude = NULL))*100,2)
# non-travel travel_frequently     travel_rarely 
#      10.23             18.81             70.95 

Plot_BusinessTravel <- ggplot(emp_data_1,aes(x = BusinessTravel, fill = BusinessTravel)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.1, hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Business Travel",
       x = "Business Travel", y = "Employee percentage")

print(Plot_BusinessTravel)

## OBSERVARIONS w.r.t. 'BusinessTravel' Variable

# Majority(70.95%) of the employees travelled rarely for business purposes in the last year
# 10.23% of the employees have not travelled in the last year
# 18.81% of the employees have travelled frequently in the last year


##3. Frequency of the variable 'Department' of employees

table(emp_data_1$Department, exclude = NULL)
which.max(table(emp_data_1$Department, exclude = NULL)) # research & development is highest in numbers
which.min(table(emp_data_1$Department, exclude = NULL)) # human resources is lowest in numbers

##3.a. Checking the percentage of employees w.r.t. 'Department' variable

round(prop.table(table(emp_data_1$Department, exclude = NULL))*100,2)
# human resources     research & development        sales 
#      4.33                  65.28                  30.40

Plot_Department <- ggplot(emp_data_1,aes(x = Department, fill = Department)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.1, hjust =0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Department",
       x = "Department", y = "Employee percentage")

print(Plot_Department)

## OBSERVARIONS w.r.t. 'Department' Variable

# Majority(65.28%) of the employees are in the research & development department
# 30.40% of the employees are in the sales department
# 4.33% of the employees are in the human resources department


##4. Frequency of the variable 'Education' of employees

table(emp_data_1$Education, exclude = NULL)
which.max(table(emp_data_1$Education, exclude = NULL)) # bachelor is highest in numbers
which.min(table(emp_data_1$Education, exclude = NULL)) # doctor is lowest in numbers

##4.a. Checking the percentage of employees w.r.t. 'Education' variable

round(prop.table(table(emp_data_1$Education, exclude = NULL))*100,2)
# bachelor below college       college        doctor        master 
#    38.84         11.60         19.14          3.26         27.16 

Plot_Education <- ggplot(emp_data_1,aes(x = Education, fill = Education)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t their Education",
       x = "Education", y = "Employee percentage")

print(Plot_Education)

## OBSERVARIONS w.r.t. 'Education' Variable

# Majority(38.84%) of the employees hold a Bachelor degree
# 27.16 % of the employees hold a Masters degree
# 19.14% of the employees  hold a College degree
# 11.60% of the employees  do not have a college degree
# Very few(3.26%) of the employees  hold a Doctor's degree

##5. Frequency of the variable 'EducationField' of employees

table(emp_data_1$EducationField, exclude = NULL)
which.max(table(emp_data_1$EducationField, exclude = NULL)) # life sciences is highest in numbers
which.min(table(emp_data_1$EducationField, exclude = NULL)) # human resources is lowest in numbers 

##5.a. Checking the percentage of employees w.r.t. 'EducationField' variable

round(prop.table(table(emp_data_1$EducationField, exclude = NULL))*100,2)
# human resources    life sciences        marketing          medical   other   technical degree 
#            1.86            41.07            10.91            31.72    5.51             8.93

Plot_EducationField <- ggplot(emp_data_1,aes(x = EducationField, fill = EducationField)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t their Education Field",
       x = "Education Field", y = "Employee percentage")

print(Plot_EducationField)

## OBSERVARIONS w.r.t. 'EducationField' Variable

# Majority(41.07%) of the employees are from the Life Sciences field of education
# 31.72 % of the employees belong to the medical field
# 10.91% of the employees belong to the Marketing field
# 8.93% of the employees  have a technical degree
# 5.51% of the employees  belong to other fields
# Very few(1.86%) of the employees  belong to  human resources field


##6. Frequency of the variable 'Gender' of employees

table(emp_data_1$Gender, exclude = NULL)
which.max(table(emp_data_1$Gender, exclude = NULL)) # male is highest in numbers

##6.a. Checking the percentage of employees w.r.t. 'Gender' variable

round(prop.table(table(emp_data_1$Gender, exclude = NULL))*100,2)
# female   male
#  40.21  59.79

Plot_Gender <- ggplot(emp_data_1,aes(x = Gender, fill = Gender)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t their Gender",
       x = "Gender", y = "Employee percentage")

print(Plot_Gender)

# OBSERVARIONS w.r.t. 'Gender' Variable

# Majority(59.8%) of the employees are male.
# 40.2% of employees are female.


##7. Frequency of the variable 'JobRole' of employees

table(emp_data_1$JobRole, exclude = NULL)
which.max(table(emp_data_1$JobRole, exclude = NULL)) # sales executive is highest in numbers
which.min(table(emp_data_1$JobRole, exclude = NULL)) # human resources  is lowest in numbers

##7.a. Checking the percentage of employees w.r.t. 'JobRole' variable

round(prop.table(table(emp_data_1$JobRole, exclude = NULL))*100,2)
# healthcare representative           human resources     laboratory technician 
#                      8.77                      3.58                     17.60 
#                   manager    manufacturing director         research director 
#                      6.95                      9.81                      5.47 
#        research scientist           sales executive      sales representative 
#                     19.98                     22.23                      5.60 


Plot_JobRole <- ggplot(emp_data_1,aes(x = JobRole, fill = JobRole)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t their Job Role",
       x = "Job Role", y = "Employee percentage")

print(Plot_JobRole)

## OBSERVARIONS w.r.t. 'JobRole' Variable

# The Job role of Majority(22.23%) of the employees is sales executive, followed by research scientists.
# There are 19.98% of employees in the research scientist job role.
# There are 17.60% employees as laboratory technicians.
# There are 9.81% of employees in the manufacturing director job role.
# There are 8.77% of employees in the healthcare representative job role.
# There are 6.95% of employees in the manager job role.
# There are 5.60% of employees in the sales representative job role.
# There are 5.47% of employees in the research director job role.
# The least number of employees(3.58%) are in the human resources job role.


##8. Frequency of the variable 'EnvironmentSatisfaction' of employees

table(emp_data_1$EnvironmentSatisfaction, exclude = NULL)
which.max(table(emp_data_1$EnvironmentSatisfaction, exclude = NULL)) # 'high' is highest in numbers
which.min(table(emp_data_1$EnvironmentSatisfaction, exclude = NULL)) # 'low' is lowest in numbers

##8.a. Checking the percentage of employees w.r.t. 'EnvironmentSatisfaction' variable

round(prop.table(table(emp_data_1$EnvironmentSatisfaction, exclude = NULL))*100,2)
#  high       low    medium very high
# 30.67     19.30     19.51     30.51 

Plot_EnvironmentSatisfaction <- ggplot(emp_data_1,aes(x = EnvironmentSatisfaction, fill = EnvironmentSatisfaction)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Environment Satisfaction Levels",
       x = "Environment Satisfaction Level", y = "Employee percentage")

print(Plot_EnvironmentSatisfaction)

## OBSERVARIONS w.r.t. 'EnvironmentSatisfaction' Variable

# Majority of the employees(30.67%) have high Work Environment Satisfaction.
# This is followed closely by employees(30.51%) having very high Work Environment Satisfaction.
# 19.51% employees have medium Work Environment Satisfaction.
# 19.30% employees have low Work Environment Satisfaction.


##9. Frequency of the variable 'JobSatisfaction' of employees

table(emp_data_1$JobSatisfaction, exclude = NULL)
which.max(table(emp_data_1$JobSatisfaction, exclude = NULL)) # 'very high' is highest in numbers
which.min(table(emp_data_1$JobSatisfaction, exclude = NULL)) # 'medium' is lowest in numbers

##9.a. Checking the percentage of employees w.r.t. 'JobSatisfaction' variable

round(prop.table(table(emp_data_1$JobSatisfaction, exclude = NULL))*100,2)
#  high       low    medium   very high
# 30.14     19.70     19.14       31.02

Plot_JobSatisfaction <- ggplot(emp_data_1,aes(x = JobSatisfaction, fill = JobSatisfaction)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Job Satisfaction Levels",
       x = "Job Satisfaction Level", y = "Employee percentage")

print(Plot_JobSatisfaction)

# OBSERVARIONS w.r.t. 'JobSatisfaction' Variable

# Majority of the employees(31.02%) have very high level of Job Satisfaction.
# This is followed closely by employees(30.14%) having high level of Job Satisfaction.
# 19.70% employees have low level of Job Satisfaction.
# 19.14% employees have medium level of Job Satisfaction.


##10. Frequency of the variable 'WorkLifeBalance' of employees

table(emp_data_1$WorkLifeBalance, exclude = NULL)
which.max(table(emp_data_1$WorkLifeBalance, exclude = NULL)) # 'better' is highest in numbers
which.min (table(emp_data_1$WorkLifeBalance, exclude = NULL)) # 'bad' is lowest in numbers

##10.a. Checking the percentage of employees w.r.t. 'WorkLifeBalance' variable

round(prop.table(table(emp_data_1$WorkLifeBalance, exclude = NULL))*100,2)
#  bad   best better   good 
# 5.49  10.47  60.67  23.37 

Plot_WorkLifeBalance <- ggplot(emp_data_1,aes(x = WorkLifeBalance, fill = WorkLifeBalance)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Work Life Balance Levels",
       x = "Work Life Balance Level", y = "Employee percentage")

print(Plot_WorkLifeBalance)

# OBSERVARIONS w.r.t. 'WorkLifeBalance' Variable

# Majority of the employees(60.67%) have better Work life balance.
# This is followed by employees(23.37%) have good Work life balance.
# 10.47% employees have best Work life balance.
# Very few(5.49%) employees have bad Work life balance.

##11. Frequency of the variable 'JobInvolvement' of employees

table(emp_data_1$JobInvolvement, exclude = NULL)
which.max(table(emp_data_1$JobInvolvement, exclude = NULL)) # 'high' is highest in numbers
which.min(table(emp_data_1$JobInvolvement, exclude = NULL)) #'low' is lowest in numbers

##11.a. Checking the percentage of employees w.r.t. 'JobInvolvement' variable

round(prop.table(table(emp_data_1$JobInvolvement, exclude = NULL))*100,2)
#  high       low    medium   very high 
# 58.95      5.60     25.67        9.77 

Plot_JobInvolvement <- ggplot(emp_data_1,aes(x = JobInvolvement, fill = JobInvolvement)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Job Involvement Levels",
       x = "Job Involvement Level", y = "Employee percentage")

print(Plot_JobInvolvement)

# OBSERVARIONS w.r.t. 'JobInvolvement' Variable

# Majority of the employees(58.95%) received high level of job involvement from their manager.
# This is followed by employees(25.67%) revceiving medium level of job involvement from their manager.
# 9.77% employees received very high level of job involvement from their manager.
# Very few(5.60%) received low level of job involvement from their manager.

##12. Frequency of the variable 'PerformanceRating' of employees

table(emp_data_1$PerformanceRating, exclude = NULL)
which.max(table(emp_data_1$PerformanceRating, exclude = NULL)) # 'excellent' is highest in numbers
which.min(table(emp_data_1$PerformanceRating, exclude = NULL)) # 'outstanding' is lowest in numbers

##12.a. Checking the percentage of employees w.r.t. 'PerformanceRating' variable

round(prop.table(table(emp_data_1$PerformanceRating, exclude = NULL))*100,2)
# excellent outstanding 
#     84.6        15.4 

Plot_PerformanceRating <- ggplot(emp_data_1,aes(x = PerformanceRating, fill = PerformanceRating)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.4) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1,hjust = 0) +
  scale_y_continuous(labels=percent,limits = c(0,1)) + coord_flip() +
  labs(title = "  Employee percentage w.r.t Performance Rating",
       x = "Performance Rating", y = "Employee percentage")


print(Plot_PerformanceRating)

# OBSERVARIONS w.r.t. 'PerformanceRating' Variable

# Majority of the employees(84.6%) received 'Excellent' as the Performance Rating in the last year.
# 15.4% employees received  received 'Outstanding' as the Performance Rating in the last year.
# None of the employees have got 'Good' or 'Low' as the Performance Rating in the last year.

##13.  Univariate analysis of the 'DistanceFromHome' variable

summary(emp_data_1$DistanceFromHome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   7.000   9.198  14.000  29.000 

range(emp_data_1$DistanceFromHome) #   1 to 29

Plot_DistanceFromHome <- ggplot(emp_data_1, aes(x= "",y= DistanceFromHome)) + 
  geom_boxplot(width = 0.17) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$DistanceFromHome),2)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) +
  labs(title = " Summary statistics of 'DistanceFromHome'",
       x = "Distance From Home", y = "Distance in Kilometers")

print(Plot_DistanceFromHome)

# OBSERVARIONS w.r.t. 'DistanceFromHome' Variable
# Most employees' homes are with 14 KM distance from the office
# There are no outliers in this variable.


##14.  Univariate analysis of the 'MonthlyIncome' variable

summary(emp_data_1$MonthlyIncome)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10090   29260   49360   65060   83803  199990 
quantile(emp_data_1$MonthlyIncome, seq(0,1,0.01))
range(emp_data_1$MonthlyIncome) #   10090 to 199990

Plot_MonthlyIncome <- ggplot(emp_data_1, aes(x= "",y= MonthlyIncome)) + 
  geom_boxplot(width = 0.15) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$MonthlyIncome),30000)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) + coord_flip() +
  labs(title = " Summary statistics of' Monthly Income'",
       x = "Monthly Income", y = "Monthly Income in Rupees")

print(Plot_MonthlyIncome)

# OBSERVARIONS w.r.t. 'MonthlyIncome' Variable
# 75 percentile of employees have a monthly income of less than 90000 Rupees
# There are outliers in this variable

##15.  Univariate analysis of the 'Age' variable

summary(emp_data_1$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   30.00   36.00   36.93   43.00   60.00 
quantile(emp_data_1$Age, seq(0,1,0.01))
range(emp_data_1$Age) #   18 to 60

Plot_Age <- ggplot(emp_data_1, aes(x= "",y=Age)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$Age),3)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) +
  labs(title = " Summary statistics of Age",
       x = "Age", y = "Age in Years")

print(Plot_Age)

# OBSERVARIONS w.r.t. 'Age' Variable
# The median age of employees is 36.
# The range of age of employees is from 18 to 60.

##16.  Univariate analysis of the 'PercentSalaryHike' variable

summary(emp_data_1$PercentSalaryHike)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.00   12.00   14.00   15.21   18.00   25.00 
quantile(emp_data_1$PercentSalaryHike, seq(0,1,0.01))
range(emp_data_1$PercentSalaryHike) #   11 to 25

Plot_PercentSalaryHike <- ggplot(emp_data_1, aes(x= "",y=PercentSalaryHike)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$PercentSalaryHike),1)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) +
  labs(title = " Summary statistics of 'Percent Salary Hike'",
       x = "Percent Salary Hike", y = "Salary Hike in Percentage")

print(Plot_PercentSalaryHike)

# OBSERVARIONS w.r.t. 'PercentSalaryHike' Variable
# The median for 'PercentSalaryHike' Variable is 14 %.

##17.  Univariate analysis of the 'YearsAtCompany' variable

summary(emp_data_1$YearsAtCompany)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   3.000   5.000   7.026   9.250  40.000 
quantile(emp_data_1$YearsAtCompany, seq(0,1,0.01))
range(emp_data_1$YearsAtCompany) #   0 to 40

Plot_YearsAtCompany <- ggplot(emp_data_1, aes(x= "",y=YearsAtCompany)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$YearsAtCompany),2)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) + coord_flip() +
  labs(title = " Summary statistics of 'Years At Company'",
       x = "Years At Company", y = "Years")

print(Plot_YearsAtCompany)

# OBSERVARIONS w.r.t. 'YearsAtCompany' Variable
# The median for 'YearsAtCompany' Variable is 5.
# 75 percentile of the employees have been at the company for little less than 10 years.
# Outliers exist for 20 years & above.

##18.  Univariate analysis of the 'YearsSinceLastPromotion' variable

summary(emp_data_1$YearsSinceLastPromotion)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    1.00    2.19    3.00   15.00 
quantile(emp_data_1$YearsSinceLastPromotion, seq(0,1,0.01))
range(emp_data_1$YearsSinceLastPromotion) #   0 to 15

Plot_YearsSinceLastPromotion <- ggplot(emp_data_1, aes(x= "",y=YearsSinceLastPromotion)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$YearsSinceLastPromotion),1)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) + coord_flip() +
  labs(title = " Summary statistics of 'Years Since Last Promotion'",
       x = "Years Since Last Promotion", y = "Years")

print(Plot_YearsSinceLastPromotion)

# OBSERVARIONS w.r.t. 'YearsSinceLastPromotion' Variable
# The median for 'YearsSinceLastPromotion' Variable is 1.
# 75 percentile of the employees have been promoted within 3 years
# Outliers exist for 8 years & above.


##19.  Univariate analysis of the 'YearsWithCurrManager' variable

summary(emp_data_1$YearsWithCurrManager)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   2.000   3.000   4.133   7.000  17.000 

range(emp_data_1$YearsWithCurrManager) #   0 to 17

Plot_YearsWithCurrManager <- ggplot(emp_data_1, aes(x= "",y=YearsWithCurrManager)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$YearsWithCurrManager),1)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) +
  labs(title = " Summary statistics of 'Years With Current Manager'",
       x = "Years With Current Manager", y = "Years")

print(Plot_YearsWithCurrManager)

# OBSERVARIONS w.r.t. 'YearsWithCurrManager' Variable
# The median for 'YearsWithCurrManager' Variable is 3.
# Outliers exist for 15 years & above.


##20.  Univariate analysis of the 'AvgMonthHours' variable

summary(emp_data_1$AvgMonthHours)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#111.0   130.0   146.0   151.8   164.0   228.0 
quantile(emp_data_1$AvgMonthHours, seq(0,1,0.01))
range(emp_data_1$AvgMonthHours) #   111 to 228

Plot_AvgMonthHours <- ggplot(emp_data_1, aes(x= "",y=AvgMonthHours)) + 
  geom_boxplot(width = 0.2) +
  scale_y_continuous(breaks=seq(0,max(emp_data_1$AvgMonthHours),9)) +
  stat_summary(geom="text", fun.y=quantile,aes(label=sprintf("%1.1f", ..y..)),
               position=position_nudge(x=0.12), size=3.5) +
  labs(title = " Summary statistics of 'Average Monthly Working Hours'",
       x = "Average Monthly Working Hours", y = "Hours")

print(Plot_AvgMonthHours)

# OBSERVARIONS w.r.t. 'AvgMonthHours' Variable
# The median for 'AvgMonthHours' Variable is 148.
# Outliers exist for 216 hours & above.

#------------------------------------------------------------------------------------# 
#                 EDA - Bivariate Analysis of emp_data_1 dataframe                   #
#------------------------------------------------------------------------------------#

##1.Bivariate analysis of continuous variables

##1a. Creating a dataframe with only continuous variables

emp_numeric <- emp_data_1[(lapply(emp_data_1,class) == "numeric")]

# Adding the 'Attrition' Variable.
emp_numeric <- cbind(emp_data_1$Attrition,emp_numeric) 

colnames(emp_numeric)[1] <- "Attrition" # Re-assigning the name of the column 'Attrition' 

# Converting Attrition variable, factor with 2 levels to numerical variables
levels(emp_numeric$Attrition) <- c(1,0)
emp_numeric$Attrition <- as.numeric(levels(emp_numeric$Attrition))[emp_numeric$Attrition]
summary(emp_numeric$Attrition)

##1b. Creating a correlation matrix
correlation_matrix <- round(cor(emp_numeric),2)


##1c. Creating a correlation plot from the correlation matrix
corrplot(correlation_matrix, order = "AOE",method = "square", number.font = 8, 
         number.cex = 0.8,addCoef.col= "black",cl.cex = 0.8,tl.cex = 0.9)


# OBSERVATION w.r.t. the correlation_matrix & Plot of correlation_matrix
#1c.1 There is no significant impact of the 'Attrition' variable on other continuous variables.
#1c.2 The variables 'YearsAtCompany' , 'YearsWithCurrManager' & 'YearsSinceLastPromotion' are correlated.
#1c.3 The variable 'TotalWorkingYears' is also correlated with the following variables:
#'YearsAtCompany' , 'YearsWithCurrManager' , 'YearsSinceLastPromotion' & 'Age'.
#1c.4 The variable 'AverageMonthHours' & 'Attrition' are negatively correlated, although not significantly. 


##1.d. Creating a scatter plot matrix with the following variables:
#'YearsAtCompany' , 'YearsWithCurrManager' , 'YearsSinceLastPromotion','AvgMonthHours' & 'Age'
# where correlation is significant 

emp_scatter_1 <- emp_numeric[,c('YearsAtCompany','YearsWithCurrManager',
                                'YearsSinceLastPromotion','Age','AvgMonthHours')]

pairs(emp_scatter_1,cex.labels = 1)


# OBSERVATIONS:
# Employees who have been at the company longer tend to have lesser monthly average working hours.

##1e. Scatter plot matrix for other continuous variables

emp_scatter_2 <- emp_numeric[,c('NumCompaniesWorked','TotalWorkingYears',
                                'TrainingTimesLastYear')]

pairs(emp_scatter_2,cex.labels = 1)


##2. Bivariate Analysis between 'BusinessTravel' & 'Attrition'

# Attrition percentage w.r.t. 'BusinessTravel'

round(prop.table(table(emp_data_1[,c(13,14)], exclude = NULL))*100,2)

# Attrition non-travel travel_frequently travel_rarely
#        no       9.40             14.19         60.26
#       yes       0.84              4.63         10.70


Plot.BusinessTravel_Attrition <-  ggplot(emp_data_1,aes(x = BusinessTravel, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t Business Travel" , 
       x = "Business Travel", y = "Attrition Percentage")

print(Plot.BusinessTravel_Attrition)

# OBSERVATIONS w.r.t. relation between 'BusinessTravel' & 'Attrition'
# Proportion of Attrition is higher in the employees who travel frequently.

##3. Bivariate Analysis between 'Department' & 'Attrition'

# Attrition percentage w.r.t. 'Department'

round(prop.table(table(emp_data_1[,c(13,15)], exclude = NULL))*100,2)
#            Department
# Attrition human resources research & development sales
#        no            3.07                  54.98 25.79
#       yes            1.26                  10.30  4.60

Plot.Department_Attrition <-  ggplot(emp_data_1,aes(x = Department, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t Department" , 
       x = "Department", y = "Attrition Percentage")

print(Plot.Department_Attrition)

# OBSERVATIONS w.r.t. relation between 'Department' & 'Attrition'
# Human Resource department has higher proportion of attrition,

##4. Bivariate Analysis between 'Education' & 'Attrition'

# Attrition percentage w.r.t. 'Education'

round(prop.table(table(emp_data_1[,c(13,16)], exclude = NULL))*100,2)

#            Education
# Attrition bachelor below college college doctor master
#       no     32.81         9.81   15.53   2.79  22.88
#       yes     6.02         1.79    3.60   0.47   4.28

Plot.Education_Attrition <-  ggplot(emp_data_1,aes(x = Education, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t Education" , 
       x = "Education", y = "Attrition Percentage")

print(Plot.Education_Attrition)


# OBSERVATIONS w.r.t. relation between 'Education' & 'Attrition'
#  Proportion of Attrition is higher among employees with college education.


##5. Bivariate Analysis between 'EducationField' & 'Attrition'

# Attrition percentage w.r.t. 'EducationField'

round(prop.table(table(emp_data_1[,c(13,17)], exclude = NULL))*100,2)

#            EducationField
# Attrition human resources life sciences marketing medical other technical degree
#       no             1.12         34.21      9.19   26.63  4.81             7.88
#       yes            0.74          6.86      1.72    5.09  0.70             1.05
# 

Plot.EducationField_Attrition <-  ggplot(emp_data_1,aes(x = EducationField, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t Education Field" , 
       x = "Education Field", y = "Attrition Percentage")

print(Plot.EducationField_Attrition)

# OBSERVATIONS w.r.t. relation between 'EducationField' & 'Attrition'
#  Proportion of Attrition  is higher among employeesin the human resources field.



##6. Bivariate Analysis between 'Gender' & 'Attrition'

# Attrition percentage w.r.t. 'Gender'

round(prop.table(table(emp_data_1[,c(13,18)], exclude = NULL))*100,2)

#              Gender
# Attrition female  male
#       no   34.05 49.79
#       yes   6.16 10.00

Plot.Gender_Attrition  <-  ggplot(emp_data_1,aes(x = Gender, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t Gender" , 
       x = "Gender", y = "Attrition Percentage")

print(Plot.Gender_Attrition)

# OBSERVATIONS w.r.t. relation between 'Gender' & 'Attrition'
# Proportion of attrition is higher for the male employees.


##7. Bivariate Analysis between 'JobLevel' & 'Attrition'

# Attrition percentage w.r.t. 'JobLevel'

round(prop.table(table(emp_data_1[,c(13,19)], exclude = NULL))*100,2)

#                    JobLevel
# Attrition     1     2     3     4     5
#       no  31.00 29.95 12.67  6.09  4.12
#       yes  5.79  6.40  2.23  1.19  0.56

Plot.JobLevel_Attrition  <- ggplot(emp_data_1,aes(x = JobLevel, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Plot showing proportion of Attrition w.r.t Job Level" , 
       x = "Job Level", y = "Attrition Percentage")

print(Plot.JobLevel_Attrition)

# OBSERVATIONS w.r.t. relation between 'JobLevel' & 'Attrition'
#  Proportion of Attrition is higher for the employees in Job Level 2.


##8. Bivariate Analysis between 'JobRole' & 'Attrition'

# Attrition percentage w.r.t. 'JobRole'

round(prop.table(table(emp_data_1[,c(13,20)], exclude = NULL))*100,2)

#            JobRole
# Attrition healthcare representative human resources laboratory technician  manufacturing director 
#       no                       7.49            3.09                 14.77                    8.70              
#       yes                      1.28            0.49                  2.84                    1.12              
#            JobRole
# Attrition research scientist sales executive sales representative    research director   manager
#        no               16.30           18.47                4.77               4.21       6.05 
#       yes               3.67            3.77                 0.84               1.26       0.91  



Plot.JobRole_Attrition <- ggplot(emp_data_1,aes(x = JobRole, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.1, vjust = 0.5)) +
  labs(title = "Attrition w.r.t Job Role" , 
       x = "Job Role", y = "Attrition Percentage")

print(Plot.JobRole_Attrition)

# OBSERVATIONS w.r.t. relation between 'JobRole' & 'Attrition'
# Proportion of Attrition is higher for the employees in the Research Director job role. 

##9. Bivariate Analysis between 'MaritalStatus' & 'Attrition'

# Attrition percentage w.r.t. 'MaritalStatus'

round(prop.table(table(emp_data_1[,c(13,21)], exclude = NULL))*100,2)
#                MaritalStatus
# Attrition divorced married single
#       no     19.88   39.95  24.00
#       yes     2.19    5.84   8.14

Plot.MaritalStatus_Attrition <- ggplot(emp_data_1,aes(x = MaritalStatus, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t. Marital Status" , 
       x = "Marital Status", y = "Attrition Percentage")

print(Plot.MaritalStatus_Attrition)

# OBSERVATIONS w.r.t. relation between 'MaritalStatus' & 'Attrition'
# Proportion of Attrition is higher for the employees who are single 
# followed by employees who are married.


##10. Bivariate Analysis between 'StockOptionLevel' & 'Attrition'

# Attrition percentage w.r.t. 'StockOptionLevel'

round(prop.table(table(emp_data_1[,c(13,22)], exclude = NULL))*100,2)

#               StockOptionLevel
# Attrition     0     1     2     3
#       no  35.70 34.30  8.88  4.95
#       yes  7.23  6.12  1.95  0.86


Plot.StockOptionLevel_Attrition <- ggplot(emp_data_1,aes(x = StockOptionLevel, fill = Attrition)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t. Stock Option Level" , 
       x = "Stock Option Level", y = "Attrition Percentage")

print(Plot.StockOptionLevel_Attrition)

# OBSERVATIONS w.r.t. relation between 'StockOptionLevel' & 'Attrition'
# The proportion of attrition is higher for the employees with Stock Option Level '2'.


##11. Bivariate Analysis between 'EnvironmentSatisfaction' & 'Attrition'

# Attrition percentage w.r.t. 'EnvironmentSatisfaction'

round(prop.table(table(emp_data_1[,c(13,23)], exclude = NULL))*100,2)

#               EnvironmentSatisfaction
# Attrition  high   low medium very high
#       no  26.47 14.40  16.58     26.40
#       yes  4.21  4.91   2.93      4.12


Plot.EnvironmentSatisfaction_Attrition <- ggplot(emp_data_1,aes(x = EnvironmentSatisfaction, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t. Environment Satisfaction" , 
       x = "Environment Satisfaction Level", y = "Attrition Percentage")

print(Plot.EnvironmentSatisfaction_Attrition)

# OBSERVATIONS w.r.t. relation between 'EnvironmentSatisfaction' & 'Attrition'
#  Proportion of Attrition is higher for the employees with Low Environment Satisfaction Level


##12. Bivariate Analysis between 'JobSatisfaction' & 'Attrition'

# Attrition percentage w.r.t. 'JobSatisfaction'

round(prop.table(table(emp_data_1[,c(13,24)], exclude = NULL))*100,2)

#               JobSatisfaction
# Attrition  high   low medium very high
#       no  25.16 15.19  16.00     27.49
#       yes  4.98  4.51   3.14      3.53


Plot.JobSatisfaction_Attrition  <- ggplot(emp_data_1,aes(x = JobSatisfaction, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) +
  labs(title = "Attrition w.r.t. Job Satisfaction" , 
       x = "Job Satisfaction Level", y = "Attrition Percentage")

print(Plot.JobSatisfaction_Attrition)

# OBSERVATIONS w.r.t. relation between 'JobSatisfaction' & 'Attrition'
# Proportion of attrition is higher for employees with Low Job Satisfaction Level 


##13. Bivariate Analysis between 'WorkLifeBalance' & 'Attrition'

# Attrition percentage w.r.t. 'WorkLifeBalance'

round(prop.table(table(emp_data_1[,c(13,25)], exclude = NULL))*100,2)

#               WorkLifeBalance
# Attrition   bad  best better  good
#       no   3.79  8.60  51.95 19.49
#       yes  1.70  1.86   8.72  3.88

Plot.WorkLifeBalance_Attrition <- ggplot(emp_data_1,aes(x = WorkLifeBalance, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) + 
  labs(title = "Attrition w.r.t. Work Life Balance" , 
       x = "Work Life Balance", y = "Employee Percentage")

print(Plot.WorkLifeBalance_Attrition)

# OBSERVATIONS w.r.t. relation between 'WorkLifeBalance' & 'Attrition'
# Rate of attrition is higher for the employees with Bad Work Life Balance Level.


##14. Bivariate Analysis between 'JobInvolvement' & 'Attrition'

# Attrition percentage w.r.t. 'JobInvolvement'

round(prop.table(table(emp_data_1[,c(13,26)], exclude = NULL))*100,2)

#               JobInvolvement
# Attrition  high   low medium very high
#       no  49.93  4.40  21.53      7.98
#       yes  9.02  1.21   4.14      1.79

Plot.JobInvolvement_Attrition <- ggplot(emp_data_1,aes(x = JobInvolvement, fill = Attrition)) + 
  geom_bar(position = "fill") + scale_y_continuous(labels = percent_format()) + 
  labs(title = "Attrition w.r.t. Job Involvement" , 
       x = "Job Involvement Level", y = "Attrition Percentage")

print(Plot.JobInvolvement_Attrition)

# OBSERVATIONS w.r.t. relation between 'JobInvolvement' & 'Attrition'
# Proportion of attrition is higher for the employees with low Level of Job Involvement.


##15. Bivariate Analysis between 'PerformanceRating' & 'Attrition'

# Attrition percentage w.r.t. 'PerformanceRating'

round(prop.table(table(emp_data_1[,c(13,27)], exclude = NULL))*100,2)

#            PerformanceRating
# Attrition excellent outstanding
#       no      71.28       12.56
#       yes     13.33        2.84

Plot.PerformanceRating_Attrition  <- ggplot(emp_data_1,aes(x = PerformanceRating, fill = Attrition)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) + 
  labs(title = "Attrition w.r.t. Performance Rating" , 
       x = "Performance Rating", y = "Attrition Percentage")

print(Plot.PerformanceRating_Attrition)

# OBSERVATIONS w.r.t. relation between 'PerformanceRating' & 'Attrition'
# Proportion of attrition is higher for the employees with Outstanding Performance Rating


##16. Boxplots of numeric variables w.r.t. the Atrittion


box_theme_numeric<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                          axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                          legend.position="none")

Plot.Grid_1<- plot_grid(ggplot(emp_data_1, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                          coord_flip() +theme(legend.position="none"),
                        ggplot(emp_data_1, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        ggplot(emp_data_1, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        align = "v",nrow = 1)
print(Plot.Grid_1)


# OBSERVATIONS
# Age tends to be factor in attrition.Yonger employees show more tendency to leave
# Monthly Income & Distance from Home does have a direct affect on attrition.

Plot.Grid_2<- plot_grid(ggplot(emp_data_1, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                          coord_flip() +theme(legend.position="none"),
                        ggplot(emp_data_1, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        ggplot(emp_data_1, aes(x=Attrition,y=TrainingTimesLastYear, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        ggplot(emp_data_1, aes(x=Attrition,y=AvgMonthHours, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        align = "v",nrow = 1)

print(Plot.Grid_2)

# OBSERVATIONS
# The arrtrition tends to increase with the increase in Average Monnthly working hours.
# Number of Companies Worked has an effect on attrition.

Plot.Grid_3 <-plot_grid(ggplot(emp_data_1, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
                          coord_flip() +theme(legend.position="none"),
                        ggplot(emp_data_1, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        ggplot(emp_data_1, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        ggplot(emp_data_1, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
                          coord_flip() + box_theme_numeric,
                        align = "v",nrow = 1)

print(Plot.Grid_3)


# OBSERVATIONS
# The 'TotalWorkingYears','YearsWithCurrManager'& 'YearsAtCompany'  have an effect of attrition.
# There are exceptions to the above mentioned trend.


#------------------------------------------------------------------------------------# 
#                                    Outlier Treatment                               #
#------------------------------------------------------------------------------------#

# Creating a emp_outliers dataframe which is the same as emp_data_1
emp_outliers <- emp_data_1

##1. Outlier treatment in the variable 'Age'

quantile(emp_outliers$Age, seq(0,1,0.01))
# No Outliers


##2. Outlier treatment in the variable 'Age'

quantile(emp_outliers$DistanceFromHome, seq(0,1,0.01))
# No Outliers


##3. Outlier treatment in the variable 'MonthlyIncome'

quantile(emp_outliers$MonthlyIncome, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$MonthlyIncome[which(emp_outliers$MonthlyIncome < 18576.2)] <- 18576.2


##4. Outlier treatment in the variable 'NumCompaniesWorked'

quantile(emp_outliers$NumCompaniesWorked, seq(0,1,0.01))
# No Outliers


##5. Outlier treatment in the variable 'PercentSalaryHike'

quantile(emp_outliers$PercentSalaryHike, seq(0,1,0.01))
# No Outliers


##6. Outlier treatment in the variable 'TotalWorkingYears'

quantile(emp_outliers$TotalWorkingYears, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$TotalWorkingYears[which(emp_outliers$TotalWorkingYears > 32)] <- 32


##7. Outlier treatment in the variable 'TrainingTimesLastYear'

quantile(emp_outliers$TrainingTimesLastYear, seq(0,1,0.01))
# Lets not remove


##8. Outlier treatment in the variable 'YearsAtCompany'

quantile(emp_outliers$YearsAtCompany, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$YearsAtCompany[which(emp_outliers$YearsAtCompany > 22)] <- 22


##9. Outlier treatment in the variable 'YearsSinceLastPromotion'

quantile(emp_outliers$YearsSinceLastPromotion, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$YearsSinceLastPromotion[which(emp_outliers$YearsSinceLastPromotion > 9)] <- 9


##10. Outlier treatment in the variable 'YearsWithCurrManager'

quantile(emp_outliers$YearsWithCurrManager, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$YearsWithCurrManager[which(emp_outliers$YearsWithCurrManager > 12)] <- 12


##11. Outlier treatment in the variable 'AvgMonthHours'

quantile(emp_outliers$AvgMonthHours, seq(0,1,0.01))
# Treating the outliers value
emp_outliers$AvgMonthHours[which(emp_outliers$AvgMonthHours > 216)] <- 216


#------------------------------------------------------------------------------------# 
#                    Standardisation of continuous variables                         #
#------------------------------------------------------------------------------------#

# Normalising continuous features 

emp_outliers$Age<- scale(emp_outliers$Age)

emp_outliers$MonthlyIncome<- scale(emp_outliers$MonthlyIncome) 

emp_outliers$DistanceFromHome <- scale(emp_outliers$DistanceFromHome)

emp_outliers$NumCompaniesWorked <- scale(emp_outliers$NumCompaniesWorked)

emp_outliers$PercentSalaryHike <- scale(emp_outliers$PercentSalaryHike)

emp_outliers$TotalWorkingYears <- scale(emp_outliers$TotalWorkingYears)

emp_outliers$TrainingTimesLastYear <- scale(emp_outliers$TrainingTimesLastYear)

emp_outliers$YearsAtCompany <- scale(emp_outliers$YearsAtCompany)

emp_outliers$YearsSinceLastPromotion <- scale(emp_outliers$YearsSinceLastPromotion)

emp_outliers$YearsWithCurrManager <- scale(emp_outliers$YearsWithCurrManager)

emp_outliers$AvgMonthHours <- scale(emp_outliers$AvgMonthHours)

#------------------------------------------------------------------------------------# 
#                             Dummy variable creation                                #
#------------------------------------------------------------------------------------#

# Converting target variable 'Attrition' from yes/no to factor with levels 1/0

emp_outliers$Attrition <- ifelse(emp_outliers$Attrition=="yes",1,0)


# Checking Attrition rate of prospect customer

Attrition <- (sum(emp_outliers$Attrition)/nrow(emp_outliers))*100
Attrition # 16.16% Attrition rate. 

# Creating a dataframe of categorical features
emp_factors <- emp_outliers[,14:27]

# creating dummy variables for factor attributes

emp_dummies<- data.frame(sapply
                         (emp_factors,
                           function(x) data.frame(model.matrix(~x-1,data =emp_factors))[,-1]))

# For 'PerformanceRating ' variables with 2 factors, outstanding =1 & excellent = 0
# For'Gender' variables with 2 factors, male =1 & female = 0


employee_final <- cbind(emp_outliers[,2:13],emp_dummies)
str(employee_final) # 4300 obs. of  56 variables:

#------------------------------------------------------------------------------------# 
#                                  Model Building                                    #
#------------------------------------------------------------------------------------#

##1. Separating employee_final data into training and testing data

set.seed(100)
trainindices = sample(1:nrow(employee_final), 0.7*nrow(employee_final))
employee_train = employee_final[trainindices,]
employee_test = employee_final[-trainindices,]


# Logistic Regression: 

##2. Initial model
model_1 = glm(Attrition ~ ., data = employee_train, family = "binomial")
summary(model_1) 

##3. Passing model_1 as an argument to stepAIC function for the selection of
# significant variables & elimination of insignificant variables.

library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

##4. Using the last model equation of stepwise method to create model_3

model_3 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother + EducationField.xtechnical.degree + 
                  JobLevel.x2 + JobLevel.x5 + JobRole.xlaboratory.technician + 
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xresearch.scientist + JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood + JobInvolvement.xlow + JobInvolvement.xmedium + 
                  JobInvolvement.xvery.high, family = "binomial", data = employee_train)

summary(model_3)
vif(model_3)


##5. Excluding EducationField.xtechnical.degree as p value = 0.150173  but vif =  1.041119

model_4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 + JobRole.xlaboratory.technician + 
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xresearch.scientist + JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood + JobInvolvement.xlow + JobInvolvement.xmedium + 
                  JobInvolvement.xvery.high, family = "binomial", data = employee_train)

summary(model_4)
vif(model_4)


##6. Excluding JobInvolvement.xmedium  as p value = 0.156726 & vif =   1.147948 


model_5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 + JobRole.xlaboratory.technician + 
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xresearch.scientist + JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood + JobInvolvement.xlow + 
                  JobInvolvement.xvery.high, family = "binomial", data = employee_train)

summary(model_5)
vif(model_5)


##7. excluding JobInvolvement.xvery.high as p value =0.246756 & vif = 1.055064

model_6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 + JobRole.xlaboratory.technician + 
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xresearch.scientist + JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood + JobInvolvement.xlow  
                  , family = "binomial", data = employee_train)

summary(model_6)
vif(model_6)


##8. Excluding JobInvolvement.xlow   as p value =0.219150 & vif = 1.024985 
model_7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 + JobRole.xlaboratory.technician + 
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xresearch.scientist + JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_7)
vif(model_7)


##9. Excluding JobRole.xlaboratory.technician as p value = 0.137411 & vif =  1.436374 
model_8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  JobRole.xsales.executive + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_8)
vif(model_8)

##10. Excluding JobRole.xsales.executive 
# as it has p value = 0.085783 .   & vif = 1.108815 

model_9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x2 + JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_9)
vif(model_9)

##11. Excluding JobLevel.x2  
# as it has p value =  0.102812    & vif = 1.051064
model_10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + StockOptionLevel.x1 + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_10)
vif(model_10)

##12. excluding StockOptionLevel.x1
# as it has p value =  0.067708 .   & vif =  1.019288 
model_11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + PercentSalaryHike + TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_11)
vif(model_11)

##13. excluding PercentSalaryHike 
# as it has p value =   0.067916 .     & vif =  1.032284 
model_12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + EducationField.xother +  
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_12)
vif(model_12)

##14. Excluding EducationField.xother
# as it has p value =    0.073221 .    & vif =  1.026746 

model_13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  Education.xcollege + 
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_13)
vif(model_13)

##15. Excluding Education.xcollege ; p-value =  0.031684 *  ; vif = 1.038832 

model_14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director + JobRole.xresearch.director + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_14)
vif(model_14)

##16. Excluding JobRole.xresearch.director ; p-value =  0.044788 *  ; vif = 1.037885 
model_15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  JobLevel.x5 +  
                  JobRole.xmanufacturing.director +  
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_15)
vif(model_15)

##17. Excluding JobLevel.x5 ; p-value =   0.027127 *    ; vif =  1.035037 
model_16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  JobRole.xmanufacturing.director +  
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_16)
vif(model_16)

##18. Excluding JobRole.xmanufacturing.director ; p-value =  0.003964 **   ; vif =   1.028490 
model_17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently + BusinessTravel.xtravel_rarely + 
                  Department.xresearch...development + Department.xsales + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_17)
vif(model_17)

##19. Excluding BusinessTravel.xtravel_rarely ; p-value =0.006838 ** ; vif =  3.895305

model_18 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently +
                  Department.xresearch...development + Department.xsales + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_18)
vif(model_18)

##20. Excluding Department.xsales ; p-value = 0.000428 ***; vif =  4.319699
# Since all variables have significant p-values, but  Department.xsales has high p-value
model_19 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently +
                  Department.xresearch...development + 
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_19)
vif(model_19)

##21. Excluding Department.xresearch...development; p-value = 0.138005;vif =  1.023216 
model_20 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears + 
                  TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                  AvgMonthHours + BusinessTravel.xtravel_frequently +
                  MaritalStatus.xsingle + EnvironmentSatisfaction.xlow + 
                  EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow + 
                  JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter + 
                  WorkLifeBalance.xgood ,
                family = "binomial", data = employee_train)

summary(model_20)
vif(model_20)




##22. WorkLifeBalance.xgood; p-value=  3.98e-05 ***;vif =  2.936687
model_21 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears +
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                   AvgMonthHours + BusinessTravel.xtravel_frequently +
                   MaritalStatus.xsingle + EnvironmentSatisfaction.xlow +
                   EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow +
                   JobSatisfaction.xvery.high + WorkLifeBalance.xbest + WorkLifeBalance.xbetter,
                   family = "binomial", data = employee_train)

 summary(model_21)
 vif(model_21)


##23.  Excluding WorkLifeBalance.xbest;p-value= 0.079166 .; vif = 1.167119

 model_22 <- glm(formula = Attrition ~ Age + NumCompaniesWorked +  TotalWorkingYears +
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                   AvgMonthHours + BusinessTravel.xtravel_frequently +
                   MaritalStatus.xsingle + EnvironmentSatisfaction.xlow +
                   EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow +
                   JobSatisfaction.xvery.high +WorkLifeBalance.xbetter,
                 family = "binomial", data = employee_train)

 summary(model_22)
 vif(model_22)

##24. Excluding Age; P-value =  0.001028 ** ; vif = 1.806593
 model_23 <- glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears +
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                   AvgMonthHours + BusinessTravel.xtravel_frequently +
                   MaritalStatus.xsingle + EnvironmentSatisfaction.xlow +
                   EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow +
                   JobSatisfaction.xvery.high +WorkLifeBalance.xbetter,
                 family = "binomial", data = employee_train)

 summary(model_23)
 vif(model_23)

##25. Excluding JobSatisfaction.xvery.high;p-value =  0.001023 ** ;vif =  1.147447
 model_24 <- glm(formula = Attrition ~ NumCompaniesWorked +  TotalWorkingYears +
                   TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager +
                   AvgMonthHours + BusinessTravel.xtravel_frequently +
                   MaritalStatus.xsingle + EnvironmentSatisfaction.xlow +
                   EnvironmentSatisfaction.xvery.high + JobSatisfaction.xlow +
                   WorkLifeBalance.xbetter,
                 family = "binomial", data = employee_train)

 summary(model_24)
 vif(model_24)

final_model <- model_24

# The final model is model_24 as it has all 12 variables with significant p-values & low vif

# Variable Name                         p-value                 vif
# NumCompaniesWorked                 2.00e-06 ***              1.184189
#   TotalWorkingYears                   < 2e-16 ***            1.665465
#   TrainingTimesLastYear              0.000405 ***            1.015762
#   YearsSinceLastPromotion            3.97e-10 ***            1.659442
#   YearsWithCurrManager               8.19e-07 ***            1.759717
#   AvgMonthHours                       < 2e-16 ***            1.044717
#   BusinessTravel.xtravel_frequently  8.92e-12 ***            1.018828
#   MaritalStatus.xsingle               < 2e-16 ***            1.033662
#   EnvironmentSatisfaction.xlow       2.65e-09 ***            1.156699
#   EnvironmentSatisfaction.xvery.high 0.000816 ***            1.148716
#   JobSatisfaction.xlow               8.21e-07 ***            1.020586
#   WorkLifeBalance.xbetter            6.63e-06 ***            1.011178


#------------------------------------------------------------------------------------# 
#                                  Model Evaluation                                  #
#------------------------------------------------------------------------------------#

# Testing final model on test data, and evaluate the accuracy of our model

test_pred = predict(final_model, type = "response", 
                    newdata = employee_test[,-12])

summary(test_pred)

employee_test$prob <- test_pred

# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(employee_test$Attrition==1,"Yes","No"))

table(test_actual_attrition,test_pred_attrition)
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
# We have High accuracy (85.9%), and Specificity(98.2), But we have very low sensitivity of 25.9%
# So we will try to get the proper cutoff value

library(e1071)
library(Rcpp)
library(caret)


test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Improving on more on cutoff & Finding the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_churn <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_churn, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability
summary(test_pred)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0007807 0.0455705 0.1062693 0.1629925 0.2182784 0.8861896 

# Creating cutoff values from 0.0007807 to 0.8861896 for plotting and initiallizing a matrix of 100 X 3.
s = seq(.001,.850,length=100)

OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plotting graph to know where Spec, Sens, Acc meeet i.e. to know the cutoff value
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff # 0.1725 

# Choosing a cutoff value of 0.1725 for final model

test_pred_attrition <- factor(ifelse(test_pred >=0.1725, "Yes", "No"))
conf_final <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc  # 0.7364341 (73.6%)

sens  # 0.7409091 (74.0%)

spec  # 0.735514 (73.5%)


#------------------------------------------------------------------------------------# 
#                         KS -statistic - Test Data                                  #
#------------------------------------------------------------------------------------#

test_pred_attrition <- ifelse(test_pred_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)

#on testing  data

pred_object_test<- prediction(test_pred_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #47.64% (0.4764231)
#------------------------------------------------------------------------------------# 
#                                Lift & Gain Chart                                   #
#------------------------------------------------------------------------------------#

# Plotting the lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile

# # A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
#   1      1   129        77      77  35.00000 3.500000
# 2      2   129        37     114  51.81818 2.590909
# 3      3   129        35     149  67.72727 2.257576
# 4      4   129        21     170  77.27273 1.931818
# 5      5   129        15     185  84.09091 1.681818
# 6      6   129         6     191  86.81818 1.446970
# 7      7   129        12     203  92.27273 1.318182
# 8      8   129         6     209  95.00000 1.187500
# 9      9   129         7     216  98.18182 1.090909
# 10     10   129         4     220 100.00000 1.000000



#------------------------------------------------------------------------------------# 
#               End of model bulding & model Evaluation                              #
#------------------------------------------------------------------------------------#







