## Loading Libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(compare)
library(corrplot)

#------------------------------------------------------------------------------------# 
#                                LOADING DATA                                        #
#------------------------------------------------------------------------------------# 

## Loading Data
loan_dataset <- read.csv("loan.csv",stringsAsFactors = F)

str(loan_dataset)
summary(loan_dataset)
head(loan_dataset)

## Loading Data Dictionary for understanding the data
Data_LoanStats <- read_excel("Data_Dictionary.xlsx", sheet = "LoanStats")
str(Data_LoanStats)

Data_RejectStats <- read_excel("Data_Dictionary.xlsx", sheet = "RejectStats")
str(Data_RejectStats)

#------------------------------------------------------------------------------------# 
#         DATA CLEANING & MANIPULATION : Checking for duplicate values               #
#------------------------------------------------------------------------------------# 

## Checking for duplicate values for id & member_id columns

sum(duplicated(loan_dataset$id)) #0
sum(duplicated(loan_dataset$member_id)) #0

#------------------------------------------------------------------------------------# 
#         DATA CLEANING & MANIPULATION : Checking for NA values                      #
#------------------------------------------------------------------------------------# 

##1. Checking total number of NA values in loan_dataset

sum(is.na(loan_dataset)) # 2208180

##2. Checking number of NA values per each columns

colSums(is.na(loan_dataset))

##3. Arranging number of NA values for various columns in the loan_dataset,in Table form for easy understanding

table(colSums(is.na(loan_dataset)))

#>  0     1    39    56   697 25682 36931 39717 <--- NA Values
#> 50     1     1     2     1     1     1    54 <--- Number of columns

# There are 50 columns having 0 NA
# There is 1 column having 1 NA
# There is 1 column having 39 NA
# There are 2 columns having 56 NA
# There is 1 column having 697 NA
# There is 1 column having 25682 NA
# There is 1 column having 36931 NA
# There are 54 columns having 39717 NA (i.e. 54 columns have NA values equal to the number of rows in loan_dataset)

##4. Finding the index & name of columns which have certain number of NA values derived from above code.

which(colSums(is.na(loan_dataset)) == 1)
which(colSums(is.na(loan_dataset)) == 39)
which(colSums(is.na(loan_dataset)) == 56)
which(colSums(is.na(loan_dataset)) == 697)
which(colSums(is.na(loan_dataset)) == 25682)
which(colSums(is.na(loan_dataset)) == 36931)

# There is 1 column having 1 NA  (column 22:"title")
# There is 1 column having 39 NA  (column 107:"tax_liens")
# There are 2 columns having 56 NA (Columns 50 & 79: "collections_12_mths_ex_med" & "chargeoff_within_12_mths" )
# There is 1 column having 697 NA  (column 106:"pub_rec_bankruptcies ")
# There is 1 column having 25682 NA  (column 29:"mths_since_last_delinq")
# There is 1 column having 36931 NA  (column 30:"mths_since_last_record")

#------------------------------------------------------------------------------------# 
#         DATA CLEANING & MANIPULATION : Missing Value Imputation                    #
#------------------------------------------------------------------------------------# 

##1. Missing value imputation for column "mths_since_last_delinq" 

table(loan_dataset$mths_since_last_delinq, exclude = NULL)

# The above code shows the number of months since boworrer's last delinquency in a tabular form,
# ranging from 0 months to 120 months of delinquency for all borrowers.
# We observe "0" it has 443 entries i.e records show maximum number of people have not been delinquent.
# "0" is a clear outlier with 433 entries compared to other values greater than zero.
# Also,number of NA values in this column is 25682.

## On the basis of "0" being an outlier for all exixting values, we can IMPUTE all NA values with 0
## for the column mths_since_last_delinq

loan_dataset$mths_since_last_delinq[is.na(loan_dataset$mths_since_last_delinq)] <- 0

##2. Missing value imputation for column "mths_since_last_record"

table(loan_dataset$mths_since_last_record, exclude = NULL)

# We observe "0" it has 670 entries & number of NA values in this column is 36931
# "0" is a clear outlier with 670 entries compared to other values greater than zero.

## On the basis of "0" being an outlier for all exixting values, we can IMPUTE all NA values with 0
## for the column mths_since_last_record

loan_dataset$mths_since_last_record[is.na(loan_dataset$mths_since_last_record)] <- 0

##3. Missing value imputation for column "pub_rec_bankruptcies"

table(loan_dataset$pub_rec_bankruptcies, exclude = NULL)

# We observe the value "0" it has 37339 entries & number of NA values in this column is 697
# "0" is a clear outlier with 37339 entries compared to other values greater than zero.

## On the basis of "0" being an outlier for all exixting values, we can IMPUTE all NA values with 0
## for the column "pub_rec_bankruptcies"

loan_dataset$pub_rec_bankruptcies[is.na(loan_dataset$pub_rec_bankruptcies)] <- 0

##4. Missing value imputation for columns "collections_12_mths_ex_med" & "chargeoff_within_12_mths"

table(loan_dataset$collections_12_mths_ex_med, exclude = NULL)
table(loan_dataset$chargeoff_within_12_mths, exclude = NULL)

# We observe for both columns the value "0" it has 39661 entries & number of NA values in this column is 56
# "0" is a clear outlier with 37339 entries compared to other values greater than zero.

## On the basis of "0" being an outlier for all exixting values, we can IMPUTE all NA values with 0
## for the column "pub_rec_bankruptcies"

loan_dataset$collections_12_mths_ex_med[is.na(loan_dataset$collections_12_mths_ex_med)] <- 0
loan_dataset$chargeoff_within_12_mths[is.na(loan_dataset$chargeoff_within_12_mths)] <- 0

##5. Missing value imputation for columns "tax_liens"

table(loan_dataset$tax_liens, exclude = NULL)

# We observe for both columns the value "0" it has 39678 entries & number of NA values = 39

## On the basis of "0" being an outlier for all exixting values, we can IMPUTE all NA values with 0
## for the column "tax_liens"

loan_dataset$tax_liens[is.na(loan_dataset$tax_liens)] <- 0

##6. Missing value imputation for column "title"
table(loan_dataset$title, exclude = NULL)
which(loan_dataset$title == "")

# # The above code shows there are 9 missing (or NULL) values
# ## IMPUTING missing(or NULL) values to NA
 loan_dataset$title[which(loan_dataset$title == "")] <- NA


#------------------------------------------------------------------------------------# 
#       FILTERING DATA : Removing Columns with NA & Constant Values                   #
#------------------------------------------------------------------------------------# 

##5a. We observe there are 54 columns which have all the values as NA
## Removing the 54 columns with just "NA" Values

loan <- loan_dataset[,colSums(is.na(loan_dataset)) != nrow(loan_dataset)]

##5b.Since analysis is for identifying pattern of defaulters, rows with loan_status "current"
# will not be required

loan <- subset(loan, loan_status != "Current")

##5c. Removing columns that are not required 
# "emp_litle","desc",title","zip_code","earlist_cr_line","collection_recovery_fee","url","next_pymnt_d"

loan <- subset(loan, select = -c(emp_title,desc,title,zip_code,earliest_cr_line,
                                 collection_recovery_fee,url,next_pymnt_d))

##5d. Identifying columns with constant values/alphabets through out all rows

which(apply(loan, 2, function(col) { length(unique(col)) > 1 }) == FALSE)

##5e.Creating a dataframe after removing all the constant value columns
loan <- loan[,apply(loan, 2, function(col) { length(unique(col)) > 1 })]

#------------------------------------------------------------------------------------# 
#       DATA CLEANING & MANIPULATION : Converting date columns into Date Format      #
#------------------------------------------------------------------------------------# 

##1. Converting Column "issue_d" to date format

typeof(loan$issue_d) #character

#Given format "Dec-11" i.e. %b-%y, Hence making it to 1-Dec-11, i.e. %d-%b-%y
loan$issue_d <- paste("1-", loan$issue_d, sep="")

# Converting Column "issue_d" to date format
loan$issue_d <- as.Date(loan$issue_d, format = "%d-%b-%y")
typeof(loan$issue_d) #double

# loan$issue_d <- as.POSIXct(loan$issue_d , format = "%Y-%m-%d") 
# typeof(loan$issue_d) #double


##2. Converting Column "last_pymnt_d" to date format

table(loan$last_pymnt_d, exclude = NULL) 
sum(loan$last_pymnt_d == '') # 71 missing values
typeof(loan$last_pymnt_d) #character

#Given format "Dec-11" i.e. %b-%y, Hence making it to 1-Dec-11, i.e. %d-%b-%y
loan$last_pymnt_d <- paste("1-", loan$last_pymnt_d, sep="")

# Converting Column "last_pymnt_d" to date format
loan$last_pymnt_d <- as.Date(loan$last_pymnt_d, format = "%d-%b-%y")
typeof(loan$last_pymnt_d) #double


##3. Converting Column "last_credit_pull_d" to date format

table(loan$last_credit_pull_d, exclude = NULL) 
sum(loan$last_credit_pull_d == '') # 2 missing values
typeof(loan$last_credit_pull_d) #character

#Given format "Dec-11" i.e. %b-%y, Hence making it to 1-Dec-11, i.e. %d-%b-%y
loan$last_credit_pull_d <- paste("1-", loan$last_credit_pull_d, sep="")

# Converting Column "last_credit_pull_d" to date format
loan$last_credit_pull_d <- as.Date(loan$last_credit_pull_d, format = "%d-%b-%y")
typeof(loan$last_credit_pull_d) #double

#------------------------------------------------------------------------------------# 
#       DATA CLEANING & MANIPULATION : Converting columns to appropriate format      #
#                                      and imputing values (if required)             #
#------------------------------------------------------------------------------------# 

##1. Converting "term" column to numeric from character

class(loan$term)
table(loan$term, exclude = NULL)
loan$term <- as.numeric(gsub("months", "", loan$term))

##1a. Creating new column "term_in_years" from the "term" column
loan$term_in_yrs <- (loan$term/12) 

##1b. Converting 36 & 60  months in term column to "Short Term" & "Long Term"
loan <- mutate(loan,term = ifelse(term == 36,"Short Term", "Long Term"))

##1c. Rearranging the columns

loan <- loan[c(1:6,39,7:38)]

##2. Converting "int_rate" column in to numeric

table(loan$int_rate, exclude = NULL) # 0 missing or NA values
sum(is.na(loan$int_rate)) # 0
loan$int_rate <- as.numeric(gsub("%$", "", loan$int_rate))

##3. Converting "revol_util" column in loan from character to numeric

table(loan$revol_util,exclude = NULL) # 50 missing values
which(loan$revol_util == "")
length(which(loan$revol_util == "")) # 50
loan$revol_util <- as.numeric(gsub("%$","",loan$revol_util))
sum(is.na(loan$revol_util)) # 50

##3a. Imputing NA values in column"revol_util" with the median value of the non-NA values

loan$revol_util[is.na(loan$revol_util)] <- median(loan$revol_util,na.rm = T)

##4. Converting column"emp_length" into correct format

## Finding if there are rows in the column"emp_length"that start with an alphabet

str_extract(loan$emp_length, "^[A-z]")

## Total number of rows that in the column"emp_length"that start with an alphabet

sum(!is.na(str_extract(loan$emp_length, "^[A-z]"))) # 1033

##4a. Imputing ,rows in the column "emp_length"that start with an alphabet, with NA

loan$emp_length[(!is.na(str_extract(loan$emp_length, "^[A-z]")))] <- NA

sum(is.na(loan$emp_length)) # 1033

##4b. Converting column"emp_length" into appropriate numeric format

loan$emp_length <- gsub("[+years| year]","", loan$emp_length) 

loan$emp_length <- gsub("<1","0", loan$emp_length) # replacing "<1" with "0"

loan$emp_length <- as.numeric(loan$emp_length) # converting into numeric format

##5. Converting columns into factors

loan[,c("loan_status","term","grade","sub_grade",
        "home_ownership","verification_status","purpose","addr_state")] <-
  lapply(loan[,c("loan_status","term","grade","sub_grade",
                 "home_ownership","verification_status","purpose","addr_state")],
         as.factor)

sapply(loan, class)

#------------------------------------------------------------------------------------# 
#           DATA ANALYSIS : Univariate Analysis & Bivariate Analysis                 #
#------------------------------------------------------------------------------------# 

##1. Subsetting loan data based on loan status

loan_charged_off <- subset(loan, loan_status == "Charged Off")
loan_fully_paid <- subset(loan, loan_status == "Fully Paid")
loan_data_exclude_current <- subset(loan, loan_status != 'Current')

##2. Number of loans that are (a) charged off, (b) fully paid & (c) current (running)

nrow(loan_charged_off)#5627
nrow(loan_fully_paid)#32950


##2a.Plot showing the count of loan status that are charged off & fully paid

# OBSERVATION:
# In the given data set, the count of charged off loans are 
# around one-fifth the count of fully paid loans.

plot_count_of_loan_status <- ggplot(loan, aes(x = loan_status, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
  labs(title = "Count of Charged Off & Fully Paid Loan Status",
       x = "Loan Status", y = "Count of incidences")
ggsave(plot_count_of_loan_status,filename=paste("plot2a_count of loan status",".png",sep=""))
print(plot_count_of_loan_status)

##3. Plot showing the number of loans that are taken on 
#short term(36 months) & long term(60 months) w.r.t.loan status

# OBSERVATION: 
# It can be seen that the number of charge offs are more in short term loans(i.e. 36 months)
# when compared with long term loans.
# The count of short term loans is little more than three times the count of long term loans
 
plot_countandtype_of_loans <- ggplot(loan, aes(x = term, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
  labs(title = "Count & term of loans w.r.t. Loan Status",
       x = "Term of Loan", y = "Count of incidences")
ggsave(plot_countandtype_of_loans,filename=paste("plot3_count & term of loans w.r.t. loan status",".png",sep=""))
print(plot_countandtype_of_loans)

##4. Plot showing the number of loans that are verified, source verified & not verified
# w.r.t.loan status

# OBSERVATION: 
# It is seen that there is a dip in the number charge off 
# when the loan applicant's income is source verified.

# plot 4a.
plot_verification_status_with_loan_status <- ggplot(loan, aes(x = verification_status, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
  labs(title = "Count of verification status w.r.t. Loan Status",
       x = "Verification Status", y = "Count of incidences")
ggsave(plot_verification_status_with_loan_status,filename=paste("plot4a_verification_status_w.r.t. loan status",".png",sep=""))
print(plot_verification_status_with_loan_status)

# plot 4b.

plot_verification_status_with_loan_status <- ggplot(data = summarise(group_by(loan,verification_status,loan_status),count = length(loan_status)), 
       aes(x = verification_status, y = count, color = loan_status, group = 1)) +
  geom_point() + geom_smooth() + facet_grid(loan_status~.) + 
  labs(title = "Plot showing count of verification status w.r.t. Loan Status",
       x = "Verification Status", y = "Count of incidences")
ggsave(plot_verification_status_with_loan_status,filename=paste("plot4b_verification_status_w.r.t. loan status",".png",sep=""))
print(plot_verification_status_with_loan_status)

##5. Plot showing the number of loans taken for various purposes w.r.t.loan status

# OBSERVATION: 
#a.Charged-off  numbers are higher for the purpose - Debt consolidation, 
# followed by Credit card,home_improvement, Other, small business etc.
#b.Number of Loans taken for debt consolidation purpose is very high
# compared with Loans taken for all other purpose
#c. Number of charged off loans taken for debt consolidation purpose 
# is very high when compared with other purposes

plot_various_purposes_with_loan_status <- ggplot(loan, aes(x = purpose, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), geom="text", vjust= -0.2) + 
  facet_grid(loan_status~.) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of various purposes w.r.t Loan Status",
       x = "Purpose", y = "Count of incidences")
ggsave(plot_various_purposes_with_loan_status,filename=paste("plot5_various_purposes_with_loan_status",".png",sep=""))
print(plot_various_purposes_with_loan_status)

##6. Plot showing the number of loans taken from various address states w.r.t.loan status

# OBSERVATION: 
#a. Top 5 States in terms of higher order of charged-off loans are CA(California),
#  NY(New,York),TX(Texas),FL(Florida),NJ(New Jersey).
#b.Number of Loans taken from the state of California (CA)  very high when
# compared loans taken from all other address states
#c. Number of charged off loans taken  from the state of California (CA)  is very high
#  when compared loans taken from all other address states

plot_address_states_with_loan_status <- ggplot(loan, aes(x = addr_state, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), 
             geom="text", vjust= 0.5, angle = 90, hjust = 0.3, size = 2.4) + 
  facet_grid(loan_status~.) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Loans Count for address states w.r.t. Loan status",
       x = "Address State", y = "Count of incidences")
ggsave(plot_address_states_with_loan_status,filename=paste("plot6_address_states_with_loan_status",".png",sep=""))
print(plot_address_states_with_loan_status)

##7. Plot showing the number of loans with various grades w.r.t.loan status

# OBSERVATION: 
#a. Total number of Loans under Grade "B" is high when compared with loans under other grades.
#b. The count of charged off loans is maximum under Grade B, followed by Grade C & Grade D respectively.
#c. The count of Fully Paid loans is maximum under Grade B, followed by Grade A & Grade C respectively.

plot_various_grades_with_loan_status <- ggplot(loan, aes(x = grade, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), 
             geom="text", vjust= -0.1, hjust = 0.3, size = 4) + 
  facet_grid(loan_status~.) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Count of loans with various grades w.r.t.loan status",
       x = "Grades", y = "Count of incidences")
ggsave(plot_various_grades_with_loan_status,filename=paste("plot7_various_grades_with_loan_status",".png",sep=""))
print(plot_various_grades_with_loan_status)

##8. Plot showing range employment length w.r.t. the loan status

# OBSERVATION: 
#a. Total number of Loans for applicants with employment length of 10 years or more is high.
#b. The count of charged off loans for applicants with employment length of 10 years or more
#c. The count of Fully Paid loans for applicants with employment length of 10 years or more
#d. There are around 1200 applicants whose employment length is not available

plot_exp_status <- ggplot(loan, aes(x = as.factor(emp_length), fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), 
             geom="text", vjust= -0.1, hjust = 0.3, size = 4) + 
  facet_grid(loan_status~.) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title = "Range of emp length w.r.t.loan status",
       x = "Employment Length", y = "Count of incidences")
ggsave(plot_exp_status,filename=paste("plot8_ employment length w.r.t. the loan status",".png",sep=""))
print(plot_exp_status)

##9. Plot showing count of all types of home ownership w.r.t. the loan status

# OBSERVATION: 
#a. Total number of Loans for applicants with home ownership as Rent is high followed by Mortgage.
#b. The count of charged off loans for applicants with home ownership as Rent is marginally  high than Mortgage.
#c. The count of Fully Paid loans for applicants with home ownership as Rent is high

plot9_home_ownership_with_loan_status <- ggplot(loan, aes(x = home_ownership, fill = loan_status)) + geom_histogram(stat = "count")+
  stat_count(aes(y=..count.., label=..count..), 
             geom="text", vjust= -0.1, hjust = 0.3, size = 4) + 
  facet_grid(loan_status~.) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  labs(title = "Count of types of home ownership w.r.t. loan status",
       x = "Type of Home Ownership", y = "Count of incidences")
ggsave(plot9_home_ownership_with_loan_status,filename=paste("plot9_home_ownership_with_loan_status",".png",sep=""))
print(plot9_home_ownership_with_loan_status)

##10.

## a.Plot showing distribution of interest rates w.r.t. the loan status

# OBSERVATION: 
#a. The rate of interest for charged off loans is higher compared to Fully Paid loans

plot_int_status <- ggplot(data=loan, aes(y=int_rate, x= loan_status , colour = loan_status))+
  geom_point(alpha =0.6 ) + geom_boxplot(alpha = 0.6) +
  labs(title = "Distribution of interest rate w.r.t.loan status",
       x = "Loan Status", y = "Rate of Interest")
ggsave(plot_int_status,filename=paste("plot10a_interest_rate_spread_loan_status",".png",sep=""))
print(plot_int_status)

## b. Plot showing the distribution of funded amount w.r.t. loan status

# OBSERVATION: 
#a. The funded amount for charged off loans is slightly higher compared to Fully Paid loans

plot_fund_status <- ggplot(data=loan, aes(x= loan_status,y=funded_amnt, colour = loan_status))+
  geom_point(alpha =0.6 ) + geom_boxplot(alpha = 0.6)+
  labs(title = " Distribution of funded amount w.r.t.loan status",
       x = "Loan Status", y = "Funded Amount")
ggsave(plot_fund_status,filename=paste("plot10b_ifunded_amnt_spread_loan_status",".png",sep=""))
print(plot_fund_status)


##11. Plot dti spread w.r.t. loan status

# OBSERVATION: 
#a.  For charged off loans the median DTI is comparatively higher than ‘Fully paid’

plot_dti_status <- ggplot(loan_data_exclude_current, aes(loan_status, dti, fill = loan_status)) + geom_boxplot() + scale_x_discrete(breaks=NULL) +
 labs(title = "Distribution of dti w.r.t. loan status", x = "Loan status", y = "Debt to Income Ratio")
ggsave(plot_dti_status,filename=paste("plot11_dti_spread_loan_status",".png",sep=""))
print(plot_dti_status)


#This functions is required for removing outliers for box plot
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

##12. Plot Revolving Utilization w.r.t. loan status

# OBSERVATION: 
#a. Charged off has the highest utilization than any other status.Higher the utilization riskier the applicant.
plot_revol_util_status <- ggplot(loan, aes(loan_status, revol_util, fill = loan_status)) + geom_boxplot() + scale_x_discrete(breaks=NULL) +
      labs(title = "Distribution of revolving utilization %  w.r.t. loan status", x = "Loan status", y = "revolving utilization %")
ggsave(plot_revol_util_status,filename=paste("plot12_revol_util_spread_loan_status",".png",sep=""))
print(plot_revol_util_status)


##13. Plot Funded amount vs term for different status

# OBSERVATION: 
#a. # For 36 Months Term the median funded amount is  generally lower than for 60 Months Term. 
# There are some outliers for 36 months term

plot_funded_term <- ggplot(loan_data_exclude_current, aes(term, funded_amnt, fill = term)) + geom_boxplot() + scale_x_discrete(breaks=NULL)
plot_funded_term <- plot_funded_term + labs(title = "Funded amount by term", x = "Term", y = "Funded amount")
ggsave(plot_funded_term,filename=paste("plot13_funded_amount_by_term",".png",sep=""))
print(plot_funded_term)


## Before starting bivariate analysis, prepare correlation matrix for identified quantitative variables from 
## univariate analysis
cor_var_name <- c('int_rate','revol_util','revol_util','funded_amnt','installment','annual_inc','dti','delinq_2yrs',
                  'inq_last_6mths','open_acc','pub_rec','revol_bal','total_acc')
correlation_loan_data <- select(loan_charged_off,one_of(cor_var_name))
correlation_loan_data <- correlation_loan_data[complete.cases(correlation_loan_data), ]  # reomve incomplete cases
cor_matrix <- cor(correlation_loan_data)  # transfer to matrix 
plot_cor_matrix  <- corrplot(cor_matrix, method = "number", title = "Correlation Map of Subgrade & Factors", 
                             type = "lower", order = "FPC", number.cex = 0.5, tl.cex = 0.8)
print(plot_cor_matrix)


##14. Plot Loan amount Distribution based vs Grades across years

# OBSERVATION: 
#a. Loan amount is higher for higher grades i.e for Grades E, F, G.

plot_loan_amt_grades <- ggplot(loan, aes(x = loan$issue_d, y = loan$loan_amnt)) +
  geom_smooth(aes(colour = loan$grade), se = FALSE) +
  labs(title = "Loan amount by grades across years", x = "Years", y = "Loan amount") +
  theme_minimal()
ggsave(plot_loan_amt_grades,filename=paste("plot14_loan_amt_grade",".png",sep=""))
print(plot_loan_amt_grades)

##15. Plot Funded amount vs purpose

# OBSERVATION: 
#a. Proportion of charged-offs are higher for small_business,followed by  educational, renewable energy,house,medical etc.

plot_funded_purpose <- ggplot(loan_data_exclude_current, aes(purpose, funded_amnt, fill = loan_status)) + geom_bar(position = "fill", stat = "identity") + theme(axis.text.x=element_text(size=8, angle = 90))
plot_funded_purpose <- plot_funded_purpose + labs(title = "Funded amount vs Purpose", x = "Purpose", y = "Funded amount")
ggsave(plot_funded_purpose,filename=paste("plot15_funded_purpose",".png",sep=""))
print(plot_funded_purpose)

##16. Plot Interest rate vs grade across years

# OBSERVATION: 
#a. Interest rate is higher for higher grades i.e. for E, F, G..

plot_interest_grade <- ggplot(loan, aes(x = loan$issue_d, y = loan$int_rate)) +
  geom_smooth(aes(colour = loan$grade)) +
  labs(title = "Interest rate vs grade across years", x = "Years", y = "Interest rate") +
  theme_minimal()
ggsave(plot_interest_grade,filename=paste("plot16_interest_grade",".png",sep=""))
print(plot_interest_grade)


##17. Plot Interest rate vs loan amount and term

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. Most of the long term loans(60 Months) are falling  in higher interest rate even for lower funded amount.This may be 
#   leading to higher instalments and may be contributing to charge-offs.
#c. A significant proportion of  short term loans(36 Months loans)  are under charge off category for lower funded amounts and 
#   medium  & lower interest range (up to 15%) – this may be due to improper income verification of the customers. 

plot_int_amt_term <- ggplot(loan_charged_off, aes(x = funded_amnt, y = int_rate, fill = term)) + geom_jitter(aes(color = term)) + 
    labs(title = "Interest rate vs funded amount and term", x = "funded amount", y = "Interest rate") +
    geom_smooth(aes(colour = term))
ggsave(plot_int_amt_term, filename=paste("plot17_int_amt_term",".png",sep=""))
print(plot_int_amt_term)

##18. Plot funded_amt vs ins_for_grade

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. For the purpose –’Debt consolidation, Credit Card and Small business – both Funded Amount and Instalment are  higher 
#   leading to higher charged-offs

plot_funded_amt_vs_ins_for_grade <- ggplot(loan_charged_off, aes(x = funded_amnt, y = installment))
plot_funded_amt_vs_ins_for_grade <- plot_funded_amt_vs_ins_for_grade + geom_point(color = "cadetblue4", size = 0.5) + 
  geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE) + 
  labs(title = "Funded amount vs installament by purpose", x = "Funded amount", y = "Installment") + 
  theme_bw() + facet_wrap(~purpose, ncol = 2)
ggsave(plot_funded_amt_vs_ins_for_grade, filename=paste("plot18_funded amount_vs_installament_across_purpose",".png",sep=""))
print(plot_funded_amt_vs_ins_for_grade)

##19. Plot funded_amt vs ins_for_grade

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. Most of the long term loans(60 Months) are falling  in higher interest rate for lower Annual income. This may be 
#   leading to higher instalments and may be contributing to charge-offs.
#c. A significant proportion of  short term loans(36 Months) are under charge off category for lower annual income 
#   and medium & lower interest range. This may be due to improper income verification of the customers.

plot_ann_inc_int_rate <- ggplot(loan_charged_off, aes(x = int_rate, y = annual_inc, fill = term)) + geom_jitter(aes(color = term, alpha =0.2)) + 
  labs(title = "Annual Income Vs Interest rate w.r.t.Term", x = "Interest Rate", y = "Annual Income")
ggsave(plot_ann_inc_int_rate, filename=paste("plot19_ann_inc_int_rate",".png",sep=""))
print(plot_ann_inc_int_rate)

##20. Plot annual income vs revol_util for top 5 states

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. Revoling Utilization analysis for top 5 states in terms of number of charge offs shows that it is higher for higher 
#   Annual income customers - typically for state Florida.However,still charge offs are concentrated towards lower income 
#   and Revoling Utilization. 
#c. Most of the charged off cases are concentrated in lower annual_inc & lower revol_util baring a few 
#   cases where a fewer number of  higher or medium  annual_inc customers are also contributing to charged-offs.

charged_off_top_5states <- filter(loan_charged_off, addr_state %in% c("CA","NY","TX","FL","NJ"))
plot_revol_util <- ggplot(charged_off_top_5states, aes(x = revol_util, y = annual_inc))
plot_revol_util <- plot_revol_util + geom_point(size = 0.5) 
plot_revol_util <- plot_revol_util + geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE) 
plot_revol_util <- plot_revol_util + labs(title = "Annual Income vs Revoling Utilization %", x = "Revolving Utilization %", y = "Annual Income") 
plot_revol_util <- plot_revol_util + theme_bw() + facet_wrap(~addr_state, ncol = 3) +
  theme(strip.background = element_rect(fill="light yellow"))
ggsave(plot_revol_util, filename=paste("plot20_revol_util",".png",sep=""))
print(plot_revol_util)

##21. Plot interest rate vs Annual Income w.r.t. verification status

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. For Higher annual income interest rates lie in the medium range but proportion of ‘Not verified’ is also higher

median_ann_inc_vs_ver_status$bin_int_rate <- cut(x=median_ann_inc_vs_ver_status$int_rate,
                                                 breaks=seq(from=0, to=ceiling(max(median_ann_inc_vs_ver_status$int_rate)), by = 1))
plot_bar_ann_inc_vs_ver_status <- ggplot(median_ann_inc_vs_ver_status, aes(x = bin_int_rate,
                                                                           y = median_annual_amount, fill = verification_status)) +
  geom_bar(stat="identity") + xlab("int_rate") + 
  labs(title = "Interest rate versus Median Annual Income w.r.t. verification status", x = "Interest Rate", y = "Annual Income")
ggsave(plot_bar_ann_inc_vs_ver_status, filename=paste("plot21_int_rate_vs_Ann_Inc_by_verif_stat",".png",sep=""))
print(plot_bar_ann_inc_vs_ver_status)

##22. Funded amount Vs Debt to Income Ratio w.r.t Purpose

# OBSERVATION: 
#a. Plot is created only on filtered data as per loan status = Charged off
#b. Customers with purpose debt consolidation are higher in number even for lower DTI and funded amount 
#c. Customers with  purpose – ‘Credit card’ or ‘Small_Bussiness’ & higher DTI – even with lower or medium Funded amount  
#   are having higher proportion of charge-offs. Probably due to higher debt burden on them than their paying capacity.

pur_vs_dti_int <- ggplot(loan_charged_off, aes(x = funded_amnt, y = dti))
pur_vs_dti_int <- pur_vs_dti_int + geom_point(size = 0.5, alpha =0.4 ) 
pur_vs_dti_int <- pur_vs_dti_int + geom_smooth(color = "red", linetype = "dashed", size = 1, se = FALSE) 
pur_vs_dti_int <- pur_vs_dti_int + labs(title = "Funded amount Vs DTI w.r.t. Purpose", x = "Funded amount", y = "Debt to Income Ratio") 
pur_vs_dti_int <- pur_vs_dti_int + theme_bw() + facet_wrap(~purpose, ncol = 3)
ggsave(pur_vs_dti_int, filename=paste("plot22_fundamt_vs_dti_wrt_pur",".png",sep=""))
print(pur_vs_dti_int)

