# Loading all libraries

library(tidyr)
library(dplyr)
library(stringr)

#-----------------------------------------------------------------------------------------#
#                             Checkpoint 1: Data Cleaning 1                               #
#-----------------------------------------------------------------------------------------#

#1. Load companies & rounds2 data

 companies <- read.delim("companies.txt", header = TRUE, 
                        as.is = TRUE,na.strings = "", stringsAsFactors = FALSE)

 rounds2 <- read.csv("rounds2.csv", header = TRUE,stringsAsFactors = FALSE,na.strings = "")

#1.a. converting 1st column variables of rounds2 to lower case
 rounds2[,1] <- tolower(rounds2[,1]) 

#1.b. converting 1st column variables of companies to lower case
 companies[,1] <- tolower(companies[,1])

#2. Expected Results

#2.1 how many unique companies are present in rounds2?#66368
 n_distinct(rounds2$company_permalink,na.rm = TRUE)

#2.2 How many unique comanies are present in companies?#66368
 n_distinct(companies$permalink,na.rm = TRUE)

#2.3 In companies data frame, which column that can be used as a unique key for each company
 # Answer-permalink 

#2.4 Are there companies in round2 which are not present in companies? # Answer- NO 
 
 # verifying if all values in permalink vector of rounds2 are present in companies
 permalinks_match <- subset(companies,companies$permalink %in% rounds2$company_permalink)
 # Result-All the values are matching. Companies in round2 are present in companies. 

#2.5.Merging rounds2 & companies dataframes
 master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
 # number of observations present in master_frame are 114949

#-----------------------------------------------------------------------------------------#
#                             Checkpoint 2: Funding Type Analysis                         #
#-----------------------------------------------------------------------------------------#

# CHECKPOINT 2 : Funding Analysis
 
#1. Calculating Average investment for each of the four funding round types
 avg_funding_round_type <- aggregate(raised_amount_usd ~ funding_round_type, master_frame, mean)
 
#2. Investment type suitable for spark funds
 Decision_invest_type <- subset(avg_funding_round_type,
                                avg_funding_round_type$raised_amount_usd >= 5000000
                                & avg_funding_round_type$raised_amount_usd <= 15000000)
 
# Conclusion: Venture is the most preferred investment type
 
 
#-----------------------------------------------------------------------------------------#
#                             Checkpoint 3: Country Analysis                              #
#-----------------------------------------------------------------------------------------#
 

# Filtering venture funds from master_frame
 FT_venture <- subset(master_frame, master_frame$funding_round_type == "venture")

# Summing up country-wise-total funds recived
 country_total <- aggregate(raised_amount_usd ~ country_code, FT_venture, sum)
 
# top 9 countries
 top9 <- head(country_total[order(country_total$raised_amount_usd,decreasing = T), ],9)
 

#-----------------------------------------------------------------------------------------#
#                             Checkpoint 4: Sector Analysis 1                             #
#-----------------------------------------------------------------------------------------#
 

 #4.1 Extracting primary sector

 mf_primary_sector <- separate(master_frame,category_list,into = c("primary_sector"), 
                         sep = "\\|", remove = TRUE)
 
#4.2 Loading mapping file
 mapping_file <- read.csv("mapping.csv")
 
# Data Cleaning on Mapping File

#a. Removing & replacing "0" with "na" (as that is the pattern of mis-spelled words) 
 mapp_clean <- data.frame(mapping_file,
                  corrected_category = str_replace(mapping_file$category_list,"0","na"))
 
#b. Correcting personal finance which was left out in 4.2.a
 mapp_clean <- data.frame(mapping_file, corrected_category = 
                    str_replace(mapp_clean$corrected_category,"Personal Fi0nce","Personal Finance"))
 
#c. Correcting Entriprise 2.0 which changed to Enterprise 2.na during the first step 
 mapp_clean <- data.frame(mapping_file, 
                  corrected_category = str_replace
                  (mapp_clean$corrected_category,"Enterprise 2.na","Enterprise 2.0"))

#d. Removing the column category_list
 map_file_clean <- within(mapp_clean,rm(category_list))
 
#e. Renaming corrected_category to category_list
 colnames(map_file_clean)[10] <- "category_list"
 
#f. Converting mapping data in map_file_clean from wide format to long format
 map_file_long <- gather(map_file_clean,key = "main_sector", value = "mapped_value", 
                  (Automotive...Sports:Social..Finance..Analytics..Advertising))
 
#g. Removing rows where mapped_value = 0
 map_file_long <- filter(map_file_long,mapped_value == 1)
 
#h. Removing the blank category from the category list
 map_file_long <- filter(map_file_long,main_sector != "Blanks")
 
#i. Mergeging the map_file_long & mf_primary_sector dataframes
 merged_df <- merge(mf_primary_sector, map_file_long, 
                      by.x = "primary_sector", by.y = "category_list")

#j. Drop mapped value column & arranging the columns
 merged_data_frame <- subset(merged_df,select = c(2:16,1))
 
#-----------------------------------------------------------------------------------------#
#                             Checkpoint 5: Sector Analysis 2                             #
#-----------------------------------------------------------------------------------------#
   

#Dataframe D1_usa with the set conditions
D1_usa <- filter(merged_data_frame, country_code == "USA", 
             funding_round_type == "venture", 
             raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 
                
#For D1_usa,the Total number (or count) of investments for each main sector
D1_count_invst <- setNames(aggregate(raised_amount_usd ~ main_sector, D1_usa, length),
                           c("main_sector","total_number_investments"))
 
#For D1_usa,the Total amount invested in each main sector
D1_total_amount <- setNames(aggregate(raised_amount_usd  ~ main_sector, D1_usa,sum),
                            c("main_sector","total_amount_invested"))

# Creating Dataframe D1 for United States with all other conditions met
#(Merging D1_total_amount,D1_count_invst & D1_usa)
D1_merged <- merge (D1_total_amount,D1_count_invst, by = "main_sector")
D1 <- merge(D1_merged,D1_usa, by = "main_sector")
  
   
#Dataframe D2_gbr with the set conditions
D2_gbr <- filter(merged_data_frame, country_code == "GBR", 
             funding_round_type == "venture", 
             raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
   
#For D2_gbr,the Total number (or count) of investments for each main sector
D2_count_invst <- setNames(aggregate(raised_amount_usd ~ main_sector, D2_gbr, length),
                           c("main_sector","total_number_investments"))
   
#For D2_gbr,the Total amount invested in each main sector
D2_total_amount <- setNames(aggregate(raised_amount_usd  ~ main_sector, D2_gbr, sum),
                            c("main_sector","total_amount_invested"))

#Creating Dataframe D2 for United Kingdom with all other conditions met
# (Merging D2_total_amount,D2_count_invst & D2_gbr)
D2_merged <- merge(D2_total_amount,D2_count_invst, by = "main_sector")
D2 <- merge(D2_merged,D2_gbr, by = "main_sector")  
   

#Dataframe D3_ind with the set conditions
D3_ind <- filter(merged_data_frame, country_code == "IND", 
             funding_round_type == "venture", 
             raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)

#For D3_ind,the Total number (or count) of investments for each main sector
D3_count_invst <- setNames(aggregate(raised_amount_usd ~ main_sector, D3_ind, length),
                           c("main_sector","total_number_investments"))

#For D3_ind,the Total amount invested in each main sector
D3_total_amount <- setNames(aggregate(raised_amount_usd  ~ main_sector, D3_ind, sum),
                            c("main_sector","total_amount_invested"))

#Creating Dataframe D3 for India with all other conditions
#(Merging D3_total_amount,D3_count_invst & D3)
D3_merged <- merge(D3_total_amount,D3_count_invst, by = "main_sector")
D3 <- merge(D3_merged,D3_ind, by = "main_sector")

   
#-----------------Checkpoint 5: RESULTS EXPECTED FOR COUNTRY 1--------------------------#

#1. Total number of investments(count)
sum(aggregate(total_number_investments ~ main_sector, D1, length)$total_number_investments)

#2. Total amount of investments(USD)
sum(aggregate(raised_amount_usd ~ main_sector, D1,sum)$raised_amount_usd)
                            
#3. Top sector (based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[1,1]

#4. Second-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[2,1]
                          
#5. Third-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[3,1]

#6. Number of investments in the top sector(refer point3)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[1,2]

#7. Number of investments in the second-best sector(refer point4)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[2,2]

#8. Number of investments in the third-best sector(refer point5)
arrange(aggregate(raised_amount_usd ~ main_sector, D1, length),desc(raised_amount_usd))[3,2]

#9. For the top sector count wise(point3) which company has received the highest investment?
head(arrange(subset(D1, D1$main_sector == "Others"),desc(raised_amount_usd)))[1,10]

#10 For the second-best sector count wise(point4) which company has received the highest investment?
head(arrange(subset(D1, D1$main_sector == "Social..Finance..Analytics..Advertising"),
             desc(raised_amount_usd)))[1,10]


#-----------------Checkpoint 5: RESULTS EXPECTED FOR COUNTRY 2--------------------------#

#1. Total number of investments(count)
sum(aggregate(total_number_investments ~ main_sector, D2, length)$total_number_investments)

#2. Total amount of investments(USD)
sum(aggregate(raised_amount_usd ~ main_sector, D2,sum)$raised_amount_usd)

#3. Top sector (based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[1,1]

#4. Second-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[2,1]

#5. Third-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[3,1]

#6. Number of investments in the top sector(refer point3)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[1,2]

#7. Number of investments in the second-best sector(refer point4)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[2,2]

#8. Number of investments in the third-best sector(refer point5)
arrange(aggregate(raised_amount_usd ~ main_sector, D2, length),desc(raised_amount_usd))[3,2]

#9. For the top sector count wise(point3) which company has received the highest investment?
head(arrange(subset(D2, D2$main_sector == "Others"),desc(raised_amount_usd)))[1,10]

#10 For the second-best sector count wise(point4) which company has received the highest investment?
head(arrange(subset(D2, D2$main_sector == "Social..Finance..Analytics..Advertising"),
             desc(raised_amount_usd)))[1,10]


#-----------------Checkpoint 5: RESULTS EXPECTED FOR COUNTRY 3--------------------------#

#1. Total number of investments(count)
sum(aggregate(total_number_investments ~ main_sector, D3, length)$total_number_investments)

#2. Total amount of investments(USD)
sum(aggregate(raised_amount_usd ~ main_sector, D3,sum)$raised_amount_usd)

#3. Top sector (based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[1,1]

#4. Second-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[2,1]

#5. Third-best sector(based on count of investments)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[3,1]

#6. Number of investments in the top sector(refer point3)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[1,2]

#7. Number of investments in the second-best sector(refer point4)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[2,2]

#8. Number of investments in the third-best sector(refer point5)
arrange(aggregate(raised_amount_usd ~ main_sector, D3, length),desc(raised_amount_usd))[3,2]

#9. For the top sector count wise(point3) which company has received the highest investment?
head(arrange(subset(D3, D3$main_sector == "Others"),desc(raised_amount_usd)))[1,10]

#10 For the second-best sector count wise(point4) which company has received the highest investment?
head(arrange(subset(D3, D3$main_sector == "Social..Finance..Analytics..Advertising"),
             desc(raised_amount_usd)))[1,10]



#------------------------------------------END-----------------------------------------------#
