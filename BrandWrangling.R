# Using R, clean this data set to make it easier to visualize and analyze. Specifically, these are the tasks you need to do:
#   
#   0: Load the data in RStudio
# Save the data set as a CSV file called refine_original.csv and load it in RStudio into a data frame.
# 1: Clean up brand names
# Clean up the 'company' column, so all of the misspellings of the brand names are standardized. For example, you can transform the values in the column to be: philips, akzo, van houten and unilever (all lowercase).
# 2: Separate product code and number
# Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number, containing the product code and number respectively
# 3: Add product categories
# You learn that the product codes actually represent the following product categories:
#   p = Smartphone
# v = TV
# x = Laptop
# q = Tablet
# In order to make the data more readable, add a column with the product category for each record.
# 4: Add full address for geocoding
# You'd like to view the customer information on a map. In order to do that, the addresses need to be in a form that can be easily geocoded. Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas.
# 
# 5: Create dummy variables for company and product category
# Both the company name and product category are categorical variables i.e. they take only a fixed set of values. In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
# Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
# Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet
# 6: Submit the project on Github
# Include your code, the original data as a CSV file refine_original.csv, and the cleaned up data as a CSV file called refine_clean.csv.


library(dplyr)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)
library(readxl)

# For fun. Let's do some eda 
# What are the files in the dir 
dir()

# list the sheets in the xl file 
excel_sheets("refine.xlsx")
#There is only one sheet Sheet1 - Great !


brand <- read_csv("refine_original.csv")

# Lets do some exploration
glimpse(brand)
str(brand)
dim(brand)

# standardize on the names

#I began by changing the names to lower case
brand$company <- tolower(brand$company)

factor(brand$company)
 
# Use an explicit for loop when each iteration is a non-trivial task. But a simple
# loop can be more clearly and compactly expressed using an apply function.
# There is at least one exception to this rule. We will see in Circle 8.1.56 that
# if the result will be a list and some of the components can be NULL, then a for loop may skip 
# give a wrong output# her we decided to use lapply

fn_clean_company_names <- function(x) { 
  
 if( grepl(regex("^[fp]+[hilp]*s$"), x         ))  { x <- "philips"}
 else if( grepl(regex("^a[zk ]*[o0]+$" ),          x))  { x <- "akzo"}
 else if( grepl(regex("^van\\s+[phils\\houten]+"), x))  { x <- "van houten"} 
 else if( grepl(regex("^u.+er$"),                  x))  { x <- "unilever"}
 return(x)
 
}

brand$company <- sapply(brand$company,fn_clean_company_names)

#Anirban wrote 

# #My suggestion is, i) for-loop and ii) conditional loop is enough. Function is not required as such. You can try the below code:
#   
#   for (i in 1:nrow(brand))
#   {
#     if(grepl("^[P|p|p|f]+", brand$company[i]) == TRUE) {brand$company[i] <- "philips"}
#     else if(grepl("^[A|a]+", brand$company[i]) == TRUE) {brand$company[i] <- "akzo"}
#     else if (grepl("^[Va|va]+", brand$company[i]) == TRUE) {brand$company[i] <- "van houten"}
#     else {brand$company[i] <- "unilever"}
#   }

str(brand)
 
 # 2: Separate product code and number
 # Separate the product code and product number into separate columns i.e. add two new columns called product_code and product_number, containing the product code and number respectively
 
 
 brand<-separate(brand, "Product code / number" ,c( "product_code", "product_number"))
 
 # 3: Add product categories
 # You learn that the product codes actually represent the following product categories:
 #   p = Smartphone
 # v = TV
 # x = Laptop
 # q = Tablet
 # In order to make the data more readable, add a column with the product category for each record.
 
brand <- mutate(brand, product_category = ifelse(product_code == "p","Smartphone", 
                                     (ifelse(product_code == "x", "Laptop",
                                    (ifelse(product_code == "v", "TV","Tablet"))))))
 # 4: Add full address for geocoding
 # You'd like to view the customer information on a map. In order to do that, the addresses need to be in a form that can be easily geocoded. Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas.
 # 
 
 brand <- unite(brand, full_address, address, city, country, sep = "," )
 
 
 # 5: Create dummy variables for company and product category
 # Both the company name and product category are categorical variables i.e. they take only a fixed set of values. In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
 # Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
 
 brand <- mutate(brand, company_philips    = ifelse(company == "philips",1,0)) 
 brand <- mutate(brand, company_akzo       = ifelse(company == "akzo",1,0)) 
 brand <- mutate(brand, company_van_houten = ifelse(company == "van houten",1,0)) 
 brand <- mutate(brand, company_unilever   = ifelse(company == "unilever",1,0)) 
 
 # Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet
 brand <- mutate(brand, product_smartphone    = ifelse(product_code == "p",1,0)) 
 brand <- mutate(brand, product_tv            = ifelse(product_code == "v",1,0)) 
 brand <- mutate(brand, product_laptop        = ifelse(product_code == "x",1,0)) 
 brand <- mutate(brand, product_tablet        = ifelse(product_code == "q",1,0)) 
 
 # Write out the cleaned data fraome into a csv file
 write_csv(brand,"refined_clean.csv", append = FALSE, col_names=TRUE)