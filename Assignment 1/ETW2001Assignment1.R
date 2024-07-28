#Load necessary library
library(tidyr)
library(dplyr)
library(reshape2)

#Set working directory and read the datasets
setwd("C:/Users/yibin/Downloads")
sales_data <- read.csv("sales.csv")
products_data <- read.csv("products.csv")
inventory_data <- read.csv("inventory.csv")

#Get the variable names
View(sales_data)
View(inventory_data)
View(products_data)

#Q1
#The Date attribute in the dataset is not clean, i separate it to 
#year, month, and day to make it becomes clean data
separated_data <- sales_data %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")
View(separated_data)
#Filter the sales dataset to include only transactions in 2020 
transaction_2020 <- separated_data %>%
  filter(Year == "2020") %>%
  summarise(count2020transaction = n())
transaction_2020

#Q2
#The total revenue per product can be calculated by using UnitPrice * Quantity
#Use function mutate in tidyr to create a new variable named TotalRevenue to the dataset 
augmented_sales <- mutate(sales_data, TotalRevenue = UnitPrice * Quantity)
augmented_sales
#Summarize total revenue by ProductId
revenue_summary <- augmented_sales %>%
  group_by(ProductId) %>%
  summarise(TotalRevenue = sum(TotalRevenue))

#Find ProductId with the highest total revenue 
max_revenue_product <- revenue_summary %>%
  arrange(desc(TotalRevenue)) %>%
  head(1)
max_revenue_product

#Filter ProductName from products_data
product_name <- products_data %>%
  filter(ProductId == max_revenue_product$ProductId) %>%
  select(ProductName)

#Filter StoreId from inventory_data
store_id <- inventory_data %>%
  filter(ProductId == max_revenue_product$ProductId) %>%
  select(StoreId)

#Print result
product_name
store_id

#Q3
#Group inventory_data by StoreId and summarize average quantity available
average_quantity <- inventory_data %>%
  group_by(StoreId) %>%
  summarise(AvgQuantityAvailable = mean(QuantityAvailable))

#Find StoreId with the lowest average quantity available
lowest_quantity_store <- average_quantity %>%
  arrange(AvgQuantityAvailable) %>%
  head(1)

#Print result
lowest_quantity_store

#Q4
#Categorize sales into 'High', 'Medium', and 'Low' based on Quantity
#if quantity >= 50 categorized to high, else go to inner if-else statement,
#Inner if-else statement state that if quantity between 20 to 49 will be categorized to medium, else categorize to low
sales_data <- sales_data %>%
  mutate(SalesCategory = if_else(Quantity >= 50, 'High', 
                                 if_else(Quantity >= 20 & Quantity < 50, 'Medium', 'Low')))
View(sales_data)
#Count the number of 'High' category sales
high_sales_count <- sales_data %>%
  filter(SalesCategory == 'High') %>%
  summarise(CountHighSales = n()) #n()is use to count the row

#Print the count of 'High' category sales
high_sales_count

#Q5
#Arrange the product information dataset in descending order of ProductCost
arranged_products <- products_data %>%
  arrange(desc(ProductCost))

third_most_expensive_product <- arranged_products$ProductName[3]
third_most_expensive_product

#Use tidyr to separate the ProductName into two columns: Product and Brand
separated_products <- arranged_products %>%
  separate(ProductName, into = c("Product", "Brand"), sep = "-", extra = "drop", fill = "right")
#The drop used of line above is to remove the extra no. of columns
#The right used of line above is to ensure the missing pieces are filled with NA
#Extract the Brand of the third most expensive product
third_most_expensive_product <- separated_products$Product[3]

#Print the Brand of the third most expensive product
third_most_expensive_product

#Q6
#Calculate average price and quantity sold for each product
sales_data <- read.csv("sales.csv")
sales_data <- sales_data %>%
  group_by(ProductId) %>%
  mutate(AvgPrice = mean(UnitPrice),
            AvgQuantity = mean(Quantity))

#Find percentage changes between consecutive time periods
sales_data <- sales_data %>%
  arrange(ProductId, Date) %>%
  group_by(ProductId) %>%
  mutate(PriceChange = (UnitPrice - AvgPrice) / AvgPrice * 100,
         QuantityChange = (Quantity - AvgQuantity) / AvgQuantity * 100)

#Calculate Price Elasticity of Demand (PED)
sales_data <- sales_data %>%
  mutate(PriceElasticity = QuantityChange / PriceChange)
  
summary_data <- sales_data[, c("ProductId","PriceChange", "QuantityChange", "PriceElasticity")]
View(summary_data)
summary(summary_data)

#Section B 
#Q1
#Assigning values
average_transactions_per_store_per_week <- 100000000 / 30000

#Conditional statement
if (average_transactions_per_store_per_week > 3500) {
  category <- "High"
} else if (average_transactions_per_store_per_week >= 2500 && average_transactions_per_store_per_week <= 3500) {
  category <- "Medium"
} else {
  category <- "Low"
}

# Print category along with a message
cat("Starbucks stores have an average transaction level of", round(average_transactions_per_store_per_week, 2), "transactions per store per week, which is categorized as", category, ".")

#Q2
#Assigning values or ranges for the five variables
population_density <- 1000 # per square kilometer
income_levels <- 50000 # average income in the area in USD
traffic <- 1000 # average daily traffic count near the location
competitor_presence <- "low" # low, medium, or high
proximity_to_starbucks <- 5 # distance in kilometers to the nearest Starbucks store

#Conditional statement to select the best location
if (population_density > 800 && income_levels > 40000 && traffic > 800 && competitor_presence == "low" && proximity_to_starbucks > 2) {
  classification <- "Ideal location"
} else if (population_density > 600 && income_levels > 35000 && traffic > 600 && competitor_presence == "medium" && proximity_to_starbucks > 3) {
  classification <- "Good location"
} else if (population_density > 400 && income_levels > 30000 && traffic > 400 && competitor_presence == "high" && proximity_to_starbucks > 4) {
  classification <- "Average location"
} else {
  classification <- "Poor location"
}

#Print out the classification along with a message
cat("The location is classified as", classification, "for opening a new Starbucks store.")

#Q3
#Assuming weather and time of day as factors
weather <- "hot" 
time_of_day <- "afternoon" 

#Conditional statement to promote a specific Starbucks product
#Only promote breakfast for morning, beverages promotion will be after morning
if (time_of_day == "morning") {
  product_to_promote <- "Pastries"
} else if (weather == "hot" && time_of_day != "morning") {
  product_to_promote <- "Cold beverages"
} else if (weather == "cold" && time_of_day != "morning"){
  product_to_promote <- "Hot beverages"
} else {
  product_to_promote <- "No promotion"
}

#Print out the product to be promoted along with a message
cat("Based on the factors, the promotion should be for", product_to_promote)
