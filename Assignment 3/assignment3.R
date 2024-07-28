## Task 2 for Problem Statement 1 ##
# Load necessary libraries
library(dplyr)

# Read Amazon Sale Report from csv file into amazon_data data frame
amazon_data <- read.csv("C:/Users/Sean/Documents/ETW2001 Group Assignment/Amazon Sale Report.csv")
# Return the column names of the amazon_data data frame
colnames(amazon_data)

# Drop NA
# Removing columns "promotion.ids" and "Unnamed..22", then filtering out rows with 
# missing values in columns "Order.ID", "Date", "Category", and "Amount"
amazon_data_cleaned <- amazon_data %>%
  select(-c(promotion.ids, Unnamed..22)) %>%
  filter(!is.na(Order.ID) & !is.na(Date) & !is.na(Category) & !is.na(Amount))

# Converts the "Date" column in the amazon_data_cleaned data frame to the Date data type 
# using the specified format ("%m-%d-%y")
amazon_data_cleaned$Date <- as.Date(amazon_data_cleaned$Date, format = "%m-%d-%y")
# Display the content of amazon_data_cleaned
amazon_data_cleaned

# Analysis profit by Category column
category_analysis <- amazon_data_cleaned %>%
  group_by(Category) %>%
  summarise(Total_Sales = sum(Amount, na.rm = TRUE),
            Average_Amount = mean(Amount, na.rm = TRUE),
            Total_Quantity_Sold = sum(Qty, na.rm = TRUE)) %>%
  mutate(Price_Per_Unit = Total_Sales / Total_Quantity_Sold)
# Display the content of category_analysis
category_analysis

# Arranges the data frame "category_analysis" in descending order based on the "Price_Per_Unit" 
# column to identify the categories with the highest profit per unit.
highest_profit_categories <- category_analysis %>%
  arrange(desc(Price_Per_Unit))
# Display the content of highest_profit_categories
highest_profit_categories



## Task 3 for Problem Statement 1 ##
# bar chart
highest_profit_categories <- highest_profit_categories %>%
  mutate(Total_Sales_Million = Total_Sales / 1e6)

# Create the bar plot
bar_plot <- ggplot(highest_profit_categories, aes(x = reorder(Category, -Total_Sales), y = Total_Sales_Million)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Total Sales by Category",
       x = "Category",
       y = "Total Sales (in millions)") +
  # Adjust y axis limits based on the data
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 5)) +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))

print(bar_plot)

# area chart
library(ggplot2)
area <- ggplot(highest_profit_categories, aes(x = Category, y = Price_Per_Unit, group = 1)) +
  geom_area(fill = "lightblue", alpha = 0.5) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Area Chart of Price per Unit by Category",
       x = "Category",
       y = "Price per Unit") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(linetype = "dashed")
  )
area


## Task 2 for Problem Statement 2 ##

# Load necessary libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# Load datasets
amazon_sales <- read.csv("Amazon Sale Report.csv")
international_sales <- read.csv("International sale Report.csv")

# Convert date columns to Date type
amazon_sales$Date <- as.Date(amazon_sales$Date, format = "%m-%d-%y")
international_sales$DATE <- as.Date(international_sales$DATE, format = "%m-%d-%y")

# Filter out cancelled orders from the Amazon sales data
amazon_sales <- amazon_sales %>%
  filter(Status != "Cancelled")

# Omit rows with any NA values
amazon_sales <- na.omit(amazon_sales)
international_sales <- na.omit(international_sales)

# Rename columns to standardize across datasets
# Select required variables 
# Convert relevant columns to numeric
# Filter datasets for April 2022
# Aggregate data by date
amazon_sales_april <- amazon_sales %>%
  rename(Quantity = Qty) %>%
  select(Date, Quantity, Amount) %>%
  mutate(Quantity = as.numeric(Quantity), Amount = as.numeric(Amount)) %>%
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2022-04-30")) %>%
  group_by(Date) %>%
  summarize(Domestic_Quantity = sum(Quantity), Domestic_Amount = sum(Amount))

international_sales_april <- international_sales %>%
  rename(Quantity = PCS, Amount = GROSS.AMT, Date = DATE) %>%
  select(Date, Quantity, Amount) %>%
  mutate(Quantity = as.numeric(Quantity), Amount = as.numeric(Amount)) %>%
  filter(Date >= as.Date("2022-04-01") & Date <= as.Date("2022-04-30")) %>%
  group_by(Date) %>%
  summarize(International_Quantity = sum(Quantity), International_Amount = sum(Amount))

# Combine datasets with full join to include all dates from both datasets
combined_data_april <- full_join(amazon_sales_april, international_sales_april, by="Date")

# Handle potential NAs introduced by the join
combined_data_april <- combined_data_april %>%
  mutate(
    Domestic_Quantity = ifelse(is.na(Domestic_Quantity), 0, Domestic_Quantity),
    Domestic_Amount = ifelse(is.na(Domestic_Amount), 0, Domestic_Amount),
    International_Quantity = ifelse(is.na(International_Quantity), 0, International_Quantity),
    International_Amount = ifelse(is.na(International_Amount), 0, International_Amount)
  )

## Task 3 for Problem Statement 2 ##
# Visualize day-by-day sales trends for April
ggplot(combined_data_april, aes(x=Date)) +
  geom_line(aes(y=Domestic_Amount, color="Domestic"), linetype="solid") +
  geom_line(aes(y=International_Amount, color="International"), linetype="dashed") +
  labs(title="Daily Sales Trends in April 2022: Domestic vs International", x="Date", y="Sales Amount", color="Sales Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Task 2 for Problem Statement 3 ##
library(dplyr)
library(ggplot2)

Amazon = read.csv("Amazon Sale Report.csv")
Report = read.csv("Sale Report.csv")
# Task 2
Amazon_Sale_Report = inner_join(Amazon, Report, by = c('SKU' = 'SKU.Code'), relationship = "many-to-many")
str(Amazon_Sale_Report) # data structure
colSums(is.na(Amazon_Sale_Report)) # number of column contain missing data
Amazon_Sale_Report = na.omit(Amazon_Sale_Report) # remove missing value
nrow(Amazon_Sale_Report) # number of rows
ncol(Amazon_Sale_Report) # number of columns
summary(Amazon_Sale_Report)

# Task 3
# Color: Calculate Number of product sold, Total Sales and Price per unit 
prob_stats_3_color = Amazon_Sale_Report %>%
  group_by(Color) %>%
  summarise(Total_Quantity = sum(Qty),
            Total_Sales = sum(Amount),
            Price_Per_Unit = sum(Amount)/sum(Qty))

# Find the top 5 popular product color in Amazon
popular_color = prob_stats_3_color %>% arrange(desc(Total_Quantity)) %>% slice(1:5)
# Find the top 5 unpopular product color in Amazon
not_popular_color = prob_stats_3_color %>% arrange(Total_Quantity) %>% slice(1:5)

# Combine them into a new data set named prob_stats_3_color
prob_stats_3_color = bind_rows(popular_color, not_popular_color)
# Defined the color group in prob_stats_3_color data set
defined_color_group = c("Black" = "black", "Blue" = "blue", 
                        "Chiku" = "#E2A949", "Green" = "green", 
                        "Khaki" = "#F0E68C", "Lemon Yellow" = "#FEF250",  
                        "MINT" = "#3EB489", "Mustard" = "#FFDB58", 
                        "Pink" = "pink", "Taupe" = "#483C32")
## Task 3 for Problem Statement 3 ##
# Scatter Plot: For each product color, 
# the quantity sold and its total sales obtained
ggplot(prob_stats_3_color, aes(x = Total_Quantity, 
                               y = Total_Sales, colour = Color)) +
  geom_point(size = 2, show.legend = TRUE) +  
  # size of dots adjustment & show legend
  labs(title = "Scatter Plot of Quantity Sold and 
       Total Sales for each Product Color", 
       x = "Total Quantity", y = "Total Sales", color = "Color") +
  scale_color_manual(values = defined_color_group) + 
  # color adjustment
  xlim(0, 15000) + ylim(0, 9999999) + # axis range adjustment
  theme_minimal()

# Size: Calculate Total Sales Percentage for each Product Size 
Amazon_Total_Sales = Amazon_Sale_Report %>% 
  summarise(Total_Sales = sum(Amount)) # Total Sales in Amazon
prob_stats_3_size = Amazon_Sale_Report %>%
  group_by(Size.x) %>%
  summarise(Total_Sales = sum(Amount)) %>%
  mutate(Product_Size = case_when(
    Size.x %in% c("4XL", "5XL", "6XL", "Free") ~ "Others",
    TRUE ~ as.character(Size.x)
  )) %>% # consider 4XL, 5XL, 6XL, Free as Others group
  group_by(Product_Size) %>%
  summarise(Total_Sales = sum(Total_Sales)) %>%
  mutate(Sales_Percentage = 
           round(Total_Sales / Amazon_Total_Sales$Total_Sales * 100, 2))

# Pie Chart: For each product size, the total sales percentage
ggplot(prob_stats_3_size, 
       aes(x = "", y = Sales_Percentage, 
           fill = factor(Product_Size), 
           label = paste(Product_Size, "\n ", Sales_Percentage, "%", sep = ""))) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5), 
            color = "black", size = 3, aes(x = 1.25)) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Total Sales Percentage by Product Size", 
       fill = "Product Size") +
  theme_void() + # remove necessary element
  theme(legend.position = "none") # remove legend



