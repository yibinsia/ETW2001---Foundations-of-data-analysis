# Load necessary library
library(dplyr)
library(ggplot2)
setwd("D:Downloads")

# Section A
# Load datasets
olist_orders_dataset <- read.csv("olist_orders_dataset.csv")
olist_order_items_dataset <- read.csv("olist_order_items_dataset.csv")
olist_order_reviews_dataset <- read.csv("olist_order_reviews_dataset.csv")
olist_products_dataset <- read.csv("olist_products_dataset.csv")
olist_customers_dataset <- read.csv("olist_customers_dataset.csv")
olist_sellers_dataset <- read.csv("olist_sellers_dataset.csv")

# Question 1
# Perform an inner join between the two datasets
merged_dataset <- inner_join(olist_orders_dataset, olist_order_items_dataset, by = 'order_id')
glimpse(merged_dataset)

# Question 2
# Perform a left join between the two datasets
merged_dataset2 <- left_join(olist_orders_dataset, olist_order_reviews_dataset)

# Count the number of orders without reviews
orders_without_reviews <- merged_dataset2 %>% 
  filter(is.na(review_score)) %>% 
  summarize(orders_without_reviews = n())

# Print the result
orders_without_reviews

# Question 3
# Perform a right join between the two datasets
merged_dataset3 <- right_join(olist_order_items_dataset, olist_products_dataset)

# Calculated products that have not been sold (have NULL values in order_item_id)
unsold_products <- merged_dataset3 %>% 
  filter(is.na(order_item_id)) %>%
  summarize(unsold_products = n())
  
# Print the unsold products
unsold_products

# Question 4
# Perform a full join between the two datasets
merged_dataset4 <- full_join(olist_customers_dataset, olist_orders_dataset, by = "customer_id")

# Filter the merged dataset for any customers without orders or orders without customer details
customers_without_orders <- merged_dataset4 %>%
  filter(is.na(order_id))

orders_without_customer_details <- merged_dataset4 %>%
  filter(is.na(customer_id))

# Print the results
customers_without_orders
orders_without_customer_details

# Question 5
# Perform semi join between the two datasets
active_sellers <- olist_sellers_dataset %>%
  semi_join(olist_order_items_dataset, by = "seller_id")

# Determine the total number of unique sellers in each dataset
total_sellers_olist_sellers <- nrow(unique(olist_sellers_dataset))
total_sellers_active_sellers <- nrow(unique(active_sellers))

# Find the number of common sellers between the datasets
common_sellers <- nrow(unique(merge(olist_sellers_dataset, active_sellers, by = "seller_id")))

# Calculate the similarity percentage
similarity_percentage <- (common_sellers / total_sellers_olist_sellers) * 100
similarity_percentage

# Question 6  
# Anti join to identify customers who have never placed an order
customers_without_orders <- olist_customers_dataset %>%
  anti_join(olist_orders_dataset, by = "customer_id")

summary(customers_without_orders)
customers_without_orders

# Question 7
# Join orders with order items using inner_join
orders_with_items <- inner_join(olist_orders_dataset, olist_order_items_dataset, by = "order_id")

# Join orders_with_items with products using left_join
orders_with_products <- left_join(orders_with_items, olist_products_dataset, by = "product_id")

# Join orders_with_products with sellers using right_join
comprehensive_dataset <- right_join(orders_with_products, olist_sellers_dataset, by = "seller_id")

# Analyze the flow of products within sellers
products_per_seller <- comprehensive_dataset %>%
  group_by(seller_id) %>%
  summarize(num_products = n_distinct(product_id))

# Calculate the average number of products sold per seller
avg_products_per_seller <- mean(products_per_seller$num_products)

cat("Average number of products sold per seller:", avg_products_per_seller, "\n")

# Analyze the flow of products within orders
products_per_order <- comprehensive_dataset %>%
  group_by(order_id) %>%
  summarize(num_products = n_distinct(product_id))

# Calculate the average number of products placed within an order
avg_products_per_order <- mean(products_per_order$num_products)

cat("Average number of products per order:", avg_products_per_order, "\n")

# Visualize the distribution of order values
ggplot(comprehensive_dataset, aes(x = price)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Order Values",
       x = "Order Value",
       y = "Frequency") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(color = "black"))

# Section B
# Question 1
# Create a data frame with product lines and their corresponding percentages
product_lines <- c("Trains", "Ships", "Trucks and Buses", "Planes", "Motorcycles", "Vintage Cars", "Classic Cars")
percentages <- c(2.7, 8.3, 10.7, 10.8, 11.7, 21.5, 34.3)
colors <- c("#FFC0CB", "#8B4513", "#800080", "#FF0000", "#008000", "#FFA500", "#0000FF") # Color according to fig.3
data <- data.frame(product_lines, percentages)

# Create the pie chart with percentage labels inside and product line names outside
pie_chart <- ggplot(data, aes(x = "", y = percentages, fill = product_lines)) +
  geom_bar(stat = "identity", width = 1.5) +
  coord_polar("y", start = 0) +
  # Fill the each segment color and without showing the legend
  scale_fill_manual(values = colors, guide = "none") +
  # Paste the segment percentage in the middle of the segment
  geom_text(aes(label = paste0(percentages, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", 
            size = 2) +
  # Paste the name of each product line around the segment
  geom_text(aes(label = product_lines, "\n"), 
            position = position_stack(vjust = 0.5),
            hjust = 0.5,
            color = "black", 
            size = 2) +
  labs(title = "Sales Distribution by PRODUCTLINE (Pie Chart)",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(color = "black"))

# Question 2
# Create sample data 
set.seed(123)  # for reproducibility
index <- seq(1, 2500)
quantity_ordered <- sample(10:200, 2500, replace = TRUE)
price_each <- rnorm(2500, mean = 50, sd = 10)

# Calculate SALES data (total sales = quantity ordered * price each)
sales <- quantity_ordered * price_each

# Create a data frame with calculated sales
sales_data <- data.frame(INDEX = index, QUANTITYORDERED = quantity_ordered, PRICEEACH = price_each, SALES = sales)

# Create the area chart
area_chart <- ggplot(sales_data) +
  geom_area(aes(x = INDEX, y = SALES, fill = "SALES"), color = "forestgreen", alpha = 0.7,) +
  geom_area(aes(x = INDEX, y = QUANTITYORDERED, fill = "QUANTITYORDERED"), color = "skyblue", alpha = 0.7) +
  geom_area(aes(x = INDEX, y = PRICEEACH, fill = "PRICEEACH"), color = "orange", alpha = 0.7) +
  labs(title = "Area Chart of Sales Data",
       x = "Index",
       y = "Values") +
  # Fill each area with corresponding color
  scale_fill_manual(name = "Variables",
                    values = c("QUANTITYORDERED" = "skyblue", "PRICEEACH" = "orange", "SALES" = "forestgreen")) +
  # Break the y and x axis with specific interval
  scale_y_continuous(expand = c(0, 0),breaks = seq(0, 15000, by = 2000)) +
  scale_x_continuous(expand = c(0, 150),breaks = seq(0, 2750, by = 500)) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),  # Adjust legend position to top left
        legend.box.background = element_rect(color = "black"),  
        legend.title = element_blank(),  # Remove legend title
        plot.title = element_text(hjust = 0.5),  
        panel.background = element_rect(color = "black")) 

# Question 3
# Create sample data 
set.seed(123)
date <- seq(as.Date("2024-04-25"), as.Date("2024-05-22"), by = "3 days")
total_sales <- cumsum(runif(length(date), min = 5000, max = 15000))

# Create a data frame
sales_data <- data.frame(Date = date, Total_Sales = total_sales)

# Create the line chart with data points
line_chart <- ggplot(sales_data, aes(x = Date, y = Total_Sales)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 2) +  # Add data points
  labs(title = "Daily Total Sales Over Time - Line Chart",
       x = "Date",
       y = "Total Sales") +
  # Break y axis into specific intervals
  scale_y_continuous(limits = c(0, 140000), breaks = seq(0, 140000, by = 20000)) +
  scale_x_date(date_breaks = "3 days", date_labels = "%Y-%m-%d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),# Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(color = "black"))  

# Question 4
# Create sample data
deal_sizes <- data.frame(
  Deal_Size = factor(c("Medium", "Small", "Large"), levels = c("Medium", "Small", "Large")),
  Count = c(1400, 1300, 200)
)

# Create the bar chart
bar_chart <- ggplot(deal_sizes, aes(x = Deal_Size, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Deal sizes - Bar Chart",
       x = "Deal Size",
       y = "Count") +
  # Break y axis into specific intervals
  scale_y_continuous(limits = c(0, 1400), breaks = seq(0, 1400, by = 200)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none", # Remove legend
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(color = "black")) 


# Question 5
# Create sample data
sales_by_country <- data.frame(
  Country = c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Ireland", 
              "Italy", "Japan", "Norway", "Philippines", "Singapore", "Spain", "Sweden", "Switzerland", "UK", "USA"),
  Number_of_Sales = c(200, 80, 60, 90, 85, 100, 300, 100, 10, 80, 30, 100, 30, 100, 300, 120, 80, 150, 1000)
)

# Create the bar plot
bar_plot <- ggplot(sales_by_country, aes(x = Country, y = Number_of_Sales)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  labs(title = "Number of Sales by Country",
       x = "Country",
       y = "Number of Sales") +
  # Break y axis into specific intervals and set the lowerbound and upperbound
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 200)) +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(hjust = 0.5))


