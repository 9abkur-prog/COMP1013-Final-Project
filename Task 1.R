#Task 1: Product Prices and Shipping Costs

library(tidyverse)


##Calculate requested statistics of item prices
order_items = read.csv("order_items.csv")
products    = read.csv("products.csv")
mean_price   = mean(order_items$price, na.rm = TRUE)
median_price = median(order_items$price, na.rm = TRUE)


##Plotting the distribution of item prices
ggplot(order_items, aes(x = price)) +
  geom_histogram(binwidth = 25, fill = "blue", color = "white") +
  geom_vline(xintercept = mean_price, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = median_price, color = "darkgreen", linewidth = 1) + 
  xlim(0, 1000) + labs(
    title = "Distribution of Item Prices (capped at 1000 Real for clarity)",
    subtitle = "Red dashed line = Mean | Green solid line = Median",
    x = "Price",
    y = "Count")

###The distribution is mostly low to mid price, with a few high value items


##Freight Value
items_products = order_items %>%
  inner_join(products, by = "product_id") %>%
  filter(!is.na(product_weight_g), !is.na(freight_value))

scatterPlot = ggplot(items_products, aes(x = product_weight_g/1000, y = freight_value))+
  geom_point(color = "red", size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(
    title = "Freight Value vs Product Weight",
    x = "Product weight(kg)",
    y = "Freight value(BRL)")


print(scatterPlot)
###Heavier items cause higher shipping charges

#Task 2

##Read data, make percentage of order statuses
orders    = read_csv("orders.csv")
customers = read_csv("customers.csv")

status_summary = orders %>%
  count(order_status, name = "order_count") %>%
  mutate(
    pct = (order_count / sum(order_count)) * 100
  )

###Pct = percentage

print(status_summary %>% arrange(desc(order_count)))



##Percentage and total of not delivered
not_delivered_total = orders %>%
  summarise(
    total_orders   = n(),
    not_delivered = sum(order_status != "delivered"),
    percentage   = mean(order_status != "delivered") * 100
  )

print(not_delivered_total)


##Barplot of statuses of orders
bar_status = ggplot(status_summary, aes(x = order_status, y = order_count)) + 
  geom_col(aes(fill = order_status == "delivered"), show.legend = FALSE) +
  coord_flip(ylim = c(0, max(status_summary$order_count))) + 
  labs(
    title    = "Order Count and Share by Order Status",
    subtitle = "97.02% of orders were delivered; 2.98% (2963 orders) were not delivered",
    x        = "Order Status",
    y        = "Number of Orders"
  )
print(bar_status)

##Orders not delivered per state 
undelivered = orders %>%
  inner_join(customers, by = "customer_id") %>%
  mutate(is_not_delivered = order_status != "delivered") %>%
  group_by(customer_state) %>%
  summarise(
    total_orders = n(),
    not_delivered_count = sum(is_not_delivered),
    not_delivered_pct = mean(is_not_delivered) * 100
  )
print(undelivered %>% arrange(desc(not_delivered_pct)), n = 30)


##Bar chart of not delivered per state, along with the average
bar_undelivered = ggplot(undelivered, aes(x = customer_state, y = not_delivered_pct)) +
  geom_col(fill = "#FF1222") +
  geom_hline(yintercept = not_delivered_total$percentage,
             linetype = "dashed", color = "#00F", linewidth = 0.9) +
  coord_flip()+
  labs(
    title    = "Percentage of Orders Not Delivered by Customer State",
    subtitle = "Dashed line shows overall national non-delivery rate (2.98%)",
    x        = "Customer State",
    y        = "Not Delivered (%)"
  )

print(bar_undelivered)

###Something might be up with SE, RO, RJ, PI, PE, PB, MA, GO, CE, BA, AL
###Something is DEFINITELY up with RR

#Task 3


##Read file
payments = read.csv("payments.csv")

##Usage frequency of each payment type
payment_summary = payments %>%
  group_by(payment_type) %>% 
  summarise(
    payment_count = n(),
    pct           = (n() / nrow(payments)) * 100,
    mean_value    = mean(payment_value, na.rm = TRUE),
    median_value  = median(payment_value, na.rm = TRUE),
    iqr_value     = IQR(payment_value, na.rm = TRUE),
    max_value     = max(payment_value, na.rm = TRUE),
  ) %>% 
  arrange(desc(payment_count))

print(payment_summary)

##Bar chart of payment type
payment_methods = payment_summary %>%
  ggplot(aes(x = payment_type, y = payment_count)) + 
  geom_col(fill = "#FE3F4A") +
  coord_flip() +
  labs(
    title = "Payment Method Usage Frequency",
    subtitle = "Credit cards are used most often (73.92% of all transactions)",
    x = "Payment Type",
    y = "Number of Payments"
  ) 

print(payment_methods)

##Boxplot for payment values (log10 scale)
###Ensure that log10 has no negatives
valid_payments = payments %>% 
  filter(payment_value > 0)

boxplot_values = ggplot(valid_payments, aes(x = payment_type, y = payment_value,
                                            fill = payment_type)) +
  geom_boxplot() +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000)) +
  labs(
    title = "Distribution of Payment Values by Payment Method (Log10 Scale)",
    subtitle = "Credit cards used for most expensive purchases, vouchers for cheapest",
    x = "Payment Type",
    y = "Payment Value (BRL, Log10 Scale)"
  )

print(boxplot_values)