

# Compute Fisher Ideal Price Index 

# Purpose:

# This script calculates the Fisher Ideal Price Index from transaction or
# purchase data. The Fisher Index combines the Laspeyres and Paasche price
# indices using their geometric mean, providing a balanced measure of price
# change while accounting for changes in purchasing behavior.
#
# Input data requirements:
# - Data
# - Household identifier (ID)
# - Product identifier (food_product)
# - Food category (category_col)
# - Quantity purchased (quantity_col)
# - Unit price (price_col)
# - Time period (time_col)
#
# Output:
# - Data identifier each household, timepoint and category
# - Sums the columns of prices and quantities (price_sum_category and quantity_sum_category) 
# - Calculated Fisher Ideal Price Index and multiplied by 100 (fisher_index and fisher_index_100)



compute_fisher_index <- function(data,ID,food_product, category_col, price_col, quantity_col, time_col) { 
  
  # Ensure time column is factor
  data[[time_col]] <- as.factor(data[[time_col]])
  
  data1 <- data %>%
    filter(.data[[quantity_col]]!=0) %>%
    group_by(.data[[food_product]]) %>%
    summarise(
      mean_pq_0 =mean(.data[[price_col]] * .data[[quantity_col]], na.rm = TRUE),
      mean_q_0 = mean(.data[[quantity_col]], na.rm = TRUE),
      mean_p_0 = mean(.data[[price_col]], na.rm = TRUE))
  
  
  # Compute Laspeyres and Paasche for each category and time
  
  data2 <- data %>% left_join(data1,by = food_product) %>%
    filter(.data[[quantity_col]]!=0) %>%
    group_by(.data[[ID]],.data[[time_col]], .data[[category_col]]) %>%
    summarise(
      sum_pq = sum(.data[[price_col]]*.data[[quantity_col]], na.rm = TRUE),
      sum_p_sum_q_0 = sum(.data[[price_col]]*mean_q_0, na.rm = TRUE),
      sum_p_0_sum_q = sum(mean_p_0 *.data[[quantity_col]], na.rm = TRUE),
      sum_pq_0 = sum(mean_pq_0, na.rm = TRUE),
      sum_q = sum(.data[[quantity_col]], na.rm = TRUE),
      sum_p = sum(.data[[price_col]], na.rm = TRUE))
  
  
  index_df <- data2 %>% 
    mutate(
      laspeyres = (sum_p_sum_q_0 / sum_pq_0),
      paasche = (sum_pq / sum_p_0_sum_q),
      fisher_index = sqrt(laspeyres * paasche),
      fisher_index_100 = 100 * fisher_index,
      price_sum_category = sum_p,
      quantity_sum_category = sum_q
    ) %>%
    select(.data[[ID]],.data[[time_col]], .data[[category_col]],price_sum_category,quantity_sum_category,fisher_index,fisher_index_100)
  
  return(index_df)
}