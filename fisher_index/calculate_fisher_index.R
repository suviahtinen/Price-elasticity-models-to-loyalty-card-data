

# Compute Fisher Ideal Price Index 

compute_fisher_index <- function(data,ID,kat_9, category_col, euro_col, price_col, quantity_col, time_col) { 
  
  # Ensure time column is factor
  data[[time_col]] <- as.factor(data[[time_col]])
  
  data1 <- data %>%
    filter(.data[[quantity_col]]!=0) %>%
    group_by(.data[[kat_9]]) %>%
    summarise(
      mean_pq_0 =mean(.data[[price_col]] * .data[[quantity_col]], na.rm = TRUE),
      mean_q_0 = mean(.data[[quantity_col]], na.rm = TRUE),
      mean_p_0 = mean(.data[[price_col]], na.rm = TRUE))
  
  
  # Compute Laspeyres and Paasche for each category and time
  
  data2 <- data %>% left_join(data1,by = kat_9) %>%
    filter(.data[[quantity_col]]!=0) %>%
    group_by(.data[[ID]],.data[[time_col]], .data[[category_col]]) %>%
    summarise(
      sum_pq = sum(.data[[price_col]]*.data[[quantity_col]], na.rm = TRUE),
      sum_p_sum_q_0 = sum(.data[[price_col]]*mean_q_0, na.rm = TRUE),
      sum_p_0_sum_q = sum(mean_p_0 *.data[[quantity_col]], na.rm = TRUE),
      sum_pq_0 = sum(mean_pq_0, na.rm = TRUE),
      sum_q = sum(.data[[quantity_col]], na.rm = TRUE),
      sum_p = sum(.data[[price_col]], na.rm = TRUE),
      sum_e = sum(.data[[euro_col]], na.rm = TRUE))
  
  
  index_df <- data2 %>% 
    mutate(
      laspeyres = (sum_p_sum_q_0 / sum_pq_0),
      paasche = (sum_pq / sum_p_0_sum_q),
      fisher_index = sqrt(laspeyres * paasche),
      fisher_index_100 = 100 * fisher_index,
      price_sum_category = sum_p,
      quantity_sum_category = sum_q,
      euro_sum_category=sum_e
    ) %>%
    select(.data[[ID]],.data[[time_col]], .data[[category_col]],price_sum_category,euro_sum_category,quantity_sum_category,fisher_index,fisher_index_100)
  
  return(index_df)
}