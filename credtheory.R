library(tidyverse)
data <- readxl::read_excel("2025 Case Comp Data CLEAN.xlsx")
market_premium <- sum(data$`2023 medical costs`)/sum(data$`Number of employees 2023`)

# expectation of the variances of each group
within_variance <- costs_table %>% 
  group_by(Industry) %>% 
  summarise(variance = var(avg_costs)) %>% 
  ungroup() %>% 
  summarise(mean_variances = mean(variance)) %>% 
  pull(mean_variances)

# variance in the means of the groups
between_variance <- costs_table %>% 
  group_by(Industry) %>% 
  summarise(mean_avg_costs = mean(avg_costs)) %>% 
  pull(mean_avg_costs) %>% 
  var() 

# calculate the credibility weight factor
k <- within_variance / between_variance # 0 < k < 1 indicating there is small within group variation - this is good means that setting an industry wide premium is a good idea.

#2023
specific_industry_MP_2023 <- data %>%
  group_by(Industry) %>%
  summarise(Industry_Premium_2023 = sum(`2023 medical costs`) / sum(`Number of employees 2023`))

credibility_industry_2023 <- data %>%
  group_by(Industry) %>%
  summarise(Credibility_Factor_2023 = sum(`Number of employees 2023`) / (sum(`Number of employees 2023`) + k), .groups = "drop")

combined_data_2023 <- left_join(specific_industry_MP_2023, credibility_industry_2023, by = "Industry")

combined_data_2023 <- combined_data_2023 %>%
  mutate(P_adjusted_2023 = Industry_Premium_2023 * Credibility_Factor_2023 + (1 - Credibility_Factor_2023) * market_premium)

#2024

specific_industry_MP_2024 <- data %>%
  group_by(Industry) %>%
  summarise(Industry_Premium_2024 = sum(`2024 medical costs`) / sum(`Number of employees 2024`))

credibility_industry_2024 <- data %>%
  group_by(Industry) %>%
  summarise(Credibility_Factor_2024 = sum(`Number of employees 2024`) / (sum(`Number of employees 2024`) + k), .groups = "drop")

combined_data_2024 <- left_join(specific_industry_MP_2024, credibility_industry_2024, by = "Industry")

combined_data_2024 <- combined_data_2024 %>%
  mutate(P_adjusted_2024 = Industry_Premium_2024 * Credibility_Factor_2024 + (1 - Credibility_Factor_2024) * market_premium)

#2025

specific_industry_MP_2025 <- data %>%
  group_by(Industry) %>%
  summarise(Industry_Premium_2025 = sum(`2025 medical costs`) / sum(`Number of employees 2025`))

credibility_industry_2025 <- data %>%
  group_by(Industry) %>%
  summarise(Credibility_Factor_2025 = sum(`Number of employees 2025`) / (sum(`Number of employees 2025`) + k), .groups = "drop")

combined_data_2025 <- left_join(specific_industry_MP_2025, credibility_industry_2025, by = "Industry")

combined_data_2025 <- combined_data_2025 %>%
  mutate(P_adjusted_2025 = Industry_Premium_2025 * Credibility_Factor_2025 + (1 - Credibility_Factor_2025) * market_premium)


final_combined_data <- combined_data_2023 %>%
  left_join(combined_data_2024, by = "Industry") %>%
  left_join(combined_data_2025, by = "Industry")
