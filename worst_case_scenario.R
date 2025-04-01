library(fitdistrplus)
library(tidyverse)
library(readxl)
data <- read_excel("2025 Case Comp Data CLEAN.xlsx")

employees_table_company <- data %>% 
  dplyr::select(`Company ID`, Industry, `2023` = `Number of employees 2023`, `2024` = `Number of employees 2024`, `2025` = `Number of employees 2025`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "employees") %>%
  mutate(year = as.numeric(year))

claims_table_company <- data %>% 
  dplyr::select(`Company ID`, Industry, `2023` = `2023 number of claims`, `2024` = `2024 number of claims`, `2025` = `2025 number of claims`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "claims") %>% 
  mutate(year = as.numeric(year)) %>% 
  full_join(employees_table_company) %>% 
  mutate(claim_rate = claims/employees)

costs_table_company <- data %>% 
  dplyr::select(Industry, `Company ID`, `2023` = `2023 medical costs`, `2024` = `2024 medical costs`, `2025` = `2025 medical costs`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "costs") %>%
  mutate(year = as.numeric(year)) %>% 
  full_join(claims_table_company) %>% 
  mutate(avg_costs = costs / employees) %>% 
  dplyr::select(`Company ID`, Industry, year, costs, claims, avg_costs)

costs_table_company_fish <- costs_table_company %>% 
  filter(Industry == "Fishing and Agriculture") %>% 
  mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE))

costs_table_company_others <- costs_table_company %>% 
  filter(Industry != "Fishing and Agriculture") %>% 
  mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE))

freq_func <- function(industry, outlier) {
  table <- costs_table_company %>% 
    filter(Industry == industry) %>% 
    mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE)) %>% 
    pull(outlier) %>% 
    table()
  out_non <- c(table[1]/(table[1]+table[2]),
  table[2]/(table[1]+table[2]))
  if_else(!outlier, out_non[1], out_non[2]) %>% return()
}

outlier_mean <- function(industry) {
  costs_table_company %>% 
  filter(Industry == industry) %>% 
  mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE)) %>% 
    filter(outlier == TRUE) %>% 
  pull(avg_costs) %>% 
  mean()
}

not_outlier_mean <- function(industry) {
  costs_table_company %>% 
    filter(Industry == industry) %>% 
    mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE)) %>% 
    filter(outlier == FALSE) %>% 
    pull(avg_costs) %>% 
    mean()
}

outlier_means <- lapply(unique(data$Industry), outlier_mean) %>% unlist()
non_outlier_means <- lapply(unique(data$Industry), not_outlier_mean) %>% unlist()
outlier_proportion <- lapply(unique(data$Industry), freq_func, TRUE) %>% unlist()
non_outlier_proportion <- lapply(unique(data$Industry), freq_func, FALSE) %>% unlist()

exp_med_cost <- outlier_means * outlier_proportion + non_outlier_means * non_outlier_proportion
medical <- exp_med_cost * employees_2026

wages <- 0.7 * 0.75 * 4/52 * claim_rate_2026 * employees_2026 * avg_wages_2026

tibble(industry = unique(data$Industry),
       Medical = medical,
       Wages = wages,
       Operating = employees_2026 / sum(employees_2026) * 2400000,
       Total = sum(Operating, Wages, Medical)) %>% 
  pivot_longer(cols = c(Operating, Wages, Medical), 
               names_to = "cost_type", 
               values_to = "amount") %>% 
  ggplot(aes(x = industry, y = amount, fill = cost_type)) +
  geom_col() +
  labs(title = "Stacked Column Chart of Costs by Industry",
       x = "Industry",
       y = "Amount",
       fill = "Cost Type") +
  theme_light(base_size=14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### ---- WORST CASE SCENARIO ----
alpha <- 0.99
worst_predict_employees <- function(industry, summary = FALSE, assumption = FALSE) {
  model <- lm(sqrt(employees) ~ year + Industry, data = employees_table)
  if(summary) {print(summary(model))}
  if(assumption) {print(plot(model))}
  predicted_value <- predict(model, newdata = data.frame(year = 2026, Industry = industry), interval = "confidence", level = alpha)
  # result <- list(model = model, prediction = predicted_value)
  # return(result)
  return(predicted_value^2)
}

worst_outlier_mean <- function(industry) {
  costs_table_company %>% 
    filter(Industry == industry) %>% 
    mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE)) %>% 
    filter(outlier == TRUE) %>% 
    pull(avg_costs) %>% 
    quantile(alpha)
}
worst_non_outlier_mean <- function(industry) {
  costs_table_company %>% 
    filter(Industry == industry) %>% 
    mutate(outlier = if_else(avg_costs > 1.5*IQR(avg_costs)+quantile(avg_costs, 0.75), TRUE, FALSE)) %>% 
    filter(outlier == FALSE) %>% 
    pull(avg_costs) %>% 
    quantile(alpha)
}
worst_freq_func <- function(industry, outlier) {
  p <- freq_func(industry, TRUE)
  n <- costs_table_company %>% 
    filter(Industry == industry) %>% 
    nrow()
  qbinom(alpha, n, p)/n
}

worst_predict_wage <- function(industry, summary = FALSE, assumptions = FALSE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_wages ~ year + Industry, data = wages_table)
  if(summary) {print(summary(model))}
  if(assumptions) {print(plot(model))}
  predict(model, newdata = data.frame(year = 2026, Industry = industry), interval = "confidence", level = alpha) %>% 
    return()
}

worst_employees <- t(sapply(unique(data$Industry), worst_predict_employees))[,3]
worst_employees
worst_outlier_means <- sapply(unique(data$Industry), worst_outlier_mean)
worst_outlier_means
worst_non_outlier_means <- sapply(unique(data$Industry), worst_non_outlier_mean)
worst_non_outlier_means
worst_outlier_proportion <- sapply(unique(data$Industry), worst_freq_func, TRUE)
worst_non_outlier_proportion <- 1 - worst_outlier_proportion

worst_med_cost <- worst_outlier_means * worst_outlier_proportion + worst_non_outlier_means * worst_non_outlier_proportion
worst_med_cost * worst_employees

worst_predict_wages <- t(sapply(unique(data$Industry), worst_predict_wage))[,3]

# ----

fit_dist <- function(industry, outlierblah) {
  industry_data <- costs_table_company_fish %>% 
    filter(outlier == outlierblah)
  fit.in <- fitdist(log(industry_data$costs), "norm")
  print(exp(fit.in$estimate))
  
  # par(mfrow = c(2,2))
  # denscomp(fit.in)
  # qqcomp(fit.in)
  # cdfcomp(fit.in)
  # ppcomp(fit.in)
}

# graphs for slides ----
scaling_factor <- max(outlier_proportion, na.rm = TRUE) / max(outlier_means, na.rm = TRUE)

tibble(industry = unique(data$Industry), outlier_proportion, outlier_means, non_outlier_means) %>%
  ggplot(aes(x = industry)) +
  geom_col(aes(y = outlier_proportion), alpha = 0.85, fill = "#ffe600") +
  geom_point(aes(y = outlier_means * scaling_factor, color = "Outlier Means"), size = 3, shape = 4) +
  geom_point(aes(y = non_outlier_means * scaling_factor, color = "Non-Outlier Means"), size = 3, shape = 5) +
  scale_y_continuous(
    name = "Outlier Proportion",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Mean Medical Cost")  # Reverse scaling for secondary axis
  ) +
  scale_color_manual(values = c("Outlier Means" = "red", "Non-Outlier Means" = "blue")) + # Assign colors
  theme_light(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
  labs(
    x = "Industry",
    title = "Outlier Proportion and Outlier Mean Medical Cost",
    subtitle = "By Industry",
    color = "Legend"
  )

