library(tidyverse)
library(readxl)
data <- read_excel("2025 Case Comp Data CLEAN.xlsx")
view(data)
wage_table <- data %>% 
  mutate(
   avg_wage_2023 = `2023 wages` / `Number of employees 2023`,
   avg_wage_2024 = `2024 wages` / `Number of employees 2024`,
   avg_wage_2025 = `2025 wages` / `Number of employees 2025`
  ) %>% 
  select(`Company ID`, Industry, `2023` = avg_wage_2023, `2024` = avg_wage_2024, `2025` = avg_wage_2025) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "avg_wage") %>%
  mutate(year = as.numeric(year))

cost_table <- data %>% 
  mutate(
    avg_costs_2023 = `2023 medical costs` / `2023 number of claims`,
    avg_costs_2024 = `2024 medical costs` / `2024 number of claims`,
    avg_costs_2025 = `2025 medical costs` / `2025 number of claims`
  ) %>% 
  select(`Company ID`, Industry, `2023` = avg_costs_2023, `2024` = avg_costs_2024, `2025` = avg_costs_2025) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "avg_cost") %>%
  mutate(year = as.numeric(year))

employees_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `Number of employees 2023`, `2024` = `Number of employees 2024`, `2025` = `Number of employees 2025`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "employees") %>%
  mutate(year = as.numeric(year))

prepped_data <- full_join(wage_table, cost_table) %>%
  full_join(employees_table) %>% 
  mutate(percentile = if_else(avg_cost > quantile(avg_cost, 0.95), TRUE, FALSE),
         fish = if_else(Industry == "Fishing and Agriculture", "F&G industry", "Others"))

# make a quick visualisation
company_counts <- prepped_data %>%
  group_by(Industry) %>%
  summarise(n_companies = n_distinct(`Company ID`))

avg_wage_plot <- ggplot(prepped_data, aes(x = year, y = avg_wage)) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Average wage over time by company",
    x = "Year",
    y = "Average wage"
  ) +
  geom_smooth(method = "lm") +
  facet_wrap(~Industry, labeller = labeller(Industry = function(x) {
    paste0(x, "\n(Number of Companies: ", company_counts$n_companies[match(x, company_counts$Industry)], ")")
  })) +
  theme_light(base_size=14)
avg_wage_plot
# looks like regression split by industry will be appropriate. fairly linear trends and in relatively close bands amongst each industry. No huge outliers anywhere

avg_cost_plot <- ggplot(prepped_data, aes(x = year, y = avg_cost, colour = percentile)) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Average medical costs over time by company",
    x = "Year",
    y = "Average costs"
  ) +
  facet_wrap(~fish) +
  theme_light(base_size=14)
avg_cost_plot
# possibly don't want to split these by industry, they seem similar cost no matter the industry. occasional large outliers indicating regression may be a poor choice. possibly just use a flat line model and then incorporate occasional outliers at a much higher cost (also fishing and agriculture has lots of incidents it seems)

employees_plot <- prepped_data %>% 
  ggplot(aes(x = year, y = sqrt(employees))) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Sqrt(Employees) over time by company",
    x = "Year",
    y = "Sqrt(No. of employees)"
  ) +
  stat_smooth(method = "lm", formula = y ~ x ) +
  facet_wrap(~Industry, scales = "free_y", labeller = labeller(Industry = function(x) {
    paste0(x, "\n(Number of Companies: ", company_counts$n_companies[match(x, company_counts$Industry)], ")")
  })) +
  theme_light(base_size = 14)
employees_plot  



claims_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `2023 number of claims`, `2024` = `2024 number of claims`, `2025` = `2025 number of claims`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "claims") %>% 
  mutate(year = as.numeric(year))
claim_data <- full_join(claims_table, employees_table) %>% 
  group_by(Industry, year) %>% 
  summarise(claim_rate = sum(claims)/sum(employees))
claim_data %>% 
  ggplot(aes(x=year, y=claim_rate, colour = Industry)) +
  geom_line() +
  theme_light()
# using a flatline (average of past 3 years) to predict the claims rate
# the graph seems to suggest that Property and Business Service is decreasing but unsure if this will continue in the future. using the flatline might be overestimating.

claim_rate_plot <- full_join(claims_table, employees_table) %>% # a plot by individual companies to show there are minimal outliers - and the ones that are outliers have like 3 employees and had 1 claim. this is why claims were instead looked at on an industry level.
  ggplot(aes(x=year, y=claims/employees)) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Claim rate over time by company and industry",
    x = "Year",
    y = "Claim rate"
  ) +
  facet_wrap(~Industry, scales = "free_y", labeller = labeller(Industry = function(x) {
    paste0(x, "\n(Number of Companies: ", company_counts$n_companies[match(x, company_counts$Industry)], ")")
  })) +
  theme_light(base_size=14)
claim_rate_plot
# to illustrate that it is basically all small companies, lets remove all companies with less than 100 employees (the 1st decile is 101 employees)
claim_rate_plot_no_small_businesses <- filter(full_join(claims_table, employees_table), employees >= 100) %>% 
  ggplot(aes(x=year, y=claims/employees)) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Claim rate over time by company and industry",
    x = "Year",
    y = "Claim rate"
  ) +
  facet_wrap(~Industry, scales = "free_y") +
  theme_light()
claim_rate_plot_no_small_businesses

predict_claim_rate <- function(industry) {
  # input an industry and then output an average claim_rate in 2026
  # regression doesn't seem suitable but an average of the 3 years seems appropiate
  claim_data %>% 
    filter(Industry == industry) %>% 
    pull(claim_rate) %>% 
    mean() %>% 
    return()
}
predict_claim_rate("Education")
predict_claim_rate("Accommodation and Hospitality")
predict_claim_rate("Fishing and Agriculture")
predict_claim_rate("Government Administration")
predict_claim_rate("Health and Community")
predict_claim_rate("Property and Business Service")

# what I'm aiming to be able to do
predict_wage <- function(industry, summary = FALSE, assumptions = FALSE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_wage ~ year + Industry, data = prepped_data)
  if(summary) {print(summary(model))}
  if(assumptions) {print(plot(model))}
  predict(model, newdata = data.frame(year = 2026, Industry = industry)) %>% 
    return()
}
predict_wage("Education")
predict_wage("Accommodation and Hospitality")
predict_wage("Fishing and Agriculture")
predict_wage("Government Administration")
predict_wage("Health and Community")
predict_wage("Property and Business Service")
# significant model with R^2 of 76% - so pretty good
# the curves in the plot look like they fit well

# --------------------------------------------------------------
predict_cost <- function(industry, summary = TRUE, assumptions = TRUE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_cost ~ year + Industry, data = prepped_data)
  if(summary) {print(summary(model))}
  if(assumptions) {print(plot(model))}
  predict(model, newdata = data.frame(year = 2026, Industry = industry)) %>% 
    return()
}
predict_cost("Education") # NOT SIGNIFICANT
predict_cost("Accommodation and Hospitality") # NOT SIGNIFICANT
predict_cost("Fishing and Agriculture") # NOT SIGNIFICANT
predict_cost("Government Administration") # NOT SIGNIFICANT
predict_cost("Health and Community") # NOT SIGNIFICANT
predict_cost("Property and Business Service") # NOT SIGNIFICANT
predict_cost(unique(data$Industry)) # SIGNIFICANT but bad R^2
# --------------------------------------------------------------

employees_by_industry <- prepped_data %>%
  group_by(Industry, year) %>%
  summarise(employees = sum(employees))
employees_by_industry %>%
  ggplot(aes(x=year, y=(employees)^0.5, colour = Industry)) +
  geom_line() +
  theme_light()
predict_employees <- function(industry, summary = TRUE, assumption = TRUE) {
  model <- lm(sqrt(employees) ~ year + Industry, data = employees_by_industry)
  if(summary) {print(summary(model))}
  if(assumption) {print(plot(model))}
  predicted_value <- predict(model, newdata = data.frame(year = 2026, Industry = industry))
  result <- list(model = model, prediction = predicted_value)
  return(result)
}
predict_employees("Education") # 3401.457
predict_employees("Accommodation and Hospitality") # 2667.659
predict_employees("Fishing and Agriculture") # 3108.48
predict_employees("Government Administration") # 2788.309
predict_employees("Health and Community")
predict_employees("Property and Business Service")
# the square root transformation works well. make sure to square the predicted value from the regression to get the actual no. of employees.

# ----
# TODO
# check what happens if i remove the health and community outlier (does it substantially change results?)
# work out what to do with medical costs / no. of claims
  # thinking keep as constant baseline with a certain chance of the larger outliers.