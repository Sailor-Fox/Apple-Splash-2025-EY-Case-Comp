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

claims_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `2023 number of claims`, `2024` = `2024 number of claims`, `2025` = `2025 number of claims`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "claims") %>%
  mutate(year = as.numeric(year))

prepped_data <- full_join(wage_table, cost_table) %>%
  full_join(employees_table) %>% 
  full_join(claims_table)

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
  theme_light()
avg_wage_plot
# looks like regression split by industry will be appropiate. fairly linear trends and in relatively close bands amongst each industry. No huge outliers anywhere

avg_cost_plot <- ggplot(prepped_data, aes(x = year, y = avg_cost)) +
  geom_line(aes(group = `Company ID`)) +
  geom_point(aes(group = `Company ID`)) +
  labs(
    title = "Average medical costs over time by company",
    x = "Year",
    y = "Average costs"
  ) +
  facet_wrap(~Industry) +
  theme_light() +
  geom_smooth(method = "lm")
avg_cost_plot
# possibly don't want to split these by industry, they seem similar cost no matter the industry. occasional large outliers indicating regression may be a poor choice. possibly just use a flat line model and then incorporate occasional outliers at a much higher cost (also fishing and agriculture has lots of incidents it seems)

# what I'm aiming to be able to do
regress_wage <- function(industry, summary = TRUE, assumptions = FALSE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_wage ~ year, data = filter(avg_table, Industry == industry))
  if(!summary) {print(summary(model))}
  if(assumptions) {print(plot(model))}
  predict(model, newdata = data.frame(year = 2026)) %>% 
    return()
}
regress_wage("Education", TRUE)
regress_wage("Accommodation and Hospitality", TRUE)
regress_wage("Fishing and Agriculture", TRUE)
regress_wage("Government Administration", TRUE)
regress_wage("Health and Community", TRUE)
regress_wage("Property and Business Service", TRUE)
# all highly significant (highest p-value is government of 1% but likely due to smallest sample size)
# R^2 are low but due to limited historical data
# the curves in the plot look like they fit well

regress_cost <- function(industry, summary = TRUE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_cost ~ year, data = filter(avg_table, Industry == industry))
  if(summary) {print(summary(model))}
  predict(model, newdata = data.frame(year = 3)) %>% 
    return()
}
regress_cost("Education", TRUE) # NOT SIGNIFICANT
regress_cost("Accommodation and Hospitality", TRUE) # NOT SIGNIFICANT
regress_cost("Fishing and Agriculture", TRUE) # NOT SIGNIFICANT
regress_cost("Government Administration", TRUE) # NOT SIGNIFICANT
regress_cost("Health and Community", TRUE) # NOT SIGNIFICANT
regress_cost("Property and Business Service", TRUE) # NOT SIGNIFICANT
regress_cost(unique(data$Industry), TRUE) # SIGNIFICANT but bad R^2

# ----
# TODO
# regress and predict for employee numbers
# work out what to do with medical costs / no. of claims
  # thinking keep as constant baseline with a certain chacne of the larger outliers.