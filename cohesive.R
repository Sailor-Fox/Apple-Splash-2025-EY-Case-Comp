# doing everything on an industry level. The plots in main.R with all the companies can still go in the appendix though to show the lack of outliers hence assumptions checked.
library(tidyverse)
library(readxl)
library(patchwork)
data <- read_excel("2025 Case Comp Data CLEAN.xlsx")

# Workforce size by industry ----
employees_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `Number of employees 2023`, `2024` = `Number of employees 2024`, `2025` = `Number of employees 2025`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "employees") %>%
  mutate(year = as.numeric(year)) %>% 
  group_by(Industry, year) %>% 
  summarise(employees = sum(employees))

# idk if we want the graph of the straight employees or the sqrt() - thats what the regression was fitted on
# prolly put employees in main slides and sqrt(employees) in the appendix
employees_table %>% 
  ggplot(aes(x = year, y = sqrt(employees), colour = Industry)) +
  geom_line() +
  geom_point() +
  labs(
    title = "sqrt(Employees) over time by industry",
    x = "Year",
    y = "Sqrt(No. of employees)"
  ) +
  theme_light()

predict_employees <- function(industry, summary = TRUE, assumption = TRUE) {
  model <- lm(sqrt(employees) ~ year + Industry, data = employees_table)
  if(summary) {print(summary(model))}
  if(assumption) {print(plot(model))}
  predicted_value <- predict(model, newdata = data.frame(year = 2026, Industry = industry))
  # result <- list(model = model, prediction = predicted_value)
  # return(result)
  return(predicted_value^2)
}

# Average wage per employee by industry ----
wages_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `2023 wages`, `2024` = `2024 wages`, `2025` = `2025 wages`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "wages") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(Industry, year) %>% 
  summarise(wages = sum(wages)) %>% 
  full_join(employees_table) %>% 
  group_by(Industry, year) %>% 
  mutate(avg_wages = wages / employees)

wages_table %>% 
  ggplot(aes(x=year, y=avg_wages, colour = Industry)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average wages over time by industry",
    x = "Year",
    y = "Average wages"
  ) +
  theme_light(base_size=14) +
  theme(legend.position = "bottom")

predict_wage <- function(industry, summary = FALSE, assumptions = FALSE) {
  # input an industry and then output an average wage in 2026
  model <- lm(avg_wages ~ year + Industry, data = wages_table)
  if(summary) {print(summary(model))}
  if(assumptions) {print(plot(model))}
  predict(model, newdata = data.frame(year = 2026, Industry = industry)) %>% 
    return()
}

# Claim rate by industry ----
claims_table <- data %>% 
  select(`Company ID`, Industry, `2023` = `2023 number of claims`, `2024` = `2024 number of claims`, `2025` = `2025 number of claims`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "claims") %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(Industry, year) %>% 
  summarise(claims = sum(claims)) %>% 
  full_join(employees_table) %>% 
  group_by(Industry, year) %>% 
  mutate(claim_rate = sum(claims)/sum(employees))

claims_table %>% 
  ggplot(aes(x=year, y=claim_rate, colour = Industry)) +
  geom_line() +
  labs(title = "Claim rates over time by industry") +
  theme_light(base_size=14) +
  theme(legend.position = "bottom")
# using a flatline (average of past 3 years) to predict the claims rate
# the graph seems to suggest that Property and Business Service is decreasing but unsure if this will continue in the future. using the flatline might be overestimating.

predict_claim_rate <- function(industry) {
  # input an industry and then output an average claim_rate in 2026
  # regression doesn't seem suitable but an average of the 3 years seems appropiate
  claims_table %>% 
    filter(Industry == industry) %>% 
    pull(claim_rate) %>% 
    mean() %>% 
    return()
}

# NOT COMPLETE AND DODGY RN Average medical cost per claim by industry ----
  # possibly using survival analysis technique? like KME or whatever
costs_table <- data %>% 
  select(Industry, `Company ID`, `2023` = `2023 medical costs`, `2024` = `2024 medical costs`, `2025` = `2025 medical costs`) %>% 
  pivot_longer(cols = 3:5, names_to = "year", values_to = "costs") %>%
  mutate(year = as.numeric(year)) %>% 
  group_by(Industry, year) %>% 
  summarise(costs = sum(costs)) %>% 
  full_join(claims_table) %>% 
  group_by(Industry, year) %>% 
  mutate(avg_costs = costs / claims) %>% 
  select(Industry, year, costs, claims, avg_costs)

costs_table %>% 
  ggplot(aes(x=year, y=avg_costs, colour = Industry)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average medical costs over time by industry",
    x = "Year",
    y = "Average medical costs"
  ) +
  theme_light(base_size = 14) +
  theme(legend.position = "bottom")

predict_costs <- function(industry) {
  # input an industry and then output an average medical costs per claim in 2026
  # THIS IS EXTREMELY PLACE HOLDER RN UNTIL WE ACTUALLY MODEL THE CLAIMS COSTS GOODLY
  costs_table %>% 
    filter(Industry == industry) %>% 
    pull(avg_costs) %>% 
    mean() %>% 
    return()
}

# Make the predictions and add to the original data to graph it ----
  # add in the appropriate regression line too (as a dashed line of the same colour and no confidence band - too messy)

employees_2026 <- c(
  predict_employees("Accommodation and Hospitality"),
  predict_employees("Government Administration"),
  predict_employees("Fishing and Agriculture"),
  predict_employees("Education"),
  predict_employees("Health and Community"),
  predict_employees("Property and Business Service")
)

avg_wages_2026 <- c(
  predict_wage("Accommodation and Hospitality"),
  predict_wage("Government Administration"),
  predict_wage("Fishing and Agriculture"),
  predict_wage("Education"),
  predict_wage("Health and Community"),
  predict_wage("Property and Business Service")
)

claim_rate_2026 <- c(
  predict_claim_rate("Accommodation and Hospitality"),
  predict_claim_rate("Government Administration"),
  predict_claim_rate("Fishing and Agriculture"),
  predict_claim_rate("Education"),
  predict_claim_rate("Health and Community"),
  predict_claim_rate("Property and Business Service") 
)

avg_costs_2026 <- c(
  predict_costs("Accommodation and Hospitality"),
  predict_costs("Government Administration"),
  predict_costs("Fishing and Agriculture"),
  predict_costs("Education"),
  predict_costs("Health and Community"),
  predict_costs("Property and Business Service")
)

predicted_data <- employees_table %>% 
  full_join(wages_table) %>% 
  full_join(claims_table) %>% 
  full_join(costs_table) %>% 
  select(Industry, year, employees, avg_wages, claim_rate, avg_costs) %>% 
  ungroup() %>% 
  add_row(Industry = c("Accommodation and Hospitality",
                       "Government Administration",
                       "Fishing and Agriculture",
                       "Education",
                       "Health and Community",
                       "Property and Business Service"),
          year = 2026,
          employees = employees_2026,
          avg_wages = avg_wages_2026,
          claim_rate = claim_rate_2026,
          avg_costs = avg_costs_2026)

plot0 <- predicted_data %>% 
  ggplot(aes(x=year, y=employees, colour = Industry)) +
  geom_line() +
  geom_point() + 
  labs(
    title = "Employees over time by industry",
    x = "Year",
    y = "Employees"   
  ) +
  theme_light()

plot1 <- predicted_data %>% 
  ggplot(aes(x=year, y=avg_wages, colour = Industry)) +
  geom_line() +
  geom_point() + 
  labs(
    title = "avg_wages over time by industry",
    x = "Year",
    y = "avg_wages"   
  ) +
  theme_light()

plot2 <- predicted_data %>% 
  ggplot(aes(x=year, y=claim_rate, colour = Industry)) +
  geom_line() +
  geom_point() + 
  labs(
    title = "claim_rate over time by industry",
    x = "Year",
    y = "claim_rate"   
  ) +
  theme_light()

plot3 <- predicted_data %>% 
  ggplot(aes(x=year, y=avg_costs, colour = Industry)) +
  geom_line() +
  geom_point() + 
  labs(
    title = "avg_costs over time by industry",
    subtitle = "cant fully justify the modelling yet",
    x = "Year",
    y = "avg_costs"   
  ) +
  theme_light()

plot0 + plot1 + plot2 + plot3 + plot_layout(guides = "collect")

# Find the amount of moneys needed ----
total_medical_costs <- (claim_rate_2026 * employees_2026 * avg_costs_2026) %>% sum()
total_salary_compensation <- (0.7 * 4/52 * claim_rate_2026 * employees_2026 * avg_wages_2026) %>% sum()
total_costs <- total_medical_costs + total_salary_compensation + 2400000
