library(tidyverse)
data <- readxl::read_excel("2025 Case Comp Data CLEAN.xlsx")
data_cred <- data %>% 
  dplyr::select(
    Industry,
    `Number of employees 2023`,
    `Number of employees 2024`,
    `Number of employees 2025`,
    `2023 medical costs`,
    `2024 medical costs`,
    `2025 medical costs`
  ) %>% 
  pivot_longer(
    cols = starts_with("20"),
    names_to = c("Year", "Type"),
    names_sep = c(4),
    values_to = "Cost"
  ) %>% 
  select(-Type)

# ORIGINAL ----
var_industry <- function() {
  data_cred %>%  
    group_by(Year, Industry) %>%  
    summarise(Ind_Var = var(Cost), .groups = "drop")
}

data_cred <- data_cred %>%
  left_join(var_industry(), by = c("Year", "Industry"))

# MINE ----
mean_Ind_Var <- data_cred$Ind_Var %>% 
  unique() %>% 
  mean()
data_cred <- data_cred %>%  
  mutate(Ind_Var = mean_Ind_Var)

# ----
var_industry_total <- function(year) {
  data_cred %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(Cost) %>% 
    var() %>% 
    unname()
}

data_cred <- data_cred %>% 
  mutate(Credibility = Ind_Var/(map_dbl(Year, var_industry_total)+Ind_Var))

mean_industry <- function() {
  data_cred %>%  
    group_by(Year, Industry) %>%  
    summarise(Ind_Mean = mean(Cost), .groups = "drop")
}

data_cred <- data_cred %>%
  left_join(mean_industry(), by = c("Year", "Industry"))

mean_industry_total <- function(year) {
  data_cred %>% 
    dplyr::filter(Year == year) %>% 
    dplyr::select(Cost) %>% 
    unlist(use.names = FALSE) %>% 
    mean()
}

data_cred <- data_cred %>% 
  mutate(Premium = Credibility*Ind_Mean + (1-Credibility)*(map_dbl(Year, mean_industry_total)))

data_cred <- data_cred %>%
  mutate(Employees = case_when(
    Year == "2023" ~ `Number of employees 2023`,
    Year == "2024" ~ `Number of employees 2024`,
    Year == "2025" ~ `Number of employees 2025`
  )) %>%
  select(-`Number of employees 2023`, -`Number of employees 2024`, -`Number of employees 2025`) %>% 
  mutate(Employee_Premium = Premium/Employees)

# MINE ----
data_cred$Premium %>% sum()
