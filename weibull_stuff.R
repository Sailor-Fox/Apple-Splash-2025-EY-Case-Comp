library(fitdistrplus)
library(tidyverse)
library(readxl)
data <- read_excel("2025 Case Comp Data CLEAN.xlsx")
# making claims cost be per claim ----
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
  mutate(avg_costs = costs / claims) %>% 
  dplyr::select(`Company ID`, Industry, year, costs, claims, avg_costs)

by_company_graphs <- function(industry) {
  industry_data <- costs_table_company %>% 
    filter(Industry == industry)
  fit.in <- fitdist(log(industry_data$avg_costs), "weibull")
  
  par(mfrow = c(2,2))
  denscomp(fit.in)
  qqcomp(fit.in)
  cdfcomp(fit.in)
  ppcomp(fit.in)
}

# original (claims cost is per company over a year) ----
byindustrygraphs <- function(industry) {
  industry_data <- data %>% 
    dplyr::filter(Industry == industry)
  industry_claims <- industry_data %>% 
    dplyr::select(13:15) %>% 
    pivot_longer(cols = starts_with("20"),
                 names_to = "Name",
                 values_to = "Cost") %>% 
    dplyr::select(-Name) %>% 
    unlist(use.names = FALSE)
  
  fit.in <- fitdist(log(industry_claims), "weibull")
  
  par(mfrow = c(2,2))
  denscomp(fit.in)
  qqcomp(fit.in)
  cdfcomp(fit.in)
  ppcomp(fit.in)
  
  return(fit.in$estimate)
}

param1 <- byindustrygraphs(unique(data$Industry)[1]) #Acc and Hosp
param2 <- byindustrygraphs(unique(data$Industry)[2]) #Govt Admin
param3 <- byindustrygraphs(unique(data$Industry)[3]) #Fish Ag
param4 <- byindustrygraphs(unique(data$Industry)[4]) #Edu
param5 <- byindustrygraphs(unique(data$Industry)[5]) #Health
param6 <- byindustrygraphs(unique(data$Industry)[6]) #Prop and Bus

percentile99 <- function(params) {
  qweibull(0.99, shape = params[1], scale = params[2])
}

claims99 <- lapply(list(param1, param2, param3, param4, param5, param6), 
                   percentile99) %>% 
  unlist() %>% exp()

# Estimate expected severity by industry
claim_rate_2026*employees_2026*claims99
# = no. of claims in industry per employee / no. of employees * no. of employees * cost of claims at a company total
# = no. of claims in industry * cost of all claims from a company in a year
# 611,615,960    144,753,582    20,786,810,440   373,806,300   1,292,792,844    153,345,780 

claims99 * c(45, 11, 51, 22, 36, 37)
# = cost of all claims from a company in a year * no. of companies in industry
# 27,562,273    5,561,869   459,638,313   13,840,908    33,677,775    20,950,029
# if this is correct then we didn't need to model claim rate so take that out of the slides

costs_table_company %>% filter(Industry == "Fishing and Agriculture") %>% pull(costs) %>% sum()
# 217,623,375

# Estimated Wages Paid in 2026
avg_wages_2026*employees_2026