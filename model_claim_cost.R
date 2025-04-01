library(tidyverse)
data <- readxl::read_excel("2025 Case Comp Data CLEAN.xlsx")

claims_table2 <- data %>% 
  dplyr::select(`Company ID`, Industry, `2023` = `2023 number of claims`, `2024` = `2024 number of claims`, `2025` = `2025 number of claims`) %>% 
  dplyr::pivot_longer(cols = 3:5, names_to = "year", values_to = "claims") %>% 
  dplyr::mutate(year = as.numeric(year))

avg_costs_table <- data %>% 
  dplyr::select(`Company ID`,
         Industry,
         `2023` = `2023 medical costs`,
         `2024` = `2024 medical costs`,
         `2025` = `2025 medical costs`) %>% 
  dplyr::pivot_longer(cols = 3:5, names_to = "year", values_to = "costs") %>% 
  dplyr::mutate(year = base::as.numeric(year)) %>% 
  dplyr::full_join(claims_table2) %>% 
  dplyr::mutate(avg_costs = costs / claims,
         industry_group = dplyr::if_else(Industry == "Fishing and Agriculture", "Fishing and Agriculture", "Other industries"))

fishing_threshold <- avg_costs_table %>% 
  dplyr::filter(industry_group == "Fishing and Agriculture") %>% 
  dplyr::pull(avg_costs) %>% 
  base::sort(decreasing = TRUE) %>% 
  dplyr::nth(11) # from the graphs we set the 11th highest avg cost be the lower bound of the outliers for fishing and agriculture

other_threshold <- avg_costs_table %>% 
  dplyr::filter(industry_group == "Other industries") %>% 
  dplyr::pull(avg_costs) %>% 
  base::sort(decreasing = TRUE) %>% 
  dplyr::nth(18) # from the graph set the 18th highest avg cost be our outlier threshold

avg_costs_table <- avg_costs_table %>% 
  dplyr::mutate(outlier = dplyr::if_else(industry_group == "Fishing and Agriculture",
                                         dplyr::if_else(avg_costs >= fishing_threshold, TRUE, FALSE),
                                         dplyr::if_else(avg_costs >= other_threshold, TRUE, FALSE)))

# look at the average medical cost per claim for each company over the 3 years
# from this plot it seems reasonable to model all industries except fishing and agriculture together
# we also need to model the outliers 
avg_costs_table %>% 
  ggplot2::ggplot(aes(x = year, y = avg_costs, group = `Company ID`)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~Industry) +
  ggplot2::theme_light()

avg_costs_table %>% 
  ggplot2::ggplot(aes(x = year, y = avg_costs, group = `Company ID`)) + 
  ggplot2::geom_line() + 
  ggplot2::geom_point(aes(colour = outlier)) +
  ggplot2::labs(title = "Average medical costs per claim split by F&A industry or not") +
  ggplot2::facet_wrap(~industry_group) +
  ggplot2::theme_light()

# deal with fishing and agriculture first
f_and_g <- avg_costs_table %>% 
  dplyr::filter(industry_group == "Fishing and Agriculture")
dplyr::count(f_and_g, outlier == TRUE) # 11/142 are outliers
fg_outlier_rate <- 11/(142+11)

# now deal with the rest
others <- avg_costs_table %>% 
  dplyr::filter(industry_group == "Other industries")
dplyr::count(others, outlier == TRUE) # 18/435 are outliers
others_outlier_rate <- 18/(435+18)

# combine for final prediction
predict_cost <- function(industry) {
  if (industry == "Fishing and Agriculture") {
    fg_outlier_predict <- f_and_g %>% 
      dplyr::filter(outlier == TRUE) %>% 
      dplyr::pull(avg_costs) %>% 
      base::mean()
    fg_standard_predict <- f_and_g %>% 
      dplyr::filter(outlier == FALSE) %>% 
      dplyr::pull(avg_costs) %>% 
      base::mean()
    return (fg_outlier_rate * fg_outlier_predict + (1 - fg_outlier_rate) * fg_standard_predict)
  } else {
    others_outlier_predict <- avg_costs_table %>% 
      dplyr::filter(industry_group == "Other industries",
                    outlier == TRUE) %>% 
      dplyr::pull(avg_costs) %>% 
      base::mean()
    others_standard_predict <- avg_costs_table %>% 
      dplyr::filter(industry_group == "Other industries",
                    outlier == FALSE) %>% 
      dplyr::pull(avg_costs) %>% 
      base::mean()
    return (others_outlier_rate * others_outlier_predict + (1 - others_outlier_rate) * others_standard_predict)
  }
}

# this only gives a point estimate at the moment
# the number of outliers is a binomial random variable so can find the variance via that (just like F(t) in ACST3058: Survival Models when doing KME)

predict_cost("Education")
predict_cost("Accommodation and Hospitality")
predict_cost("Fishing and Agriculture")
predict_cost("Government Administration")
predict_cost("Health and Community")
predict_cost("Property and Business Service")
