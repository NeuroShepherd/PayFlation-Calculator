



calculator_net_inflation_table_cleaned <- calculator_net_inflation_table_scraped %>%
  # Create column that uses Annual rate if available, and will use the most recent
  # month available if Annual is not available.
  mutate(year_to_year_inflation = case_when(!is.na(Annual) ~ Annual,
                                            is.na(Annual) ~
                                              select(.,Year,Jan:Dec) %>%
                                              pivot_longer(-Year) %>%
                                              group_by(Year) %>%
                                              dplyr::filter(any(is.na(value))) %>%
                                              ungroup() %>%
                                              mutate(month_number = 1:12) %>%
                                              dplyr::filter(!is.na(value)) %>% dplyr::filter(month_number == max(month_number)) %>%
                                              pull(value)
                                            )
         ) %>%
  # turn percentages into rates
  mutate(across(Jan:year_to_year_inflation, ~.x/100)) %>%
  # calculate number of days in a given year. will always be one of the following:
  # A) 365 days, B) 366 days (leap years), or C) the number of days that have passed in the current year
  mutate(elapsed_days_in_year = case_when(Year == year(Sys.Date()) ~ difftime(Sys.Date(),
                                                                              as_date(paste(Year,"-01-01")),
                                                                              units = "days" ),
                                          Year != year(Sys.Date()) ~ difftime(as_date(paste(dplyr::lag(Year),"-01-01")),
                                                                              as_date(paste(Year,"-01-01")),
                                                                              units = "days" )) %>% as.numeric()
  ) %>%
  # calculate the relative daily inflation rate for each year given the yearly inflation rate
  # and the number of days in each year
  mutate(relative_daily_inflation_rate_of_year = (pracma::nthroot(1+year_to_year_inflation, elapsed_days_in_year) - 1))




calculator_net_inflation_table_cleaned_default <- calculator_net_inflation_table_cleaned %>%
  mutate(yearly_mult_factors = num((1+relative_daily_inflation_rate_of_year)^elapsed_days_in_year, digits = 6) %>%
           as.numeric()) %>%
  select(Year, `Annual Inflation` = year_to_year_inflation,
         `Elapsed Days in Year` = elapsed_days_in_year,
         `Daily Inflation Rate` = relative_daily_inflation_rate_of_year,
         `Effective Inflation` = yearly_mult_factors)

# For current year when annualized inflation rate is missing, need to just fill in with otherwise most recent
# monthly value

# usa_inflation_table_cleaned %>%
#   select(Year,Jan:Dec) %>%
#   pivot_longer(-Year) %>%
#   group_by(Year) %>%
#   dplyr::filter(any(is.na(value))) %>%
#   ungroup() %>%
#   mutate(month_number = 1:12) %>%
#   dplyr::filter(!is.na(value)) %>% dplyr::filter(month_number == max(month_number)) %>%
#   pull(value)



