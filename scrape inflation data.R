

library(rvest)
library(tidyverse)
library(lubridate)

usa_inflation_url <- "https://www.usinflationcalculator.com/inflation/historical-inflation-rates/"

usa_inflation_page <- read_html(usa_inflation_url)

usa_inflation_table_scraped <- usa_inflation_page %>%
  html_element(xpath = "/html/body/div[1]/div[2]/div[1]/div[1]/div/article/div/div[1]/table") %>%
  html_table() %>%
  arrange(desc(Year)) %>%
  mutate(year_to_year_inflation = case_when(!is.na(Ave) ~ Ave,
                                            is.na(Ave) ~
                                              select(.,Year,Jan:Dec) %>%
                                              pivot_longer(-Year) %>%
                                              group_by(Year) %>%
                                              dplyr::filter(any(is.na(value))) %>%
                                              ungroup() %>%
                                              mutate(month_number = 1:12) %>%
                                              dplyr::filter(!is.na(value)) %>% dplyr::filter(month_number == max(month_number)) %>%
                                              pull(value)

                                            )
         )

usa_inflation_table_cleaned <- usa_inflation_table_scraped %>%
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



