

library(rvest)
library(tidyverse)
library(lubridate)



# Scrape main data! This site was chosen because it has inflation info to
# the second decimal place. Downside is that it doesn't not have monthly
# data for the current year
calculator_net_inflation_url <- "https://www.calculator.net/inflation-calculator.html"
calculator_net_inflation_page <- read_html(calculator_net_inflation_url)

calculator_net_inflation_table_scraped <- calculator_net_inflation_page %>%
  html_element(xpath = "/html/body/div[3]/div[1]/table[4]") %>%
  html_table(header = T) %>%
  mutate(across(Jan:Annual, ~as.numeric(str_remove_all(.x, "%"))) )


# Scrape auxiliary data! This site has the monthly data for the current year,
# but it only has data to one decimal place so use of this data should be
# kept to a minimum
us_inflation_calculator_url <- "https://www.usinflationcalculator.com/inflation/historical-inflation-rates/"
us_inflation_calculator_page <- read_html(us_inflation_calculator_url)

us_inflation_calculator_table_scraped <- us_inflation_calculator_page %>%
  html_element(xpath = "/html/body/div[1]/div[2]/div[1]/div[1]/div/article/div/div[1]/table") %>%
  html_table() %>%
  dplyr::filter(Year == max(Year)) %>%
  rename(Annual = Ave)




# If the Calculator.net table is not yet updated to the same year as the
# USInflationCalculator.com site, then use the data available in the latter site
# (e.g. bind the latest year from the USInflationCalc to Calculator.net data)

if (max(pull(calculator_net_inflation_table_scraped,Year)) == pull(us_inflation_calculator_table_scraped,Year)) {
  calculator_net_inflation_table_scraped <- calculator_net_inflation_table_scraped
} else {
  calculator_net_inflation_table_scraped %<>%
    bind_rows(us_inflation_calculator_table_scraped) %>%
    arrange(desc(Year))
}


rm(calculator_net_inflation_url,calculator_net_inflation_page,
   us_inflation_calculator_url,us_inflation_calculator_page,us_inflation_calculator_table_scraped)

