
library(tidyverse)
library(lubridate)

# Ae^(rt) = B
# e^(rt) = B/A
# ln(rt) = ln(B/A)
# ln(r) + ln(t) = ln(B/A)
# ln(r) = ln(B/A) - ln(t)
# r = e^( ln(B/A) - ln(t) )


# Example: 4%/year OR ?%/day

# 1*e^(r*365) = 1.04
#
# ln(r*365) = ln(1.04)
# ln(r) + ln(365) = ln(1.04)
# ln(r) = ln(1.04) - ln(365)
# r = e^( ln(1.04) - ln(365) )
# exp(log(1.04) - log(365))


# What the above illustrates:
# In order for the calculator to function properly, people must be able to enter
# particular start and end dates for when they had X salary and Y salary. These
# dates will, obviously, not always be complete years (e.g. Jan 1 XXXX to Dec 31 XXXX)
# so I need to have daily inflation rates over a year


us_inflation_data_sample <- tibble::tibble(year = 2000:2021,
                                           yearly_inflation = sample(100:250, 22)/100
                                           ) %>%
  mutate(days_in_year = case_when(year == year(Sys.Date()) ~ difftime(Sys.Date(), as_date(paste(year,"-01-01")), units = "days" ),
                          year != year(Sys.Date()) ~ difftime(as_date(paste(lead(year),"-01-01")), as_date(paste(year,"-01-01")), units = "days" )) %>%
                              as.numeric(),
         daily_inflation_of_year = exp(log(yearly_inflation) - log(days_in_year))
                        ) %>%
  View()










