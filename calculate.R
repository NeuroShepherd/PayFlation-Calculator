

calculate_inflation_adjusted_return <- function(date1, date2, salary1, salary2) {

  date1 <- lubridate::as_date(date1)
  date2 <- lubridate::as_date(date2)

  year1 <- lubridate::year(date1);   lubridate::year2 <- year(date2)
  month1 <- lubridate::month(date1); lubridate::month2 <- month(date2)
  day1 <- lubridate::day(date1);     lubridate::day2 <- day(date1)


   if(year1 == year2) {
     within_year_days <- difftime(date2,
                                  date1,
                                  units = "days") %>% as.numeric()
   } else {
     within_year_days <- NA_real_
   }


  year1_elapsed_days <- difftime(lubridate::as_date(paste(year1,"-12-31")),
                                 date1,
                                 units = "days") %>% as.numeric()

  current_year_user_elapsed_days <- difftime(date2,
                                             lubridate::as_date(paste(year2,"-01-01")),
                                             units = "days") %>% as.numeric()

  cumulative_compounding_inflation_factor <- calculator_net_inflation_table_cleaned %>%
    dplyr::filter(Year >= year1, Year <= year2) %>%
    dplyr::mutate(user_days = case_when((year1 != Year) & (year2 != Year) & (year1 != year2) ~ elapsed_days_in_year,
                                 (year1 == Year) & (year1 != year2) ~ year1_elapsed_days,
                                 (year2 == year(Sys.Date())) & (year1 != Year) ~ current_year_user_elapsed_days,
                                 (year2 == max(Year)) ~ current_year_user_elapsed_days)
           ) %>%
    dplyr::mutate(yearly_mult_factors = (1+relative_daily_inflation_rate_of_year)^user_days) %>%
    dplyr::pull(yearly_mult_factors) %>%
    purrr::reduce(`*`)


  # inflation adjusted return = [(1+return)/(1+inflation rate)] - 1
  # e.g. salary doubles, and inflation is 35% over the time period
  # (1+1)/(1+.35)

  return = (salary2-salary1)/salary1

  adjusted_return = {((1+return)/(cumulative_compounding_inflation_factor)) - 1} %>%
    round(3)*100

  glue::glue("Your inflation-adjusted return from {date1} to {date2} is {adjusted_return}%")

}



# as_date("2014-10-12")

oinking_fxn(("2005-01-01"), ("2020-01-01"), 1, 2)




