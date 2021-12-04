

calculate_inflation_adjusted_return <- function(date1, date2, salary1, salary2) {


  date1 <- lubridate::as_date(date1)
  date2 <- lubridate::as_date(date2)

  year1 <- lubridate::year(date1);   year2 <- lubridate::year(date2)
  month1 <- lubridate::month(date1); month2 <- lubridate::month(date2)
  day1 <- lubridate::day(date1);     day2 <- lubridate::day(date1)


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

  cumulative_compounding_info <- calculator_net_inflation_table_cleaned %>%
    dplyr::filter(Year >= year1, Year <= year2) %>%
    dplyr::mutate(user_days = case_when((year1 != Year) & (year2 != Year) & (year1 != year2) ~ elapsed_days_in_year,
                                 (year1 == Year) & (year1 != year2) ~ year1_elapsed_days,
                                 (year2 == year(Sys.Date())) & (year1 != Year) ~ current_year_user_elapsed_days,
                                 (year2 == max(Year)) ~ current_year_user_elapsed_days)
           ) %>%
    dplyr::mutate(yearly_mult_factors = num((1+relative_daily_inflation_rate_of_year)^user_days, digits = 6) %>%
                    as.numeric()
                  )


  cumulative_inflationary_mult_factor <- cumulative_compounding_info %>%
    dplyr::pull(yearly_mult_factors) %>%
    purrr::reduce(`*`)

  # inflation adjusted return = [(1+return)/(1+inflation rate)] - 1
  # e.g. salary doubles, and inflation is 35% over the time period
  # (1+1)/(1+.35)

  nominal_return = (salary2-salary1)/salary1

  inflation_adjusted_return_percentage = {((1+nominal_return)/(cumulative_inflationary_mult_factor)) - 1} %>%
    round(3)*100

  text_output <- glue::glue("Cumulative inflation of the US dollar from {date1} to {date2} is {round(cumulative_inflationary_mult_factor-1, 3)*100 }%.
  In this same time frame, your salary increased from ${salary1} to ${salary2} which is a nominal {round(nominal_return,3)*100}% increase.
  This means your inflation-adjusted return over this period is {inflation_adjusted_return_percentage}%. In other terms, you have received
  a real raise of {inflation_adjusted_return_percentage}% relative to the increased cost of goods over time.")

  compounding_table <- cumulative_compounding_info %>%
    select(Year, `Annual Inflation` = year_to_year_inflation, `Compounding Days` = user_days,
           `Daily Inflation Rate` = relative_daily_inflation_rate_of_year,
           `Effective Inflation` = yearly_mult_factors)



  return(list(nominal_return = nominal_return+1,
              inflation_adjusted_return_percentage = inflation_adjusted_return_percentage,
              text_output = text_output,
              compounding_table = compounding_table,
              cumulative_inflationary_mult_factor = cumulative_inflationary_mult_factor)

         )

}



# as_date("2014-10-12")

calculate_inflation_adjusted_return(lubridate::as_date("2016-01-01"), lubridate::as_date("2021-11-24"), 2300, 2843)
#   magrittr::extract2("compounding_table") %>% print() %>% class()



