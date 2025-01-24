# This script prepares discount rates for the coal cross over project
library(readxl)
#library(XLConnect)
library(openxlsx)
library(tidyverse)
library(countrycode)
library(janitor)
library(PerformanceAnalytics)

discount_location <- "https://www.stern.nyu.edu/~adamodar/pc/datasets/ctryprem.xlsx"

# load temporary excel spreadsheet
temp  <-  tempfile(fileext = ".xlsx")
download.file(discount_location, destfile=temp, mode='wb')

# most recent estimate of the risk premia
dr_file <- read.xlsx(temp, sheet= "PRS Worksheet") |>
        janitor::clean_names()|>
        dplyr::select("country",
                      "total_equity_risk_premium",
                      "rating_based_default_spread",
                      "final_erp",
                      "tax_rate",
                      "crp") |>
        as_tibble() |>
        dplyr::mutate(country_id = countrycode::countryname(country, 'iso3c'),
                      across(where(is.numeric), ~ .x * 100))|>
                      dplyr::filter(!is.na(country))

# risk free rates
treas_10y <- quantmod::getSymbols("DGS10", src="FRED", auto.assign=FALSE)|>
        as.data.frame()|>
        tibble::rownames_to_column(var = "date")|>
        tibble::as_tibble()|>
        dplyr::filter(date == max(date,na.rm = T))

# merge our datasets
dr_rfr_table <- dr_file |> mutate(rfr_us_10y = treas_10y |>
                                          pull(DGS10),
                                date = treas_10y |>
                                        pull(date))

# construct country level discount rates
dr_calculated <- dr_rfr_table|>
        mutate(cost_of_debt = rfr_us_10y + rating_based_default_spread,
               cost_of_equity = rfr_us_10y + total_equity_risk_premium,
               cost_of_capital = 0.2 * cost_of_equity + 0.8*cost_of_debt) # assumed 4x D2E

write.csv(dr_calculated, "discount_rates_country.csv")
