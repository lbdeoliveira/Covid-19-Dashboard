## Libraries:
library(readr)
library(dplyr)
library(tidyr)

## Functions ------------------------------------

format.states <- function(states.tbl) {
        
        library(dplyr)
        library(readr)
        
        # States Population
        states.pop <- read_csv("./data/state_population2019.csv")
        states.tbl <- left_join(states.tbl, states.pop[c("FIPS", "POPESTIMATE2019")], by = c("fips" = "FIPS"))
        
        # Get Daily Mortality Rates
        states.tbl$mortality <- (as.numeric(states.tbl$deaths) / as.numeric(states.tbl$cases))
        
        # Get New Cases & Deaths
        states.tbl <- states.tbl %>%
                group_by(state) %>%
                mutate(new_cases = cases - lag(cases)) %>%
                ungroup
        
        states.tbl <- states.tbl %>%
                group_by(state) %>%
                mutate(new_deaths = deaths - lag(deaths)) %>%
                ungroup
        
        # Get Growth Rates
        states.tbl <- states.tbl %>%
                group_by(state) %>%
                mutate(per_growth_cases = as.numeric(new_cases) / as.numeric(lag(cases)) * 100) %>%
                ungroup
        
        states.tbl <- states.tbl %>%
                group_by(state) %>%
                mutate(per_growth_deaths = as.numeric(new_deaths) / as.numeric(lag(deaths)) * 100) %>%
                ungroup
        
        # Adjust to Population
        states.tbl$cases_per_mil <- as.numeric(states.tbl$cases) / as.numeric(states.tbl$POPESTIMATE2019) *1000000
        states.tbl$deaths_per_mil <- as.numeric(states.tbl$deaths) / as.numeric(states.tbl$POPESTIMATE2019) *1000000
        states.tbl$new_cases_per_mil <- as.numeric(states.tbl$new_cases) / as.numeric(states.tbl$POPESTIMATE2019) * 1000000
        states.tbl$new_deaths_per_mil <- as.numeric(states.tbl$new_deaths) / as.numeric(states.tbl$POPESTIMATE2019) * 1000000
        
        return(states.tbl)
}

format.counties <- function(counties.tbl) {
        
        library(dplyr)
        library(readr)
        
        # Counties Population
        counties.pop <- read_csv("./data/county_population2019.csv")
        counties.tbl <- left_join(counties.tbl, counties.pop[c("FIPS", "POPESTIMATE2019")], by = c("fips" = "FIPS"))
        
        # Get Daily Mortality Rates
        counties.tbl$mortality <- (as.numeric(counties.tbl$deaths) / as.numeric(counties.tbl$cases))
        
        # Get New Cases & Deaths
        counties.tbl <- counties.tbl %>%
                group_by(fips) %>%
                mutate(new_cases = cases - lag(cases)) %>%
                ungroup
        
        counties.tbl <- counties.tbl %>%
                group_by(fips) %>%
                mutate(new_deaths = deaths - lag(deaths)) %>%
                ungroup
        
        # Get Growth Rates
        counties.tbl <- counties.tbl %>%
                group_by(fips) %>%
                mutate(per_growth_cases = as.numeric(new_cases) / as.numeric(lag(cases))) %>%
                ungroup
        
        counties.tbl <- counties.tbl %>%
                group_by(fips) %>%
                mutate(per_growth_deaths = as.numeric(new_deaths) / as.numeric(lag(deaths))) %>%
                ungroup
        
        # Adjust to Population
        counties.tbl$cases_per_mil <- as.numeric(counties.tbl$cases) / as.numeric(counties.tbl$POPESTIMATE2019) *1000000
        counties.tbl$deaths_per_mil <- as.numeric(counties.tbl$deaths) / as.numeric(counties.tbl$POPESTIMATE2019) *1000000
        counties.tbl$new_cases_per_mil <- as.numeric(counties.tbl$new_cases) / as.numeric(counties.tbl$POPESTIMATE2019) * 1000000
        counties.tbl$new_deaths_per_mil <- as.numeric(counties.tbl$new_deaths) / as.numeric(counties.tbl$POPESTIMATE2019) * 1000000
        
        return(counties.tbl)
        
}

format.us <- function(us.tbl) {
        
        # Get Daily Mortality Rates
        us.tbl$mortality <- (as.numeric(us.tbl$deaths) / as.numeric(us.tbl$cases))
        
        # Get New Cases & Deaths
        us.tbl <- us.tbl %>%
                mutate(new_cases = cases - lag(cases))
        
        us.tbl <- us.tbl %>%
                mutate(new_deaths = deaths - lag(deaths))
        
        # Get Growth Rates
        us.tbl <- us.tbl %>%
                mutate(per_growth_cases = as.numeric(new_cases) / as.numeric(lag(cases)))
        
        us.tbl <- us.tbl %>%
                mutate(per_growth_deaths = as.numeric(new_deaths) / as.numeric(lag(deaths)))
        
        us.tbl <- tidyr::gather(us.tbl, "metric", "val", -date)
        
        return(us.tbl)
        
}

