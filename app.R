###################################################################################################
#################                    COVID DASHBOARD APP                          #################
###################################################################################################

source("./dashboard_helper.R")

## REFERENCES: -------------------------

# Download .shp file URL: 
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

## LIBRARIES: --------------------------
library(shiny)
library(readr)
library(dplyr)
library(DT)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(plotly)
library(stringr)

## IMPORT DATA: ----------------------------

# States
states_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
states.tbl <- read_csv(url(states_url))
states.tbl$date <- as.Date(states.tbl$date)

# Counties
counties_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
counties.tbl <- read_csv(url(counties_url))
counties.tbl$date <- as.Date(counties.tbl$date)

# US Data
us_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
us.tbl <- read_csv(url(us_url))
us.tbl$date <- as.Date(us.tbl$date)

## FORMAT DATA: ----------------------------------------
states.tbl <- format.states(states.tbl)
counties.tbl <- format.counties(counties.tbl)
us.tbl <- format.us(us.tbl)

## PREP MAP DATA: --------------------------------------
counties_map <- filter(counties.tbl, counties.tbl$date == max(counties.tbl$date))
mymap <- st_read("./data/cb_2018_us_county_500k/cb_2018_us_county_500k.shp", stringsAsFactors = FALSE)
map_and_data <- inner_join(mymap, counties_map, by = c("GEOID" = "fips"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("USA COVID-19 Dashboard"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
        # About Tab ---------------------------------------------------------
        tabPanel("About",
                 br(),
                 h4("This dashboard visualizes the spread and fatality of COVID-19 in the USA through tables, maps, and graphs."),
                 br(),
                 h2("Coronavirus, coronavirus, coronavirus..."),
                 br(),
                 p("Tired of hearing people on the news demagouging a serious public health issue? 
                 I created this dashboard to give myself, my family, co-workers, and whoever you may be a resource to track the spread of COVID-19 in the US without the noise.
                 Track daily trends and counts for the entire US, compare trends between states, and counties in your chosen state.
                 Play around with the map (it may take a bit to load)."),
                 br(),
                 a(href = "https://github.com/lbdeoliveira/Covid-19-Dashboard", "Access the Source Code!"),
                 br(), br(),
                 p("State and county COVID-19 data are pulled from the ", a(href = "https://github.com/nytimes/covid-19-data", "New York Times Github.")),
                 p("State and county population & .shp data are pulled from the ", a(href = "https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html", "US Census Bureau.")),
                 br(),
                 h6("Developed by Lucas De Oliveira (a man with too much time on his hands).")
                 ),
        
        # US Summary Tab ---------------------------------------------
        tabPanel("US Summary",
                 fluidRow(
                     column(4,
                            h3("Summary Statistics"),
                            fluidRow(
                                column(3, h5("US Cases: ")), 
                                column(3, h6(textOutput("us_cases")))
                            ),
                            fluidRow(
                                column(3, h5("US Deaths: ")),
                                column(3, h6(textOutput("us_deaths")))
                            ),
                            fluidRow(
                                column(3, h5("US Mortality: ")), 
                                column(3, h6(textOutput("us_mort")))
                            )
                     ),
                     column(8,
                            h3(paste0("Latest Update ", (max(us.tbl$date)))),
                            fluidRow(
                                column(2, h5("New Cases: ")), 
                                column(4, h6(textOutput("us_new_cases")))
                            ),
                            fluidRow(
                                column(2, h5("New Deaths: ")),
                                column(4, h6(textOutput("us_new_deaths")))
                            )
                     )
                 ),
                 fluidRow(
                     
                     column(6,
                        br(), br(),
                        h3("Total Cases and Deaths"),
                        br(), br(),
                        plotlyOutput("us_graph_total"),
                        br(), br(),
                        h3("New Cases and Deaths"),
                        br(), br(),
                        plotlyOutput("us_graph_new")
                     ),
                     
                     column(6,
                        br(), br(),
                        h3("Mortality Rate (%)"),
                        br(), br(),
                        plotlyOutput("us_graph_mort"),
                        br(), br(),
                        h3("Growth in Cases and Deaths (%)"),
                        br(), br(),
                        plotlyOutput("us_graph_growth")
                     ))
                 ),
        
        # US States Tab ------------------------------------------------------------
        tabPanel("US States",
                 fluidRow(
                     sidebarPanel( width = 3,
                         dateRangeInput("states_dates", "Select Date Range",
                                        min = min(states.tbl$date),
                                        max = max(states.tbl$date),
                                        start = max(states.tbl$date) - 60,
                                        end = max(states.tbl$date)),
                         checkboxGroupInput("states_selected",
                                            "Select States to Graph",
                                            choices = str_sort(unique(states.tbl$state)))),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Raw Data",
                                      fluidRow(
                                          h3("Total Cases", align = "center"),
                                          plotlyOutput("graph_state_cases", width = "100%"),
                                          h3("New Cases", align = "center"),
                                          plotlyOutput("graph_state_new_cases", width = "100%")),
                                      fluidRow(
                                          h3("Total Deaths", align = "center"),
                                          plotlyOutput("graph_state_deaths", width = "100%"),
                                          h3("New Deaths", align = "center"),
                                          plotlyOutput("graph_state_new_deaths", width = "100%"))
                                      ),
                             tabPanel("By Population",
                                      fluidRow(
                                          h3("Total Cases (per million people)", align = "center"),
                                          plotlyOutput("graph_state_cases_per_mil", width = "100%"),
                                          h3("New Cases (per million people)", align = "center"),
                                          plotlyOutput("graph_state_new_cases_per_mil", width = "100%")),
                                      fluidRow(
                                          h3("Total Deaths (per million people)", align = "center"),
                                          plotlyOutput("graph_state_deaths_per_mil", width = "100%"),
                                          h3("New Deaths (per million people)", align = "center"),
                                          plotlyOutput("graph_state_new_deaths_per_mil", width = "100%"))
                                      )
                         )
                     )
                 ),
                 br(),
                 DT::dataTableOutput("states_table")),
        
        # Counties Tab --------------------------------------------------------------
        tabPanel("US Counties", 
                 fluidRow(
                     column( 4,
                         selectInput("state", h3("Select State"),
                                     choices = sort(unique(counties.tbl[complete.cases(counties.tbl), ]$state)))),
                     column( 8,
                         h3("State Statistics"),
                         fluidRow(
                             column(2, h5("Total Cases: ")), 
                             column(6, h6(textOutput("state_cases")))),
                         fluidRow(
                             column(2, h5("Total Deaths: ")),
                             column(6, h6(textOutput("state_deaths")))),
                         fluidRow(
                             column(2, h5("Current COVID Mortality: ")), 
                             column(5, h6(textOutput("state_mort")))))
                     ),
                 br(),
                 fluidRow(
                     sidebarPanel( width = 3,
                        dateRangeInput("county_dates", "Select Date Range to Graph",
                                       min = min(counties.tbl$date),
                                       max = max(counties.tbl$date),
                                       start = max(counties.tbl$date) - 60,
                                       end = max(counties.tbl$date)),
                        uiOutput("select_counties")),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Raw Data",
                                      fluidRow(
                                          h3("Total Cases", align = "center"),
                                          plotlyOutput("graph_county_cases", width = "100%"),
                                          h3("New Cases", align = "center"),
                                          plotlyOutput("graph_county_new_cases", width = "100%")),
                                      fluidRow(
                                          h3("Total Deaths", align = "center"),
                                          plotlyOutput("graph_county_deaths", width = "100%"),
                                          h3("New Deaths", align = "center"),
                                          plotlyOutput("graph_county_new_deaths", width = "100%"))
                             ),
                             tabPanel("By Population",
                                      fluidRow(
                                          h3("Total Cases (per million people)", align = "center"),
                                          plotlyOutput("graph_county_cases_per_mil", width = "100%"),
                                          h3("New Cases (per million people)", align = "center"),
                                          plotlyOutput("graph_county_new_cases_per_mil", width = "100%")),
                                      fluidRow(
                                          h3("Total Deaths (per million people)", align = "center"),
                                          plotlyOutput("graph_county_deaths_per_mil", width = "100%"),
                                          h3("New Deaths (per million people)", align = "center"),
                                          plotlyOutput("graph_county_new_deaths_per_mil", width = "100%"))
                             )
                         )
                     )
                     ),
                 br(),
                 DT::dataTableOutput("counties_table")),
        
        # Map Tab ------------------------------------------------------------
        tabPanel("Map",
                 
                 selectInput("map_measure", h3("Select Measure"),
                             choices = c("Cases", "Cases (per mil)", "Deaths", "Deaths (per mil)", "Mortality Rate (%)")),
                 br(),
                 leafletOutput("map")
                 
        )
    )
)


# Server
server <- function(input, output) {

    measure_choices <- data.frame(choices = c("Cases", "Deaths", "Cases per mil", "Deaths per mil", "Mortality Rate"),
                                  metrics = c("cases", "deaths", "cases_per_mil", "deaths_per_mil", "mortality"))
    
    ## US Summary ----------------------------------
    
    ## First Fluid Row
    output$us_cases <- renderText({
        
        num_cases <- filter(us.tbl, us.tbl$metric == "cases", us.tbl$date == max(us.tbl$date))$val
        formatC(num_cases, format = "d", big.mark = ",")
        
    })
    
    output$us_deaths <- renderText({
        
        num_deaths <- filter(us.tbl, us.tbl$metric == "deaths", us.tbl$date == max(us.tbl$date))$val
        formatC(num_deaths, format = "d", big.mark = ",")
        
    })
    
    output$us_mort <- renderText({
        
        mort_rate <- filter(us.tbl, us.tbl$metric == "mortality", us.tbl$date == max(us.tbl$date))$val * 100
        paste0(round(mort_rate, digits = 2), "%")
        
    })
    
    output$us_new_cases <- renderText({
        
        num_cases <- filter(us.tbl, us.tbl$metric == "new_cases", us.tbl$date == max(us.tbl$date))$val
        growth_cases <- filter(us.tbl, us.tbl$metric == "per_growth_cases", us.tbl$date == max(us.tbl$date))$val * 100
        paste0(formatC(num_cases, format = "d", big.mark = ","), " (", round(growth_cases, digits = 2), "%)")
        
    })
    
    output$us_new_deaths <- renderText({
        
        num_deaths <- filter(us.tbl, us.tbl$metric == "new_deaths", us.tbl$date == max(us.tbl$date))$val
        growth_deaths <- filter(us.tbl, us.tbl$metric == "per_growth_deaths", us.tbl$date == max(us.tbl$date))$val * 100
        paste0(formatC(num_deaths, format = "d", big.mark = ","), " (", round(growth_deaths, digits = 2), "%)")
        
    })
    
    ## Second Fluid Row
    
    output$us_graph_total <- renderPlotly({
        
        ggplot(filter(us.tbl, us.tbl$metric %in% c("cases", "deaths")),
               aes(x = date,
                   y = val,
                   color = metric))+
            geom_line()+
            geom_point()+
            scale_y_continuous(labels = scales::comma)+
            ylab("Totals")
        
    })
    
    output$us_graph_new <- renderPlotly({
        
        ggplot(filter(us.tbl, us.tbl$metric %in% c("new_cases", "new_deaths")),
               aes(x = date,
                   y = val,
                   color = metric))+
            geom_line()+
            geom_point()+
            scale_y_continuous(labels = scales::comma)+
            ylab("New Cases and Deaths")
        
    })
    
    output$us_graph_mort <- renderPlotly({
        
        ggplot(filter(us.tbl, us.tbl$metric == "mortality"),
               aes(x = date,
                   y = val,
                   color = metric))+
            geom_line()+
            geom_point()+
            scale_y_continuous(labels = scales::percent)+
            ylab("Mortality Rate")
        
    })
    
    output$us_graph_growth <- renderPlotly({
        
        ggplot(filter(us.tbl, us.tbl$metric %in% c("per_growth_cases", "per_growth_deaths")),
               aes(x = date,
                   y = val,
                   color = metric))+
            geom_line()+
            geom_point()+
            scale_y_continuous(labels = scales::percent)+
            ylab("Daily Growth Rate")
        
    })
    
    ## States Tab Server Logic ------------------------------------------

    output$graph_state_cases <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = cases,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Total Cases")
        
    })
    
    output$graph_state_new_cases <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = new_cases,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Cases")
        
    })
    
    output$graph_state_deaths <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = deaths,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Total Deaths")
        
    })
    
    output$graph_state_new_deaths <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = new_deaths,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Deaths")
        
    })
    
    output$graph_state_cases_per_mil <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = cases_per_mil,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Cases (per mil)")
        
    })
    
    output$graph_state_new_cases_per_mil <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = new_cases_per_mil,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Cases (per mil)")
        
    })
    
    output$graph_state_deaths_per_mil <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = deaths_per_mil,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Total Deaths (per mil)")
        
    })
    
    output$graph_state_new_deaths_per_mil <- renderPlotly({
        
        ggplot(filter(states.tbl, state %in% input$states_selected,
                      date >= as.Date(as.character(min(input$states_dates))),
                      date <= as.Date(as.character(max(input$states_dates)))),
               aes(x = date,
                   y = new_deaths_per_mil,
                   color = state))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Deaths (per mil)")
        
    })
    
    output$states_table <- DT::renderDataTable({
        
        states.summary <- filter(states.tbl[c("state", "POPESTIMATE2019", "cases", "deaths", "mortality", "cases_per_mil", "deaths_per_mil")],
                                 states.tbl$date == max(states.tbl$date))
        colnames(states.summary) <- c("State", "Population", "Cases", "Deaths", "Mortality Rate", "Cases (per million)", "Deaths (per million)")
        
        datatable(states.summary[complete.cases(states.summary), ], options = list(pageLength = nrow(states.summary[complete.cases(states.summary), ]))) %>%
            formatRound(c("Cases (per million)", "Deaths (per million)"), 2) %>%
            formatRound(c("Population", "Cases", "Deaths"), 0) %>%
            formatPercentage("Mortality Rate", 2) 

    })
    
    ## Counties Tab Server Logic -------------------------------------
    
    # Render UI for Selecting Counties
    output$select_counties <- renderUI({
        
        county_choices <- str_sort(unique(counties.tbl$county[counties.tbl$state == input$state]))
        checkboxGroupInput("counties", "Select Counties to Graph", county_choices)
        
    })
    
    # Create County Graphs
    output$graph_county_cases <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = cases,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Total Cases")
        
    })
    
    output$graph_county_new_cases <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = new_cases,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Cases")
        
    })
    
    output$graph_county_deaths <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = deaths,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Total Deaths")
        
    })
    
    output$graph_county_new_deaths <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = new_deaths,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Deaths")
        
    })
    
    output$graph_county_cases_per_mil <- renderPlotly({
    
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = cases_per_mil,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Total Cases per mil")
        
    })
    
    output$graph_county_new_cases_per_mil <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = new_cases_per_mil,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Cases per mil")
        
    })
    
    output$graph_county_deaths_per_mil <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = deaths_per_mil,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Total Deaths per mil")
        
    })
    
    output$graph_county_new_deaths_per_mil <- renderPlotly({
        
        ggplot(filter(counties.tbl, state == input$state, county %in% input$counties,
                      date >= as.Date(as.character(min(input$county_dates))),
                      date <= as.Date(as.character(max(input$county_dates)))),
               aes(x = date,
                   y = new_deaths_per_mil,
                   color = county))+
            geom_point()+
            geom_smooth()+
            ylab("Daily Deaths per mil")
        
    })
    
    # Create County Data Table
    output$counties_table <- DT::renderDataTable({
        
        counties.summary <- filter(counties.tbl[c("county", "POPESTIMATE2019", "cases", "deaths", "mortality", "cases_per_mil", "deaths_per_mil")],
                                   counties.tbl$date == max(counties.tbl$date),
                                   counties.tbl$state == input$state)
        colnames(counties.summary) <- c("County", "Population", "Cases", "Deaths", "Mortality Rate", "Cases (per million)", "Deaths (per million)")
        
        datatable(counties.summary[complete.cases(counties.summary), ], options = list(pageLength = nrow(counties.summary[complete.cases(counties.summary), ]))) %>%
            formatRound(c("Cases (per million)", "Deaths (per million)"), 2) %>%
            formatRound(c("Population", "Cases", "Deaths"), 0) %>%
            formatPercentage("Mortality Rate", 2) 
        
    })
    
    # Create Summary State Stats for County Page
    output$state_cases <- renderText({
        
        num_cases <- filter(states.tbl, states.tbl$state == input$state, states.tbl$date == max(states.tbl$date))$cases
        formatC(num_cases, format = "d", big.mark = ",")
        
    })
    
    output$state_deaths <- renderText({
        
        num_deaths <- filter(states.tbl, states.tbl$state == input$state, states.tbl$date == max(states.tbl$date))$deaths
        formatC(num_deaths, format = "d", big.mark = ",")
        
    })
    
    output$state_mort <- renderText({
        
        mort_rate <- filter(states.tbl, states.tbl$state == input$state, states.tbl$date == max(states.tbl$date))$mortality * 100
        paste0(round(mort_rate, digits = 2), "%")
        
    })
    
    ## Map Tab Server Logic ------------------------------------------
    
    output$map <- renderLeaflet({
        
        map_and_data$mortality <- as.numeric(map_and_data$mortality) * 100
        
        tmap_mode("view")
        
        options <- c("Cases", "Cases (per mil)", "Deaths", "Deaths (per mil)", "Mortality Rate (%)")
        metrics <- c("cases", "cases_per_mil", "deaths", "deaths_per_mil", "mortality")
        input.df <- as.data.frame(cbind(options, metrics))
            
        s.metric <- as.character(input.df$metrics[input.df$options == as.character(input$map_measure)])
            
        tm <- tm_shape(filter(map_and_data))+
        tm_fill(s.metric, title = paste(input$map_measure), alpha = 0.7, n = 10, style = "quantile",
                id = "county",
                popup.vars = c("Cases" = "cases", 
                               "Deaths" = "deaths", 
                               "Cases/mil" = "cases_per_mil",
                               "Deaths/mil" = "deaths_per_mil",
                               "Mortality Rate (%)" = "mortality"),
                popup.format = list(mortality = list(digits = 2), cases_per_mil = list(digits = 0), deaths_per_mil = list(digits = 0)))+
        tm_borders()+
        tm_basemap(server = "OpenStreetMap", alpha = 0.5)
        
        tmap_leaflet(tm)
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
