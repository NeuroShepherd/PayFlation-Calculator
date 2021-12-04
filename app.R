#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(flexdashboard)
source("scrape inflation data.R")
source("clean inflation data.R")
source("calculate.R")

parameters <- list(sidebarwidth = 300)
# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "PayFlation Calculator",
                    titleWidth = parameters$sidebarwidth),



    dashboardSidebar(

        width = parameters$sidebarwidth,
        br(),
        br(),
        br(),
        dateInput(inputId = "date1", label = "First Paycheck Date",
                  format = "yyyy-mm-dd",
                  value = Sys.Date() %m-% months(12) ),
        currencyInput(inputId = "salary1", label = "First Paycheck Amount in $USD",
                     value = 0, format = "dollar", align = "left"),

        dateInput(inputId = "date2", label = "Latest Paycheck Date",
                  format = "yyyy-mm-dd",
                  max = Sys.Date() %m+% months(12)),
        currencyInput(inputId = "salary2", label = "Latest Paycheck Amount in $USD",
                     value = 0, format = "dollar", align = "left"),
        br(),
        br(),

        actionBttn("runAnalysis", "Run Analysis")

    ),



    dashboardBody(

        fluidRow(
            box(
                title = "Inflation-adjusted Change",
                gaugeOutput("gauge"),
                footer = "here's some footer text"
                ),
            box(
                textOutput("textMessage")
            )
        ),

        fluidRow(
            tabBox(
                width = 12, side = "right", selected = "Graph",

                tabPanel("All Time Table", DT::dataTableOutput("AllTimeTableData")),
                tabPanel("User Table", DT::dataTableOutput("testTable") ),
                tabPanel("Graph", plotly::plotlyOutput("testPlot"))
                )
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    salary1_reac <- reactive({input$salary1})
    date1_reac <- reactive({input$date1})
    salary2_reac <- reactive({input$salary2})
    date2_reac <- reactive({input$date2})

    vals <-reactiveValues(Table = NULL,
                          Graph = NULL,
                          gauge = NULL,
                          textMessage = NULL)


    salary_data <- eventReactive(input$runAnalysis, {

        calculate_inflation_adjusted_return(date1 = date1_reac(), date2 = date2_reac(),
                                            salary1 = salary1_reac(), salary2 = salary2_reac())

    })




    output$testTable <- DT::renderDataTable({
        salary_data() %>%
            magrittr::extract2("compounding_table") %>%
            DT::datatable() %>%
            DT::formatPercentage(c("Annual Inflation"), 2) %>%
            DT::formatPercentage(c("Daily Inflation Rate"), 6)
            # DT::formatRound(c("Daily Inflation Rate", "Effective Inflation"), 8)
    }, options = list(scrollX = TRUE))


    output$testPlot <- plotly::renderPlotly({
        calculator_net_inflation_table_cleaned_default %>%
            arrange(Year) %>%
            mutate(`Cumulative Inflation` = cumprod(`Effective Inflation`)) %>%
            {ggplot(., aes(x=Year)) +
            theme_bw() +
            geom_line(aes(y=`Cumulative Inflation`)) +
            geom_bar(aes(y=`Effective Inflation`), stat = "identity")} %>%
        plotly::ggplotly()

        # TODO: update the graph to include a line showing someone's starting and ending
        # salary points so they can compare against inflation visually. Note that doing this
        # means either changing this or creating a new graph where the X-axis and cumprod()
        # calculations are time-locked to the date1 and date2 inputs
    })

    output$gauge <- renderGauge({
        gauge(salary_data()[["inflation_adjusted_return_percentage"]],
              min = -200,
              max = 200,
              symbol = "%",
              # label = "Hello, here is a bunch of text for the label arg",
              sectors = gaugeSectors(success = c(50, 200),
                                     warning = c(0, 50),
                                     danger = c(-200, 0)))
    })


    output$textMessage <- renderText({
        salary_data()[["text_output"]]
    })

    output$AllTimeTableData <- DT::renderDataTable({
        calculator_net_inflation_table_cleaned_default %>%
            DT::datatable() %>%
            DT::formatPercentage(c("Annual Inflation"), 2) %>%
            DT::formatPercentage(c("Daily Inflation Rate"), 6)
    })

}

# Run the application
shinyApp(ui = ui, server = server)
