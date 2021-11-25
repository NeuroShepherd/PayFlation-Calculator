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
                     value = 0, format = "dollar", align = "left")

    ),



    dashboardBody(

        fluidRow(
            box(
                title = "title text",
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

                tabPanel("Table", DT::dataTableOutput("testTable") ),
                tabPanel("Graph", plotOutput("testPlot"))
                )
        )

    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$testTable <- DT::renderDataTable({
        mtcars
    }, options = list(scrollX = TRUE))


    output$testPlot <- renderPlot({
        ggplot(mtcars, aes(x=mpg, y=wt)) +
            geom_smooth(method = "lm", se=F) +
            geom_point(aes(color=factor(cyl)))
    })

    output$gauge <- renderGauge({
        gauge(170,
              min = 0,
              max = 200,
              symbol = "%",
              # label = "Hello, here is a bunch of text for the label arg",
              sectors = gaugeSectors(success = c(50, 200),
                                     warning = c(20, 50),
                                     danger = c(0, 20)))
    })


    output$textMessage <- renderGauge({
       print("Here's a text message!")
    })

}

# Run the application
shinyApp(ui = ui, server = server)
