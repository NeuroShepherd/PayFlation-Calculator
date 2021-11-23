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

# Define UI for application that draws a histogram
ui <- dashboardPage(

    dashboardHeader(title = "PayFlation Calculator",
                    titleWidth = 300),
    dashboardSidebar(

        width = 300,
        br(),
        br(),
        br(),
        dateInput(inputId = "date1", label = "First Paycheck Date",
                  format = "yyyy-mm-dd",
                  value = Sys.Date() %m-% months(12) ),
        numericInput(inputId = "salary1", label = "First Paycheck Amount in $USD",
                     value = 0, min = 0),

        dateInput(inputId = "date2", label = "Latest Paycheck Date",
                  format = "yyyy-mm-dd",
                  max = Sys.Date() %m+% months(12)),
        numericInput(inputId = "salary2", label = "Latest Paycheck Amount in $USD",
                     value = 0, min = 0)

    ),
    dashboardBody()
)

# Define server logic required to draw a histogram
server <- function(input, output) {


}

# Run the application
shinyApp(ui = ui, server = server)
