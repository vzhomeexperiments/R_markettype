#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Inspect Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("rows",
                        "Number of rows:",
                        min = 1,
                        max = nrow(macd_ai),
                        value = 1,
                        step = 1),
            actionButton(inputId = "Store", label = "Store This Pattern")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("AI_Data")
        )
    )
))
