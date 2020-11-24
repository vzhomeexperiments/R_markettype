#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(ggplot2)
library(magrittr)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    
    #current data row
    mt_analysed <- reactive({ mt_analysed <- macd_ai[input$rows, 65]})
    ln_analysed <- reactive({ ln_analysed <- macd_ai[input$rows, ]})
    
    #write data while pressing a button
    observeEvent(input$Store, {
        
        #use usual construct to read if exist aggregate, etc
        if(!file.exists(file_checked)){
            ln_analysed() %>% 
                readr::write_rds(file_checked)
            #remove row
            macd_ai <- macd_ai[-input$rows, ]
        } else {
            readr::read_rds(file_checked) %>% 
                dplyr::bind_rows(ln_analysed()) %>% 
                readr::write_rds(file_checked)
            #remove row
            macd_ai <- macd_ai[-input$rows, ]
            updateSliderInput(session, inputId = "rows",min = 1, max = nrow(macd_ai),value = 1)
        }
        
    })
    
   
    
    #graphs and outputs
    output$AI_Data <- renderPlot({

        # generate bins based on input$bins from ui.R
        #To Do create a Shiny APP to scroll through!
        plot(x = 1:64, y = macd_ai[input$rows, 1:64], main = mt_analysed())
        abline(h=0, col="blue")
        
        #plot(x = 1:64, y = macd_ai[3, 1:64])
      

    })

})
