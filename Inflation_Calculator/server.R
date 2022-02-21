#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$state <- renderText(input$states)
    output$AK_txtOutput = renderText({
        paste0("Note: 'Urban Alaska' refers to the Anchorage Metropolitan Statistical Area.")
      })
    output$AZ_txtOutput = renderText({
      paste0("Note: 'Phoenix-Mesa-Scottsdale' refers to the Phoenix-Mesa-Chandler Metropolitan Statistical Area which includes Maricopa and Pinal Counties. See above map for more details.")
    })
    output$CA_txtOutput = renderText({
      paste0("Note:'San Francisco-Oakland-Hayward' includes Alameda, Contra Costa, Marin, San Francisco and San Mateo Counties.
             'San Diego-Carlsbad'refers to the San Diego-Chula Vista-Carlsbad Metropolitan Statistical Area which is comprised of San Diego County. See above map for more details ")
    })
    output$FL_txtOutput = renderText({
      paste0("Note:")
    })
    })