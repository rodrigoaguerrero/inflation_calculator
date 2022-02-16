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
    titlePanel("Personal Inflation Calculator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("state",
                        "Select your state or territory of residence",
                        all_states)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            conditionalPanel(
                condition = ("input.state == 'Alabma' || 'Arkansas'"),
                selectInput(
                    "test",
                    "test",
                    c("test option 1","hell ya")
                )
            ),
            conditionalPanel(
                
                condition = "input.state == 'Alaska'",
                selectInput(
                    "metro_area",
                    "Select your metro area of residence, if applicable",
                    c("None", "Metro Alaska"),
                    
                )
                
            ),
            
                )
            )
           
        )
    )
))
