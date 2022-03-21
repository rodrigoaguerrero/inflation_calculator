#Load csv of states found in regional resources of BLS with sub regions

all_states = read.csv("data/full_state_list.csv")

unlist(all_states, recursive = TRUE, use.names = TRUE)

states_w_metro_areas = read.csv("data/states_w_metro_areas.csv")

items = c("Beef", "Eggs", "Gasoline", "Bread", "Socks")
base_period_prices = c("")
rep?

library(shiny)
library(tidyverse)
library(blscrapeR)
library(ggplot2)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cosmo"),
                # testing nav bar
                navbarPage("MyInflation",
                         tabPanel("Home", verbatimTextOutput("Home"),
                           
                           sidebarLayout(
                             
                             sidebarPanel(
                               
                               #conditonal side panel for calculator page
                               
                               
                               # conditional side panel for dollar check page
                               conditionalPanel(
                                 condition = "input.tabSelected == 2",
                                 tags$b("This tool measures the buying power of the dollar over time."),
                                 br(),
                                 br(),
                                 tags$em("The tool uses the latest US government CPI data published on February 10, 2022 to adjust for inflation and calculate the cumulative inflation rate through January 2022. The U.S. Labor Department's Bureau of Labor Statistics will release the Consumer Price Index (CPI) with inflation data for February on March 10, 2022. (See a chart of recent inflation rates.)")
                                 
                               ), 
                               br(),

                               # OTHER INPUTS:
                               # Monthly Food expenses
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("foodInput", "Your Monthly Food Expenses: ",
                                             min = 0, max = 5000, value = 1200, sep = "")
                               ),
                               # Housing Expenses
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("housingInput", "Your Monthly Housing Expenses: ",
                                             min = 0, max = 10000, value = 8200, sep = "")
                               ),
                               # Transportation
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("transInput", "Your Monthly Transportation Expenses: ",
                                             min = 0, max = 5000, value = 3200, sep = "")
                               ),
                               # Medical
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("medInput", "Your Monthly Medical Expenses: ",
                                             min = 0, max = 10000, value = 480, sep = "")
                               ),
                               # Communication/Education
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("comInput", "Your Monthly Communication and Education Expenses: ",
                                             min = 0, max = 10000, value = 2300, sep = "")
                               ),
                               # Clothing
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("clothInput", "Your Monthly Apparel Expense: ",
                                             min = 0, max = 5000, value = 4000, sep = "")
                               ),
                               # Recreation
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("recInput", "Your Monthly Recreational Expenses: ",
                                             min = 0, max = 3000, value = 250, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("otherInput", "Your Other Monthly Expenses: ",
                                             min = 0 , max = 5000, value = 4200, sep = "")
                               )
                               
                             ),
                             
                             # main panel for home page/tool page
                             mainPanel(
                               tabsetPanel(
                                 # tab for main calculator page
                                 tabPanel("Personal Inflation Calculator", value = 1,
                                          conditionalPanel(
                                            condition = "input.tabSelected == 1",
                                            selectInput("state", "Choose Your State/Territory of Residence", all_states),
                                            br(),
                                            
                                            # conditionals for residence area if applicable
                                            conditionalPanel(
                                              condition = "input.state == 'Alaska'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", c("None", "Metro Alaska")
                                              ),
                                              a("Click here to view a map of counties and Metro Areas in Alaska",target="_blank",href="Alaska.pdf"),
                                              textOutput("AK_txtOutput")
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'Arizona'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", c("None", "Phoenix-Mesa-Scottsdale")
                                              ),
                                              a("Click here to view a map of counties and Metro Areas in Arizona",target="_blank",href="Arizona.pdf"),
                                              textOutput("AZ_txtOutput")
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'California'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                          c("None", "Los Angeles-Long Beach-Anaheim","Riverside-San Bernardino-Ontario",
                                                            "San Diego-Carlsbad", "San Francisco-Oakland-Hayward")
                                              ),
                                              a("Click here to view a map of counties and Metro Areas in California",target="_blank",href="California.pdf"),
                                              textOutput("CA_txtOutput")
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'Colorado'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                          c("None", "Denver-Aurora-Lakewood")
                                              ),
                                              a("Click here to view a map of counties and Metro Areas in Colorado",target="_blank",href="Colorado.pdf")
                                              
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'Delaware'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                          c("None", "Philadelphia-Camden-Wilmington")
                                              ),
                                              a("Click here to view a map of Counties and Metro Areas in Delaware",target="_blank",href="Delaware.pdf")
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'Florida'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                          c("None", "Tampa-St. Petersburg-Clearwater","Miami-Fort Lauderdale-West Palm Beach")
                                              ),
                                              a("Click here to view a map of Counties and Metro Areas in Florida",target="_blank",href="Florida.pdf"),
                                              textOutput("FL_txtOutput")
                                            ),
                                            conditionalPanel(
                                              condition = "input.state == 'Georgia'",
                                              selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                          c("None", "Atlanta-Sandy Springs-Roswell")
                                              ),
                                              a("Click here to view a map of Counties and Metro Areas in Georgia",target="_blank",href="Georgia.pdf"),
                                              textOutput("GA_txtOutput")
                                            )
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Hawaii'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Urban Hawaii")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Hawaii",target="_blank",href="Hawaii.pdf"),
                                            textOutput("HI_txtOuput")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Illinois'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Chicago-Naperville-Elgin", "St. Louis")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Illinois",target="_blank",href="Illinois.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Indiana'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Chicago-Naperville-Elgin")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Indiana",target="_blank",href="Indiana.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Maryland'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Baltimore-Columbia-Towson", "Philadelphia-Camden-Wilmington", 
                                                          "Washington-Arlington-Alexandria")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Maryland",target="_blank",href="Maryland.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Massachusetts'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Boston-Cambridge-Newton")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Massachusetts",target="_blank",href="Massachusetts.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Michigan'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Detroit-Warren-Dearborn")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Michigan",target="_blank",href="Michigan.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Minnesota'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Minneapolis-St.Paul-Bloomington")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Minnesota",target="_blank",href="Minnesota.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Missouri'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "St. Louis")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Missouri",target="_blank",href="Missouri.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'New Hampshire'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Boston-Cambridge-Newton")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in New Hampshire",target="_blank",href="New_Hampshire.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'New Jersey'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "New York-Newark-Jersey City", "Philadelphia-Camden-Wilmington")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in New Jersey",target="_blank",href="New_Jersey.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'New York'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "New York-Newark-Jersey City")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in New York",target="_blank",href="New_York.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Pennsylvania'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "New York-Newark-Jersey City", "Philadelphia-Camden-Wilmington")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Pennsylvania",target="_blank",href="Pennsylvania.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Texas'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Dallas-Fort Worth-Arlington", "Houston-The Woodlands-Sugar Land")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Texas",target="_blank",href="Texas.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Virginia'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Washington-Arlington-Alexandria")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Virginia",target="_blank",href="Virginia.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Washington'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Seattle-Tacoma-Bellevue")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Washington",target="_blank",href="Washington.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'West Virginia'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Washington-Arlington-Alexandria")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in West Virginia",target="_blank",href="West_Virginia.pdf")
                                          ),
                                          conditionalPanel(
                                            condition = "input.state == 'Wisconsin'",
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", 
                                                        c("None", "Chicago-Naperville-Elgin", "Minneapolis-St.Paul-Bloomington")
                                            ),
                                            a("Click here to view a map of Counties and Metro Areas in Wisconsin",target="_blank",href="Wisconsin.pdf")
                                          ),
                                          br(),
                                          br(),
                                          textOutput("personalRate"),
                                          br(),
                                          textOutput("usRate"),
                                          br(),
                                          textOutput("weightRate"),
                                          br(),
                                          br(),
                                          radioButtons("plotType", "Purchasing Power Trend ($)",c("Inflation Trend")),
                                           sliderInput("yearInput", "Select Year:",
                                                      min = 2011, max = 2021, value = 2015, sep = "", animate = animationOptions(interval = 500, loop = TRUE)),
                                          plotOutput("inflateplot")
                                          
                                 ),
                                 
                                 # tab for dollar checker page
                                 tabPanel("Dollar Comparison", value = 2,
                                          helpText("Embedded Calculator tool goes here"),
                                          br(),
                                          helpText("*****WORK IN PROGRESS*****")
                                          ),
                                 
                                 id = "tabSelected"
                               )
                               
                             )
                           )
                         ),
                         # glossary page
                         tabPanel("Glossary",
                                  # defintions
                                  tags$h1("Definitions"),
                                  tags$b("Inflation: "),
                                  tags$h5("Inflation is a decrease in the buying power of money which is caused by the rise in prices of goods and services over a period of time, most commonly a one year period."),
                                  br(),
                                  tags$b("Market Basket: "),
                                  tags$h5("Because there are innumerable goods and services and not all goods and services are frequently bought by the members of the population and as such would not be instructive in creating a general inflation rate, certain goods and services must be chosen to be a part of the aggregation which creates the general inflation rate."),
                                  tags$h5("The Bureau of Labor Statistics does this by creating a market basket which creates a consumer price index. The market basket is created by taking a monthly survey which is distributed to consumers and from that survey data, a sample of goods and services is generated which can then be used to create a general inflation rate."),
                                  br(),
                                  tags$b("Consumer Price Index (CPI): "),
                                  tags$h5("The consumer price index (CPI) takes weighted averages of prices of items within a market basket (see above). A CPI is calculated by taking the price changes of each of those items in the market basket and averaging them. Then using the CPI, rates of inflation and deflation can be found."),
                                  tags$h5("The formula for a CPI is as follows:"),
                                  tags$h5(img(src = "CPI_Formula.png")),
                                  tags$h5("To illustrate how a CPI is calculated here is an example:"),
                                  tags$h5("First, let's begin with our Market Basket. For this example, our market basket is much more limited than the market basket that is used by the Bureau of Labor Statistics. In our basket we have the following five items:"),
                                  tags$b("1. Beef"),
                                  br(),
                                  tags$b("2. Eggs"),
                                  br(),
                                  tags$b("3. Gasoline"),
                                  br(),
                                  tags$b("4. Bread "),
                                  br(),
                                  tags$b("5. Socks"),
                                  br(),
                                  tags$h5("In order to calculate our CPI we need to know certain data points about these five items. Specifically, we need to know the cost of each of these items in the current period and the cost of each of these items in the base period."),
                                  tags$h5("For our example, our current period will be the year 2021 and the base period will be the year 2020. In the below table, labeled 'Figure 1' we can see how the prices of each of these items has changed between the base period and the current period."),
                                  tags$b("CPI-U (Consumer Price Index for All Urban Consumers): "),
                                  tags$h5("--definition--"),
                                  br(),
                                  # math formulas
                                  tags$h1("Calculation Formulas"),
                                  tags$b("Calculating CPI (Consumer Price Index): "),
                                  tags$h5("The Bureau of Labor Statistics does this by creating a market basket which creates a consumer price index. The market basket is created by taking a monthly survey which is distributed to consumers and from that survey data, a sample of goods and services is generated which can then be used to create a market basket which is representative of general populations."),
                                  tags$h5("CPI = ( Cost of Market Basket in Current Period / Cost of Market Basket in Base Period ) * 100", style="color:red"),
                                  br(),
                                  tags$b("Calculating Inflation: "),
                                  tags$h5("( ( Starting Cost - Ending Cost ) / Starting Cost ) x 100", style="color:red")
                         ),
                         
                         # other pages/ help and about page
                         navbarMenu("More",
                                    tabPanel("Help", helpText("Help Page Info Here")),
                                    tabPanel("About", 
                                             helpText("Through the personal inflation rate, a person will be able to first, create a market basket, taking the inputs of the user to then create an inflation rate using the personalized market basket they created with the inputs they provided and then being able to compare that inflation rate to other available data points, helping to contextualize it and applying it to their everyday life and providing information that could prove vital to their financial strategies and doing so in an easy to understand way which advises the user throughout the experience of using the tool."),
                                             br(),
                                             helpText("Users will also be able to compare how their inflation rate differs from the inflation rates of other regions/states generally according to the data from the Bureau of Labor Statistics (BLS) as well as from different years generally as well as the inflation rates of their own market basket"),
                                             br(),
                                             tags$h1("MEET THE TEAM: ", style="color:red"),
                                             tags$em("Project Manager - Rodrigo Guerrero"),
                                             br(),
                                             tags$em("'ROLE' - Ian Forsyth"),
                                             br(),
                                             tags$em("'ROLE' - Nick Manuel"),
                                             br(),
                                             tags$em("'ROLE' - Travis Myers")
                                    )
                         )
   
                ),
                hr(),
                print("Privacy Disclaimer HERE!")

)

server <- function(input, output, session){
  # test data
  data <- data <- bls_api("CUUR0000SA0R",startyear = 2011, endyear = 2021, Sys.getenv("BLS_KEY"))
  data <- data %>%
    mutate(
      periodName = factor(periodName, levels = month.name)
    ) %>%
    arrange(periodName)
  
  # test output render
  output$inflateplot <- renderPlot({
    if (input$plotType == "Inflation Trend") {
      data_filtered <- data %>%
        filter(year == input$yearInput)
      ggplot(data_filtered, aes(periodName, value, group = 1)) +
        geom_line() + 
        geom_point() +
        theme_classic()
    }
  })
  
  # text outputs
  # personal rate
  output$personalRate <- renderText({
    paste("Your Personal Inflation Rate: ", input$housingInput)
  })
  
  # US national average
  output$usRate <- renderText({
    paste("U.S. Average Inflation Rate: ", input$clothInput)
  })
  
  # Weighted Individual Inflation Rate
  output$weightRate <- renderText({
    paste("Your Weighted Individual Inflation Rate: ", input$recInput)
  })
}




shinyApp(ui,server = server)

#EXAMPLE
# 
# library(shiny)
# 
# shinyUI(fluidPage(
#   titlePanel("Demonstration of the selectInput UI widget in shiny"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput("states","Select the state",all_states)
#     ),
#     mainPanel()
#   )
# ))
# shiny::runApp()

