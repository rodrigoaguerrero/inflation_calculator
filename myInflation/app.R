#Load csv of states found in regional resources of BLS with sub regions

all_states = read.csv("data/full_state_list.csv")

unlist(all_states, recursive = TRUE, use.names = TRUE)

states_w_metro_areas = read.csv("data/states_w_metro_areas.csv")

#print(states_w_metro_areas)
# Install hash package, used for creating dictionaries. Skip if already installed

#install.packages("hash")

# load hash package
library(hash)

#set hash function to "h" for ease of reading.
h <- hash() 
# set values

for (state in states_w_metro_areas) {
  if ("Alaska" %in% state) {
    h[["Alaska"]] <- "Urban Alaska"
  }
  
  if ("Arizona" %in% state) {
    h[["Arizona"]] <- "Phoenix-Mesa-Scottsdale"
  }
  
  if ("California" %in% state) {
    h[["California"]] <- list("Los Angeles" = "Los Angeles-Long Beach-Anaheim",
                              "Riverside" = "Riverside-San Bernardino-Ontario",
                              "San Diego" = "San Diego-Carlsbad",
                              "San Francisco" = "San Francisco-Oakland-Hayward")
  }
  
  if ("Colorado" %in% state) {
    h[["Colorado"]] <- "Denver-Aurora-Lakewood"
  }
  
  if ("Delaware" %in% state) {
    h[["Delaware"]] <- "Philadelphia-Camden-Wilmington"
  }
  
  if ("District of Columbia" %in% state) {
    h[["District of Columbia"]] <- "Washington-Arlington-Alexandria"
  }
  
  if ("Florida" %in% state) {
    h[["Florida"]] <- list("Tampa" = "Tampa-St. Petersburg-Clearwater",
                           "Miami" = "Miami-Fort Lauderdale-West Palm Beach")
  }
  
  if ("Georgia" %in% state) {
    h[["Georgia"]] <- "Atlanta-Sandy Springs-Roswell"
  }
  if ("Hawaii" %in% state) {
    h[["Hawaii"]] <- "Urban Hawaii"
  }
  if ("Illinois" %in% state) {
    h[["Illinois"]] <- list("Chicago" = "Chicago-Naperville-Elgin",
                            "St. Louis" = "St. Louis")
  }
  if ("Indiana" %in% state) {
    h[["Indiana"]] <- "Chicago-Naperville-Elgin"
  }
  if ("Maryland" %in% state) {
    h[["Maryland"]] <- list("Baltimore" = "Baltimore-Columbia-Towson",
                            "Philadelphia" = "Philadelphia-Camden-Wilmington",
                            "Washington D.C." = "Washington-Arlington-Alexandra")
  }
  if ("Massachusetts" %in% state) {
    h[["Maassachusetts"]]  <- "Boston-Cambridge-Newton"
  }
  if ("Michigan" %in% state) {
    h[["Michigan"]] <- "Detroit-Warren-Dearborn"
  }
  if ("Minnesota" %in% state) {
    h[["Minnesota"]] <- "Minneapolis-St.Paul-Bloomington"
  }
  if ("Missouri" %in% state) {
    h[["Missouri"]] <- "St. Louis"
  }
  if ("New Hampshire" %in% state) {
    h[["New Hampshire"]] <- "Boston-Cambridge-Newton"
  }
  if ("New Jersey" %in% state) {
    h[["New Jersey"]] <- list("New York-Newark-Jersey City",
                              "Philadelphia-Camden-Wilmington")
  }
  if ("New York" %in% state) {
    h[["New York"]] <-"New York-Newark-Jersey City"
  }
  if ("Pennsylvania" %in% state){
    h[["Pennsylvania"]] <- list("New York-Newark-Jersey City",
                                "Philadelphia-Camden-Wilmington") 
  }
  if ("Texas" %in% state) {
    h[["Texas"]] <- list("Dallas-Fort Worth-Arlington",
                         "Houston-The Woodlands-Sugar Land")
  }
  if ("Virginia" %in% state) {
    h[["Virginia"]] <- "Washington-Arlington-Alexandria"
  }
  if ("Washington" %in% state) {
    h[["Washington"]] <- "Seattle-Tacoma-Bellevue"
  }
  if ("West Virginia" %in% state) {
    h[["West Virginia"]] <- "Washington-Arlington-Alexandria"
  }
  if ("Wisconsin" %in% state) {
    h[["Wisconsin"]] <- list("Chicago-Naperville-Elgin",
                             "Minneapolis-St.Paul-Bloomington")
  }
}

h[["Wisconsin"]] #MAKE SURE VALUES ARE HASHED TO KEY


library(shiny)
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
                               
                               # conditional side panel for dollar check page
                               conditionalPanel(
                                 condition = "input.tabSelected == 2",
                                 tags$b("This tool measures the buying power of the dollar over time."),
                                 br(),
                                 br(),
                                 tags$em("The tool uses the latest US government CPI data published on February 10, 2022 to adjust for inflation and calculate the cumulative inflation rate through January 2022. The U.S. Labor Department's Bureau of Labor Statistics will release the Consumer Price Index (CPI) with inflation data for February on March 10, 2022. (See a chart of recent inflation rates.)")
                                 
                               )
                             ),
                             
                             # main panel for home page/tool page
                             mainPanel(
                               tabsetPanel(
                                 # tab for main calculator page
                                 tabPanel("Personal Inflation Calculator", value = 1,
                                          # test conditional statement for displaying plot
                                          conditionalPanel(
                                            condition = "input.state == 'Arizona' && input.plotType == 'lau'",
                                            plotOutput("lauPlot")
                                          )
                                 ),
                                 
                                 # tab for dollar checker page
                                 tabPanel("Dollar Comparison", value = 2,
                                          helpText("Embedded Calculator tool goes here")),
                                 
                                 id = "tabSelected"
                               )
                               
                             )
                           )
                         ),
                         # glossary page
                         tabPanel("Glossary", helpText("Glossary, more detailed definitions go here")),
                         
                         # other pages/ help and about page
                         navbarMenu("More",
                                    tabPanel("Help", helpText("Help Page Info Here")),
                                    tabPanel("About", 
                                             helpText("Through the personal inflation rate, a person will be able to first, create a market basket, taking the inputs of the user to then create an inflation rate using the personalized market basket they created with the inputs they provided and then being able to compare that inflation rate to other available data points, helping to contextualize it and applying it to their everyday life and providing information that could prove vital to their financial strategies and doing so in an easy to understand way which advises the user throughout the experience of using the tool."),
                                             br(),
                                             helpText("Users will also be able to compare how their inflat.ion rate differs from the inflation rates of other regions/states generally according to the data from the Bureau of Labor Statistics (BLS) as well as from different years generally as well as the inflation rates of their own market basket"),
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
                             
                           
                           
                )
                
                
                #NEED SOME SORT OF CONDITIONAL PANEL HERE
)

server <- function(input, output, session){
  # test data
  data <- data <- bls_api("LAUST040000000000004",startyear = 2010, endyear = 2020, Sys.getenv("BLS_KEY"))
  data <- data %>%
    mutate(
      periodName = factor(periodName, levels = month.name)
    ) %>%
    arrange(periodName)
  
  # test output render
  output$lauPlot <- renderPlot({
    if (input$plotType == "lau") {
      data_filtered <- data %>%
        filter(year == input$yearInput)
      ggplot(data_filtered, aes(periodName, value, group = 1)) +
        geom_line() + 
        geom_point() +
        theme_classic()
    }
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

