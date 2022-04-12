library(shiny)
library(tidyverse)
library(blscrapeR)
library(ggplot2)
library(shinythemes)
library(gridExtra)

all_states = read.csv("data/full_state_list.csv")

unlist(all_states, recursive = TRUE, use.names = TRUE)

states_w_metro_areas = read.csv("data/states_w_metro_areas.csv")

library(gridExtra)
egCPI_table_creation <- function(){
  egCPI <- matrix(
    c(4.80,6.20,
      2.29,2.80,
      3.32,4.29,
      4.75,5.40,
      9.50, 12), 
    ncol = 2,
    byrow = TRUE)
  colnames(egCPI) <- c("Price in U.S. Dollars in Current Period (2021)",
                       "Price in U.S. Dollars in Base Period (2022)")
  rownames(egCPI) <- c("Beef (per pound)",
                       "Eggs (per dozen)", 
                       "Gasoline (per gallon)",
                       "Bread (per pre-sliced loaf)",
                       "Socks (per pack of three)")
  egCPI <- as.table(egCPI)
  grid.table(egCPI)
  returnValue(egCPI)
}
CPI2_table_creation <- function(){
  egCPI <- matrix(c(4.80,6.20, 77.42,
                    2.29,2.80, 81.79,
                    3.32,4.29, 77.39,
                    4.75,5.40, 87.96,
                    9.50, 12, 79.17),
                  ncol = 3, 
                  byrow = TRUE)
  colnames(egCPI) <- c("Price in U.S. Dollars in Current Period (2022)",
                       "Price in U.S. Dollars in Base Period (2021)",
                       "CPI of Item between Current Period (2022) and Base Period (2021)")
  rownames(egCPI) <- c("Beef (per pound)",
                       "Eggs (per dozen)", 
                       "Gasoline (per gallon)",
                       "Bread (per pre-sliced loaf)",
                       "Socks (per pack of three)")
  egCPI <- as.table(egCPI)
  grid.table(egCPI)
  returnValue(egCPI)
}

egCPI_table_creation
CPI2_table_creation()



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
                               
                               # Personal Input title and Advanced settings checkbox
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 h3("Personal Inputs"),
                                 checkboxInput("advancedSet", "Advanced", FALSE)
                               ),
                               br(),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 textInput("incomeInput", "Income")
                               ),

                               # OTHER INPUTS
                               
                               # Start and End Year selection
                               # conditionalPanel(
                               #   condition = "input.tabSelected == 1",
                               #   sliderInput("startEndYear", "Select Year Range", min = 1999, max = 2022, sep = "", value = c(2000, 2010))
                               # ),
                               
                               # Type of eaters
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 selectInput("eaterType", "What is your Eating Lifestyle?", c("Omnivore", "Carnivore", "Pollotarian",
                                                                                              "Pescetarian", "Vegetarian", "Vegan"))
                               ),
                               # food groups
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxGroupInput("foodGroups", "Do You Buy These Items Frequently?", c("Cereal" = "cereal",
                                                                                                "Sugar" = "sugar",
                                                                                                "Alcohol" = "alcohol",
                                                                                                "Milk" = "milk",
                                                                                                "Fats and/or Oils" = "fatOil"))
                                                                                                
                               ), 
                               # Monthly Food expenses
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("foodInput", "Your Monthly Food Expenses: ",
                                             min = 0, max = 5000, value = 1200, sep = "")
                               ),
                               # Renting or Mortgage?
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 selectInput("rentBuy", "Are you Renting, Buying, or Own?", c("Renting", "Buying w/Mortgage", "Own"))
                               ),
                               # Housing Expenses
                               conditionalPanel(
                                 condition = "input.tabSelected == 1",
                                 sliderInput("housingInput", "Your Monthly Housing Expenses: ",
                                             min = 0, max = 10000, value = 8200, sep = "")
                               ),
                               # Public or Private
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 selectInput("transPrivatePublic", "What Type of Transportation?", c("Private", "Public"))
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
                                 condition = "input.tabSelected == 1 && input.advancedSet != 1",
                                 sliderInput("comInput", "Your Monthly Communication and Education Expenses: ",
                                             min = 0, max = 10000, value = 2300, sep = "")
                               ),
                               # advanced phone 
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet ==1",
                                 sliderInput("comInput", "Your Monthly Phone Expenses: ",
                                             min = 0, max = 500, value = 130, sep = "")
                               ),
                               # advanced education
                               conditionalPanel(
                                 condition = "input.tabSelected ==1 && input.advancedSet == 1",
                                 radioButtons("eduInput", "Education Type: ",
                                              c("None" = "None",
                                                "Elementary/Highschool" = "elehs",
                                                "Tuition/College" = "tuition"))
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 sliderInput("eduInput", "Your Monthly Education Expenses:",
                                             min = 0, max = 50000, value = 300, sep = "")
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
                                            condition = "input.tabSelected == 1 && input.advancedSet == 1",
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
                                          fluidRow(
                                            column(
                                              width = 4,
                                              div(style = "white-space: nowrap;",
                                                  h5("If in the Year", style = "display:inline-block"),
                                                  div(style = "display: inline-block; width: 100%", textInput("infYearInp",label = "", value = "1913", width = 150))
                                              )
                                                  
                                            )
                                          ),
                                          fluidRow(
                                            column(
                                              width = 4,
                                              div(style = "white-space: nowrap;",
                                                  h5("and you Purchased an Item worth $", style = "display:inline-block"),
                                                  div(style = "display:inline-block; width: 100%", textInput("infPriceInp", label = "", value = "20.11", width = 150))
                                                  )
                                            )
                                          ),
                                          fluidRow(
                                            column(
                                              width = 4,
                                              div(style = "white-space: nowrap;",
                                                  h5("then in the Year", style = "display:inline-block"),
                                                  div(style = "display:inline-block; width: 100%", textInput("infThenInp", label = "", value = "2022", width = 150))
                                                  )
                                            )
                                          ),
                                           textOutput("dollarComp")
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
                                  tags$i("Figure 1:"),
                                  tags$h5(img(src = "price_table.png")),
                                  tags$h5("Now using the above price table, we can calculate the CPI by taking the quotient of the cost of the itemin the current period and the cost in the base period and multiply it by 100. The result of that calculuation can be seen in figure 2 which is identitcal to figure 1 with the exception of the addition of a column denoting the CPI."),
                                  tags$i("Figure 1:"),
                                  tags$h5(img(src = "price_table_w_cpi.png")),
                                  tags$b("CPI-U (Consumer Price Index for All Urban Consumers): "),
                                  tags$h5("The CPI-U is simply a specific type of CPI which is calculated by taking into account the prices that are paid by urban consumers for a market basket of consumer goods and services. The CPI-U is often the most applicable CPI for most inhabitants of the United States as the majority of inhabitants live in areas which are considered by the U.S. Bureau of Labor Statistics to be 'urban'"),
                                  br(),
                                  # math formulas
                                  tags$h1("Formulas"),
                                  tags$b("Consumer Price Index (CPI) Formula: "),
                                  tags$h5("The Bureau of Labor Statistics does this by creating a market basket which creates a consumer price index. The market basket is created by taking a monthly survey which is distributed to consumers and from that survey data, a sample of goods and services is generated which can then be used to create a market basket which is representative of general populations."),
                                  tags$h5("CPI = ( Cost of Market Basket in Current Period / Cost of Market Basket in Base Period ) * 100", style="color:red"),
                                  br(),
                                  tags$b("Inflation Formula"),
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
                print("Privacy Policy: If you require any more information or have any questions about this privacy policy, please feel free to contact one of us by email at si_info@arizona.edu. Someone from the iSchool will contact us on your behalf.
                The privacy of visitors and users is of extreme importance to us. This privacy policy document outlines the types of personal information is received and collected by the MyInflation Calculator and how it is used. 
                Log Files: Like many other Web sites and applications, we make use of log files. The information inside the log files includes internet protocol (IP) addresses, type of browser, date/time, referring pages, and cookies to analyze trends, administer the site, track user’s movement around the site, and gather demographic information. IP addresses, cookies, and other such information are not linked to any information that is personally identifiable.
                Cookies: The MyInflation Calculator does NOT use cookies to store information about visitor’s preferences (including any numbers or characters inputted into our app). We feel this information is unnecessary to collect and store due to the nature of the applications usage. ")

)


server <- function(input, output, session){
  # test data
  # data <- data <- bls_api("CUUR0000SA0R",startyear = 2011, endyear = 2020, Sys.getenv("BLS_KEY"))
  # data <- data %>%
  #   mutate(
  #     periodName = factor(periodName, levels = month.name)
  #   ) %>%
  #   arrange(periodName)
  
  # datasets for series ids
  metro_series <- read.csv("data/metro_series.csv", row.names=1)
  us_series <- read.csv("data/us_city.csv", row.names=1)
  income_series <- read.csv("data/income_series.csv", row.names = 1)
  
  
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
  
  # personal rate
  output$personalRate <- renderText({
    if (input$advancedSet == 1) {
      income = as.numeric(input$incomeInput)
      
      # advanced setting rent finder
      index <- grep(input$metro_area, rownames(metro_series))
      
      # if user selected metro_area, didn't select "own", and we have the series ID in our dataset
      if (input$metro_area != "None" && is.na(metro_series$rent[index]) != TRUE && input$rentBuy != "Own") {
        rentData <- metro_series$rent[index]
        rentData <- bls_api(metro_series$rent[index], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
        
      } # if user selected a metro_area, have the data, and selected own
      else if (input$metro_area != "None" && is.na(metro_series$own[index]) != TRUE && input$rentBuy == "Own") {
        rentData <- metro_series$own[index]
        rentData <- bls_api(metro_series$rent[index], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
        
      } else {    # no metro area data for rent, use us city avg or user didn't select a metro area 
        rentData <- bls_api("CWUR0000SAS2RS", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
      }
      # advanced education 
      # if user selected "Elementary/Highschool, picked a metro area, and we have the data for it 
      if (input$eduInput == "elehs" && input$metro_area != "None" && is.na(metro_series$school[index]) != TRUE) {
        eduData <- metro_series$school[index]
        eduData <- bls_api(eduData, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      # if user selected "Elementary/Highschool, didn't pick a metro area or we don't have the data
      else if (input$eduInput == "elehs" && input$metro_area == "None" | is.na(metro_series$school[index]) == TRUE) {
        eduData <- bls_api(us_series$school, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      # if user selected "College", no metro data for this one
      else if (input$eduInput == "tuition") {
        eduData <- bls_api(us_series$tuition, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      
      # advanced medical cpi fetcher
      # if user picked a metro area and we have the data stored
      if (input$metro_area != "None" && is.na(metro_series$medical[index]) != TRUE) {
        medicalData <- bls_api(metro_series$medical[index], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMedical1 <- medicalData[1]
        cpiMedical2 <- medicalData[2]
      }
      # if user selected metro area and we don't have data stored or just didn't select metro area
      else {
        medicalData <- bls_api(us_series$medical, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_buy(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMedical1 <- medicalData[1]
        cpiMedical2 <- medicalData[2]
      }
      
      # advanced apparel cpi fetcher
      # if user selected a metro area and we have the data for it
      if (input$metro_area != "None" && is.na(metro_series$clothes[index]) != TRUE) {
        clothData <- bls_api(metro_series$clothes[index], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCloth1 <- clothData[1]
        cpiCloth2 <- clothData[2]
      }
      # user didn't select a metro area or they did and we don't have the data
      else {
        clothData <- bls_api(us_series$clothes, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCloth1 <- clothData[1]
        cpiCloth2 <- clothData[2]
      }
      
      # advanced transportation cpi fetcher
      # if user selected "private", selected a metro area, and we have the data stored
      if (input$metro_area != "None" && input$transPrivatePublic == "Private" && is.na(metro_series$fuel[index]) != TRUE) {
        travelData <- bls_api(metro_series$fuel[index], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData[1]
        cpiTravel2 <- travelData[2]
      }
      # if user selected "private" but we don't have the data or they didn't select a metro
      else if (input$transPrivatePublic == "Private" && input$metro_area != "None" && is.na(metro_series$fuel[index]) == TRUE) {
        travelData <- bls_api(us_series$fuel, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData[1]
        cpiTravel2 <- travelData[2]
      }
      # user selected "private" and didn't select metro
      else if (input$transPrivatePublic == "Private" && input$metro_area == "None") {
        travelData <- bls_api(us_series$fuel, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData[1]
        cpiTravel2 <- travelData[2]
      }
      # user selected "public" and gave an income
      else if (input$transPrivatePublic == "Public" && input$income != "") {
        
        # income <69,999
        if (income < 70000) {
          travelData <- bls_api(income_series$X15000.29999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData[1]
          cpiTravel2 <- travelData[2]
        } 
        # 69,999 < income < 99,999
        else if (income < 99999 && income > 69999) {
          travelData <- bls_api(income_series$X70000.99999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData[1]
          cpiTravel2 <- travelData[2]
        }
        # 100,000 < income < 149,999
        else if (income < 149999 && income > 99999) {
          travelData <- bls_api(income_series$X100000.149999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData[1]
          cpiTravel2 <- travelData[2]
        }
        # 150,000 < income < 199,999
        else if (income < 199999 && income > 149999) {
          travelData <- bls_api(income_series$X150000.199999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData[1]
          cpiTravel2 <- travelData[2]
        }
        # income > 200,000
        else {
          travelData <- bls_api(income_series$X200000[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData[1]
          cpiTravel2 <- travelData[2]
        }
        
        # user didn't input income
      }else {
        travelData <- bls_api(us_series$travel, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData[1]
        cpiTravel2 <- travelData[2]
      }
      
      # advanced cell phone cpi fetcher
      if (income != "") {
        if (income < 70000) {
          cellData <- bls_api(income_series$X15000.29999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiCell1 <- cellData[1]
          cpiCell2 <- cellData[2]
        } 
        # 69,999 < income < 99,999
        else if (income < 99999 && income > 69999) {
          cellData <- bls_api(income_series$X70000.99999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiCell1 <- cellData[1]
          cpiCell2 <- cellData[2]
        }
        # 100,000 < income < 149,999
        else if (income < 149999 && income > 99999) {
          cellData <- bls_api(income_series$X100000.149999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiCell1 <- cellData[1]
          cpiCell2 <- cellData[2]
        }
        # 150,000 < income < 199,999
        else if (income < 199999 && income > 149999) {
          cellData <- bls_api(income_series$X150000.199999[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiCell1 <- cellData[1]
          cpiCell2 <- cellData[2]
        }
        # income > 200,000
        else {
          cellData <- bls_api(income_series$X200000[1], startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiCell1 <- cellData[1]
          cpiCell2 <- cellData[2]
        }
      }
      # user provided no income
      else {
        cellData <- bls_api(us_series$cell, startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData[1]
        cpiCell2 <- cellData[2]
      }
      
      # advanced food groups cpi fetching
      # goes here
      ##################################################################################################
      
    } else {       # non advanced settings
      # rent cpi fetcher
      rentData <- bls_api("CWUR0000SAS2RS", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiRent1 <- rentData$name[1]
      cpiRent2 <- rentData$name[2]
      
      # transportation cpi fetcher
      transportData <- bls_api("CUUR0000SAT", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiTransport1 <- transportData$name[1]
      cpiTransport2 <- transportData$name[2]
      
      # medical cpi fetcher
      medicalData <- bls_api("CUUR0000SAM", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiMed1 <- medicalData$name[1]
      cpiMed2 <- medicalData$name[2]
      
      # communication cpi fetcher
      commData <- bls_api("CUUR0000SAE2", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiCom1 <- commData$name[1]
      cpiCom2 <- commData$name[2]
      
      # apparel cpi fetcher
      apparelData <- bls_api("SUUR0000SAA", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiApp1 <- apparelData$name[1]
      cpiApp2 <- apparelData$name[2]
      
      # recreational cpi fetcher
      recData <- bls_api("SUUR0000SAR", startyear = 2021, endyear = 2022, Sys.getenv("BLS_KEY")) %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiRec1 <- recData$name[1]
      cpiRec2 <- recData$name[2]
      
      
    }
    
  
    
  })
  
  # US national average
  output$usRate <- renderText({
    paste("U.S. Average Inflation Rate: ", input$clothInput)
  })
  
  # Weighted Individual Inflation Rate
  output$weightRate <- renderText({
    paste("Your Weighted Individual Inflation Rate: ", input$recInput)
  })
  output$dollarComp <- renderText({
    data1 <- bls_api("CUUR0000SA0",startyear = input$infYearInp, endyear = input$infYearInp, Sys.getenv("BLS_KEY"))
    data2 <- bls_api("CUUR0000SA0",startyear = input$infThenInp, endyear = input$infThenInp, Sys.getenv("BLS_KEY"))
    cpi1 <- data1$value[1]
    cpi2 <- data2$value[1]
    
    formula <- as.numeric(input$infPriceInp) * (as.numeric(cpi2) / as.numeric(cpi1))
    perChange <- (formula - as.numeric(input$infPriceInp)) / as.numeric(input$infPriceInp)
    paste("That same item would cost: $", round(formula,2))
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

