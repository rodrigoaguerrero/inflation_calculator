library(shiny)
library(tidyverse)
library(blscrapeR)
library(ggplot2)
library(shinythemes)
library(gridExtra)
library(scales)

all_states = read.csv("data/full_state_list.csv")

unlist(all_states, recursive = TRUE, use.names = TRUE)

states_w_metro_areas = read.csv("data/states_w_metro_areas.csv")

egCPI_table_creation <- function(){
  egCPI <- matrix(
    c(4.80,6.20,
      2.29,2.80,
      3.32,4.29,
      4.75,5.40,
      9.50, 12), 
    ncol = 2,
    byrow = TRUE)
<<<<<<< Updated upstream
  colnames(egCPI) <- c("Price in U.S. Dollars during Base Period (2021)",
                       "Price in U.S. Dollars during Current Period (2022)")
=======
  colnames(egCPI) <- c("Price in U.S. Dollars in Current Period (2020)",
                       "Price in U.S. Dollars in Base Period (2021)")
>>>>>>> Stashed changes
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
  egCPI <- matrix(c(4.80,6.20, 129.17,
                    2.29,2.80, 122.27,
                    3.32,4.29, 129.22,
                    4.75,5.40, 113.68,
                    9.50, 12.00, 126.32),
                  #rounded to the nearest .00
                  ncol = 3, 
                  byrow = TRUE)
<<<<<<< Updated upstream
  colnames(egCPI) <- c("Price in U.S. Dollars in Base Period (2021)",
                       "Price in U.S. Dollars in Current Period (2022)",
                       "CPI of Item between Base Period (2021) and Current Period (2022)")
=======
  colnames(egCPI) <- c("Price in U.S. Dollars in Current Period (2021)",
                       "Price in U.S. Dollars in Base Period (2020)",
                       "CPI of Item between Current Period (2021) and Base Period (2020)")
>>>>>>> Stashed changes
  rownames(egCPI) <- c("Beef (per pound)",
                       "Eggs (per dozen)", 
                       "Gasoline (per gallon)",
                       "Bread (per pre-sliced loaf)",
                       "Socks (per pack of three)")
  tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  egCPI <- as.table(egCPI)
  grid.table(egCPI, theme = tt)
  help("grid.table")
  returnValue(egCPI)
}


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
                                 tags$em("The tool uses the latest US government CPI data published on February 10, 2021 to adjust for inflation and calculate the cumulative inflation rate through January 2021. The U.S. Labor Department's Bureau of Labor Statistics will release the Consumer Price Index (CPI) with inflation data for February on March 10, 2021. (See a chart of recent inflation rates.)")
                                 
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
                                 textInput("incomeInput", "Income", value = "")
                               ),
                               

                               # OTHER INPUTS
                               
                               # Start and End Year selection
                               # conditionalPanel(
                               #   condition = "input.tabSelected == 1",
                               #   sliderInput("startEndYear", "Select Year Range", min = 1999, max = 2021, sep = "", value = c(2000, 2010))
                               # ),
                               
                               
                               # Monthly Food expenses
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 0",
                                 sliderInput("foodInput", "Your Monthly Food Expenses: ",
                                             min = 0, max = 5000, value = 1200, sep = "")
                               ),
                               # Type of eaters
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 selectInput("eaterType", "What is your Eating Lifestyle?", c("Omnivore", "Carnivore", "Pollotarian",
                                                                                              "Pescetarian", "Vegetarian", "Vegan"))
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Omnivore'",
                                 sliderInput("meatInput", "Your Monthly Meat Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Omnivore'",
                                 sliderInput("fruitInput", "Your Monthly Fruit and Vegetable Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Carnivore'",
                                 sliderInput("meatInput", "Your Monthly Meat Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Pollotarian'",
                                 sliderInput("meatInput", "Your Monthly Poultry Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Pollotarian'",
                                 sliderInput("fruitInput", "Your Monthly Fruit and Vegetable Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Pescetarian'",
                                 sliderInput("meatInput", "Your Monthly Fish Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Pescetarian'",
                                 sliderInput("fruitInput", "Your Monthly Fruit and Vegetable Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Vegan'",
                                 sliderInput("fruitInput", "Your Monthly Fruit and Vegetable Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.eaterType == 'Vegetarian'",
                                 sliderInput("fruitInput", "Your Monthly Fruit and Vegetable Expenses: ",
                                             min = 0, max = 500, value = 22, sep = "")
                               ),
                               # food groups
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 h5("Do You Buy These Items Frequently?")
                               ), 
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxInput("cerealCheckbox", "Cereal")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxInput("sugarCheckbox", "Sugar")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxInput("alcCheckbox", "Alcohol")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxInput("milkCheckbox", "Milk")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1",
                                 checkboxInput("fatOilCheckbox", "Fats and Oils")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.cerealCheckbox == 1",
                                 sliderInput("cerealInput", "How Much Do You Spend on Cereal Monthly",
                                             min = 0, max = 300, value = 20, sep="")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.sugarCheckbox == 1",
                                 sliderInput("sugarInput", "How Much do you Spend on Sugar Monthly",
                                             min = 0, max = 150, value = 80, sep="")
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.alcCheckbox == 1",
                                 sliderInput("alcInput", "How Much do you Spend on Alcohol Monthly",
                                             min = 0, max = 500, value = 230)
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.milkCheckbox == 1",
                                 sliderInput("milkInput", "How Much do you Spend on Milk Monthly",
                                             min = 0, max = 200, value = 110)
                               ),
                               conditionalPanel(
                                 condition = "input.tabSelected == 1 && input.advancedSet == 1 && input.fatOilCheckbox == 1",
                                 sliderInput("fatInput", "How Much do you Spend on Fats and Oils Monthly",
                                             min = 0, max = 300, value = 23)
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
                                 radioButtons("eduTypeInput", "Education Type: ",
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
                                        
                                              
                                              # ),
                                              # a("Click here to view a map of counties and Metro Areas in Alaska",target="_blank",href="Alaska.pdf"),
                                              # textOutput("AK_txtOutput")
                                            selectInput("metro_area", "Select your metro area of residence, if applicable", choices = c("None"))
                                      
                                          ),
                                          br(),
                                          br(),
                                          
                                          textOutput("personalRate"),
                                          
                                          br(),
                                          textOutput("usRate"),
                                          br(),
                                          textOutput("weightRate"),
                                          actionButton("go", "Get Personal Inflation Results"),
                                          br(),
                                          br(),
                                          radioButtons("plotType", "Purchasing Power Trend ($)",c("Inflation Trend")),
                                          sliderInput("yearInput", "Select Year:",
                                                      min = 2011, max = 2020, value = 2015, sep = "", animate = animationOptions(interval = 500, loop = TRUE)),
                                          
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
                                                  div(style = "display:inline-block; width: 100%", textInput("infThenInp", label = "", value = "2021", width = 150))
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
                                  tags$b("Consumer Price Index (CPI): "),
                                  tags$h5("The consumer price index (CPI) is an instrument used to measure the inflation rate. It estimates the variation in price of a good or service between two different periods of time. "),
                                  tags$h5("It is calculated by taking the quotient of the price of a good or service in the current period and dividing it by the price of the same good or service in the base period and multiplying the quotient by one hundred (see Figure 1)."),
                                  tags$i("Figure 1:"),
                                  tags$h5(img(src = "CPI_Formula.png")),
                                  tags$b("Market Basket: "),
                                  tags$h5("A Market Basket is a tool utilized to create a CPI for a variety of goods and services which are within a market basket. A bundle of goods and services which serve as a representative sample of what goods and services are purchased by the average consumer of a population is what constitutes a market basket."),
                                  tags$h5("Often, the prices of items in a market basket are recorded over multiple periods of time to determine the CPI at various points in time and ultimately, calculate an inflation rate between any town points in time in which a CPI has been calculated and recorded."),
                                  tags$h1("An Example"),
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
                                  tags$h5("For our example, our current period will be the year 2020 and the base period will be the year 2020. In the below table, labeled 'Figure 1' we can see how the prices of each of these items has changed between the base period and the current period."),
                                  tags$i("Figure 1:"),
                                  tags$h5(img(src = "price_table.png")),
                                  tags$h5("Now using the above price table, we can calculate the CPI by taking the quotient of the cost of the itemin the current period and the cost in the base period and multiply it by 100. The result of that calculuation can be seen in figure 2 which is identitcal to figure 1 with the exception of the addition of a column denoting the CPI."),
                                  tags$i("Figure 2:"),
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
  inflateData <- bls_api("CUUR0000SA0R",startyear = 2011, endyear = 2020, "04b92e5f5d22471f8190da23796fda75")
  inflateData <- inflateData %>%
    mutate(
      periodName = factor(periodName, levels = month.name)
    ) %>%
    arrange(periodName)
  
  # datasets for series ids
  metro_series <- read.csv("data/metro_series.csv", row.names=1)
  metro_series$fuel[1] <- "CUURS49GSETB"
  us_series <- read.csv("data/us_city.csv", row.names=1)
  income_series <- read.csv("data/income_series.csv", row.names = 1)
  # test comment
  
  # test output render 
  output$inflateplot <- renderPlot({
    if (input$plotType == "Inflation Trend") {
      data_filtered <- inflateData %>%
        filter(year == input$yearInput)
      ggplot(data_filtered, aes(periodName, value, group = 1)) +
        geom_line() + 
        geom_point() +
        theme_classic()
    }
  })
  
  # dynamically change metro area inputs
  observe({
    state <- input$state
    if (state == "Alaska") {
      updateSelectInput(session, "metro_area", choices = c("None", "Metro Alaska"))
    }
    else if (state == "Arizona") {
      updateSelectInput(session, "metro_area",choices = c("None", "Phoenix-Mesa-Scottsdale"))
    }
    else if (state == "California") {
      updateSelectInput(session, "metro_area", choices = c("None", "Los Angeles-Long Beach-Anaheim","Riverside-San Bernardino-Ontario",
                                                         "San Diego-Carlsbad", "San Francisco-Oakland-Hayward"))
    }
    else if (state == "Colorado") {
      updateSelectInput(session, "metro_area", choices = c("None", "Denver-Aurora-Lakewood"))
    }
    else if (state == "Delaware") {
      updateSelectInput(session, "metro_area", choices = c("None", "Philadelphia-Camden-Wilmington"))
    }
    else if (state == "Florida") {
      updateSelectInput(session, "metro_area", choices = c("None", "Tampa-St. Petersburg-Clearwater","Miami-Fort Lauderdale-West Palm Beach"))
    }
    else if (state == "Georgia") {
      updateSelectInput(session, "metro_area", choices = c("None", "Atlanta-Sandy Springs-Roswell"))
    }
    else if (state == "Hawaii") {
      updateSelectInput(session, "metro_area", choices = c("None", "Urban Hawaii"))
    }
    else if (state == "Illinois") {
      updateSelectInput(session, "metro_area", choices = c("None", "Chicago-Naperville-Elgin", "St. Louis"))
    }
    else if (state == "Indiana") {
      updateSelectInput(session, "metro_area", choices = c("None", "Chicago-Naperville-Elgin"))
    }
    else if (state == "Maryland") {
      updateSelectInput(session, "metro_area", choices = c("None", "Baltimore-Columbia-Towson", "Philadelphia-Camden-Wilmington", 
                                                         "Washington-Arlington-Alexandria"))
    }
    else if (state == "Massachusetts") {
      updateSelectInput(session, "metro_area", choices = c("None", "Boston-Cambridge-Newton"))
    }
    else if (state == "Michigan") {
      updateSelectInput(session, "metro_area", choices = c("None", "Detroit-Warren-Dearborn"))
    }
    else if (state == "Minnesota") {
      updateSelectinput(session, metro_area, choices = c("None", "Minneapolis-St.Paul-Bloomington"))
    }
    else if (state == "Missouri") {
      updateSelectInput(session, "metro_area", choices = c("None", "St. Louis"))
    }
    else if (state == "New Hampshire") {
      updateSelectInput(session, "metro_area", choices = c("None", "Boston-Cambridge-Newton"))
    }
    else if (state == "New Jersey") {
      updateSelectInput(session, "metro_area", choices = c("None", "New York-Newark-Jersey City", "Philadelphia-Camden-Wilmington"))
    }
    else if (state == "New York") {
      updateSelectInput(session, "metro_area", choices = c("None", "New York-Newark-Jersey City"))
    }
    else if (state == "Pennsylvania") {
      updateSelectInput(session, "metro_area", choices = c("None", "New York-Newark-Jersey City", "Philadelphia-Camden-Wilmington"))
    }
    else if (state == "Texas") {
      updateSelectInput(session, "metro_area", choices = c("None", "Dallas-Fort Worth-Arlington", "Houston-The Woodlands-Sugar Land"))
    }
    else if (state == "Virginia") {
      updateSelectInput(session, "metro_area", choices = c("None", "Washington-Arlington-Alexandria"))
    }
    else if (state == "Washington") {
      updateSelectInput(session, "metro_area", choices = c("None", "Seattle-Tacoma-Bellevue"))
    }
    else if (state == "West Virginia") {
      updateSelectInput(session, "metro_area", choices = c("None", "Washington-Arlington-Alexandria"))
    }
    else if (state == "Wisconsin") {
      updateSelectInput(session, "metro_area", choices = c("None", "Chicago-Naperville-Elgin", "Minneapolis-St.Paul-Bloomington"))
    }
    else {
      updateSelectInput(session, "metro_area", choices = c("None"))
    }
  })
  
  observeEvent(input$go, {
    if (input$advancedSet == 1) {
      income = as.numeric(gsub(",","",input$incomeInput))
      # advanced setting rent finder
      index <- grep(input$metro_area, rownames(metro_series))
      
      print(paste(metro_series$fruit_veg[index]))
      # if user selected metro_area, didn't select "own", and we have the series ID in our dataset
      if (input$metro_area != "None" && metro_series$rent[index] != "" && input$rentBuy != "Own") {
        rentData <- metro_series$rent[index]
        rentData <- bls_api(metro_series$rent[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
        
      } # if user selected a metro_area, have the data, and selected own
      else if (input$metro_area != "None" && metro_series$rent[index] != "" && input$rentBuy == "Own") {
        rentData <- metro_series$own[index]
        rentData <- bls_api(metro_series$own[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
        
      } else {    # no metro area data for rent, use us city avg or user didn't select a metro area 
        rentData <- bls_api("CWUR0000SAS2RS", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiRent1 <- rentData$name[1]
        cpiRent2 <- rentData$name[2]
        
      }
      # advanced education 
      # if user selected "Elementary/Highschool, picked a metro area, and we have the data for it
      if (input$eduTypeInput == "None") {
        cpiEdu1 <- 1
        cpiEdu2 <- 1
      }
      else if (input$eduTypeInput == "elehs" && input$metro_area != "None" && is.na(metro_series$school[index]) != TRUE) {
        eduData <- metro_series$school[index]
        eduData <- bls_api(eduData, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      # if user selected "Elementary/Highschool, didn't pick a metro area or we don't have the data
      
      else if (input$eduTypeInput == "elehs" && input$metro_area == "None" && mis.na(metro_series$school[index]) != TRUE || input$metro_area == "None" && input$eduTypeInput == "elehs") {
        print("elementary no metro")
        eduData <- bls_api(us_series$school, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      # if user selected "College", no metro data for this one
      else if (input$eduTypeInput == "tuition") {
        eduData <- bls_api(us_series$tutition, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      else {
        eduData <- bls_api(us_series$school, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiEdu1 <- eduData$name[1]
        cpiEdu2 <- eduData$name[2]
      }
      
      # advanced medical cpi fetcher
      # if user picked a metro area and we have the data stored
      if (input$metro_area != "None" && is.na(metro_series$medical[index]) != TRUE) {
        medicalData <- bls_api(metro_series$medical[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMedical1 <- medicalData$name[1]
        cpiMedical2 <- medicalData$name[2]
        print("this better not do anything ever")
      }
      # if user selected metro area and we don't have data stored or just didn't select metro area
      else {
        medicalData <- bls_api(us_series$medical, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMedical1 <- medicalData$name[1]
        cpiMedical2 <- medicalData$name[2]
      }
      
      # advanced apparel cpi fetcher
      # if user selected a metro area and we have the data for it
      if (input$metro_area != "None" && metro_series$clothes[index] != "") {
        print("clothe check 1")
        clothData <- bls_api(metro_series$clothes[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCloth1 <- clothData$name[1]
        cpiCloth2 <- clothData$name[2]
      }
      # user didn't select a metro area or they did and we don't have the data
      else {
        clothData <- bls_api(us_series$clothes, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCloth1 <- clothData$name[1]
        cpiCloth2 <- clothData$name[2]
      }
      # advanced transportation cpi fetcher
      # if user selected "private", selected a metro area, and we have the data stored
      
      if (input$metro_area != "None" && input$transPrivatePublic == "Private" && metro_series$fuel[index] != "") {
        print("metro area, private transport, data")
        travelData <- bls_api(metro_series$fuel[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData$name[1]
        cpiTravel2 <- travelData$name[2]
      }
      # if user selected "private" but we don't have the data or they didn't select a metro
      else if (input$transPrivatePublic == "Private" && input$metro_area != "None" && metro_series$fuel[index] == "") {
        travelData <- bls_api(us_series$fuel, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData$name[1]
        cpiTravel2 <- travelData$name[2]
      }
      # user selected "private" and didn't select metro
      else if (input$transPrivatePublic == "Private" && input$metro_area == "None") {
        travelData <- bls_api(us_series$fuel, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData$name[1]
        cpiTravel2 <- travelData$name[2]
      }
      # user selected "public" and gave an income
      
      
      else if (input$transPrivatePublic == "Public" && input$incomeInput != "") {
        
        # income <69,999
        if (income < 70000) {
          travelData <- bls_api(income_series$X15000.29999[1], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData$name[1]
          cpiTravel2 <- travelData$name[2]
        } 
        # 69,999 < income < 99,999
        else if (income < 99999 && income > 69999) {
          travelData <- bls_api(income_series$X70000.99999[1], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData$name[1]
          cpiTravel2 <- travelData$name[2]
        }
        # 100,000 < income < 149,999
        else if (income < 149999 && income > 99999) {
          travelData <- bls_api(income_series$X100000.149999[1], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData$name[1]
          cpiTravel2 <- travelData$name[2]
        }
        # 150,000 < income < 199,999
        else if (income < 199999 && income > 149999) {
          travelData <- bls_api(income_series$X150000.199999[1], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData$name[1]
          cpiTravel2 <- travelData$name[2]
        }
        # income > 200,000
        else {
          travelData <- bls_api(income_series$X200000[1], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiTravel1 <- travelData$name[1]
          cpiTravel2 <- travelData$name[2]
        }
        
        # user didn't input income
      }else {
        travelData <- bls_api(us_series$travel, startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiTravel1 <- travelData$name[1]
        cpiTravel2 <- travelData$name[2]
      }
      
      # advanced cell phone cpi fetcher
      if (input$incomeInput == "") {
        print("cell no income")
        cellData <- bls_api(us_series$cell, startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      }
      else if (income < 70000) {
        cellData <- bls_api(income_series$X15000.29999[2], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      } 
      # 69,999 < income < 99,999
      else if (income < 99999 && income > 69999) {
        cellData <- bls_api(income_series$X70000.99999[2], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      }
      # 100,000 < income < 149,999
      else if (income < 149999 && income > 99999) {
        cellData <- bls_api(income_series$X100000.149999[2], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      }
      # 150,000 < income < 199,999
      else if (income < 199999 && income > 149999) {
        cellData <- bls_api(income_series$X150000.199999[2], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      }
      # income > 200,000
      else {
        cellData <- bls_api(income_series$X200000[2], startyear = 2018, endyear = 2019, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCell1 <- cellData$name[1]
        cpiCell2 <- cellData$name[2]
      }
      
      # advanced food groups cpi fetching
      if (input$milkCheckbox) {
        cpiMilk1 <- 0
        cpiMilk2 <- 0
      }
      else {
        milkData <- bls_api(us_series$milk, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMilk1 <- milkData$name[1]
        cpiMilk2 <- milkData$name[2]
      }
      # sugar
      if (!input$sugarCheckbox) {
        cpiSugar1 <- 0
        cpiSugar2 <- 0
      }
      else {
        sugarData <- bls_api(us_series$sugar, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiSugar1 <- sugarData$name[1]
        cpiSugar2 <- sugarData$name[2]
      }
      # alcohol
      if (!input$alcCheckbox) {
        cpiAlc1 <- 0
        cpiAlc2 <- 0
      }
      else {
        alcData <- bls_api(us_series$alcohol, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiAlc1 <- alcData$name[1]
        cpiAlc2 <- alcData$name[2]
      }
      # Cereal
      if (!input$cerealCheckbox) {
        cpiCereal1 <- 0
        cpiCereal2 <- 0
      }
      else {
        cerealData <- bls_api(us_series$cereal, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiCereal1 <- cerealData$name[1]
        cpiCereal2 <- cerealData$name[2]
      }
      # fats and oils
      if (!input$fatOilCheckbox) {
        cpiFat1 <- 0
        cpiFat2 <- 0
      }
      else {
        fatData <- bls_api(us_series$fats, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiFat1 <- fatData$name[1]
        cpiFat2 <- fatData$name[2]
      }
      
      # omnivore
      if (input$eaterType == "Omnivore") {
        # if we have fruit for their metro area
        if (input$metro_area != "None" && metro_series$fruit_veg[index] != "") {
          fruitData <- bls_api(metro_series$fruit_veg[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        else {
          fruitData <- bls_api(us_series$fruit_veg, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        # if we have meat and fish for their metro area
        if (input$metro_area != "None" && metro_series$meat_fish[index] != "") {
          meatData <- bls_api(metro_series$meat_fish[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiMeat1 <- meatData$name[1]
          cpiMeat2 <- meatData$name[2]
        }
        else {
          meatData <- bls_api(us_series$meat_fish, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiMeat1 <- meatData$name[1]
          cpiMeat2 <- meatData$name[2]
        }
        
      }
      # carnivore
      else if (input$eaterType == "Carnivore") {
        # if we have meat and fish for their metro area
        if (input$metro_area != "None" && metro_series$meat_fish[index] != "") {
          meatData <- bls_api(metro_series$meat_fish[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiMeat1 <- meatData$name[1]
          cpiMeat2 <- meatData$name[2]
        }
        else {
          meatData <- bls_api(us_series$meat_fish, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiMeat1 <- meatData$name[1]
          cpiMeat2 <- meatData$name[2]
        }
        cpiFruit1 <- 0
        cpiFruit2 <- 0
      }
      # pollotarian
      # if we have fruit for their metro area
      else if (input$eaterType == "Pollotarian") {
        if (input$metro_area != "None" && metro_series$fruit_veg[index] != "") {
          fruitData <- bls_api(metro_series$fruit_veg[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        else {
          fruitData <- bls_api(us_series$fruit_veg, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        # chicken data
        meatData <- bls_api(us_series$chicken, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMeat1 <- meatData$name[1]
        cpiMeat2 <- meatData$name[2]
      }
      
      # pescetarian
      else if (input$eaterType == "Pescetarian") {
        if (input$metro_area != "None" && metro_series$fruit_veg[index] != "") {
          fruitData <- bls_api(metro_series$fruit_veg[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        else {
          fruitData <- bls_api(us_series$fruit_veg, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        # fish data
        meatData <- bls_api(us_series$fish, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
          group_by(year) %>%
          summarise_at(vars(value), list(name = mean))
        cpiMeat1 <- meatData$name[1]
        cpiMeat2 <- meatData$name[2]
      }
      
      # Vegetarian / Vegan
      else {
        if (input$metro_area != "None" && metro_series$fruit_veg[index] != "") {
          fruitData <- bls_api(metro_series$fruit_veg[index], startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        else {
          fruitData <- bls_api(us_series$fruit_veg, startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
            group_by(year) %>%
            summarise_at(vars(value), list(name = mean))
          cpiFruit1 <- fruitData$name[1]
          cpiFruit2 <- fruitData$name[2]
        }
        cpiMeat1 <- 0
        cpiMeat2 <- 0
      }
      
    } else {       # non advanced settings
      # rent cpi fetcher
      rentData <- bls_api("CWUR0000SAS2RS", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiRent1 <- rentData$name[1]
      cpiRent2 <- rentData$name[2]
      
      # transportation cpi fetcher
      transportData <- bls_api("CUUR0000SAT", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiTransport1 <- transportData$name[1]
      cpiTransport2 <- transportData$name[2]
      
      # medical cpi fetcher
      medicalData <- bls_api("CUUR0000SAM", startyear = 2020, endyear = 2021,"04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiMed1 <- medicalData$name[1]
      cpiMed2 <- medicalData$name[2]
      
      # communication cpi fetcher
      commData <- bls_api("CUUR0000SAE2", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiCom1 <- commData$name[1]
      cpiCom2 <- commData$name[2]
      
      # apparel cpi fetcher
      apparelData <- bls_api("SUUR0000SAA", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiApp1 <- apparelData$name[1]
      cpiApp2 <- apparelData$name[2]
      
      # recreational cpi fetcher
      recData <- bls_api("SUUR0000SAR", startyear = 2020, endyear = 2021, "04b92e5f5d22471f8190da23796fda75") %>%
        group_by(year) %>%
        summarise_at(vars(value), list(name = mean))
      cpiRec1 <- recData$name[1]
      cpiRec2 <- recData$name[2]
      
      
    }
    
    output$personalRate <- renderText({
      # unadjusted total spending
      if (input$advancedSet == 1) {
        unadjTotal <- 0.0
        adjTotal <- 0.0
        
        # get correct food totals
        if (input$eaterType == "Omnivore" || input$eaterType == "Pollotarian" || input$eaterType == "Pescetarian") {
          unadjTotal <- unadjTotal + input$meatInput + input$fruitInput
          
          adjMeat <- as.numeric(input$meatInput) * (as.numeric(cpiMeat1) / as.numeric(cpiMeat2))
          adjFruit <- as.numeric(input$fruitInput) * (as.numeric(cpiFruit1) / as.numeric(cpiFruit2))
          adjTotal <- adjTotal + adjMeat + adjFruit
          
        }
        else if (input$eaterType == "Carnivore") {
          unadjTotal <- unadjTotal + input$meatInput
          
          adjMeat <- as.numeric(input$meatInput) * (as.numeric(cpiMeat1) / as.numeric(cpiMeat2))
          adjTotal <- adjTotal + adjMeat
        }
        else {
          unadjTotal <- unadjTotal + input$fruitInput
          
          adjFruit <- as.numeric(input$fruitInput) * (as.numeric(cpiFruit1) / as.numeric(cpiFruit2))
          adjTotal <- adjTotal + adjFruit
        }
        if (input$milkCheckbox) {
          unadjTotal <- unadjTotal + input$milkInput
          
          adjMilk <- as.numeric(input$milkInput) * (as.numeric(cpiMilk1) / as.numeric(cpiMilk2))
          adjTotal <- adjTotal + adjMilk
        }
        if (input$alcCheckbox) {
          unadjTotal <- unadjTotal + input$alcInput
          
          adjAlc <- as.numeric(input$alcInput) * (as.numeric(cpiAlc1) / as.numeric(cpiAlc2))
          adjTotal <- adjTotal + adjAlc
        }
        if (input$sugarCheckbox) {
          unadjTotal <- unadjTotal + input$sugarInput
          
          adjSugar <- as.numeric(input$sugarInput) * (as.numeric(cpiSugar1) / as.numeric(cpiSugar2))
          adjTotal <- adjTotal + adjSugar
        }
        if (input$fatOilCheckbox) {
          unadjTotal <- unadjTotal + input$fatInput
          
          adjFat <- as.numeric(input$fatInput) * (as.numeric(cpiFat1) / as.numeric(cpiFat2))
          adjTotal <- adjTotal + adjFat
        }
        if (input$cerealCheckbox) {
          unadjTotal <- unadjTotal + input$cerealInput
          
          adjCereal <- as.numeric(input$cerealInput) * (as.numeric(cpiCereal1) / as.numeric(cpiCereal2))
          adjTotal <- adjTotal + adjCereal
        }
        if (cpiEdu1 > 1) {
          unadjTotal <- unadjTotal + input$eduInput
          
          adjEdu <- as.numeric(input$eduInput) * (as.numeric(cpiEdu1) / as.numeric(cpiEdu2))
          adjTotal <- adjTotal + adjEdu
        }
        # cellphone
        unadjTotal <- unadjTotal + input$comInput
        adjCell <- as.numeric(input$comInput) * (as.numeric(cpiCell1) / as.numeric(cpiCell2))
        adjTotal <- adjTotal + adjCell
        # clothes
        unadjTotal <- unadjTotal + input$clothInput
        adjClothes <- as.numeric(input$clothInput) * (as.numeric(cpiCloth1) / as.numeric(cpiCloth2))
        adjTotal <- adjTotal + adjClothes
        # travel
        unadjTotal <- unadjTotal + input$transInput
        adjTravel <- as.numeric(input$transInput) * (as.numeric(cpiTravel1) / as.numeric(cpiTravel2))
        adjTotal <- adjTotal + adjTravel
        # medical
        unadjTotal <- unadjTotal + input$medInput
        adjMedical <- as.numeric(input$medInput) * (as.numeric(cpiMedical1) / as.numeric(cpiMedical2))
        adjTotal <- adjTotal + adjMedical
        # rent
        unadjTotal <- unadjTotal + input$housingInput
        adjRent <- as.numeric(input$housingInput) * (as.numeric(cpiRent1) / as.numeric(cpiRent2))
        adjTotal <- adjTotal + adjRent
        print(paste(cpiRent1))
        
        inflation_rate <- ((unadjTotal - adjTotal) / adjTotal)
        print(paste("Personal Inflation Rate: ", percent(inflation_rate, accuracy = 0.01)))

      }
      else {
        # (New CPI) / (Old CPI) * (Old Price) = Price
        # unadjusted total
        unadjTotal <- (input$housingInput + input$clothInput + input$recInput + input$comInput + input$transInput + input$medInput)
        
        # adjusted
        adjHouse <- as.numeric(input$housingInput) * (as.numeric(cpiRent1) / as.numeric(cpiRent2))
        adjCloth <- as.numeric(input$clothInput) * (as.numeric(cpiApp1) / as.numeric(cpiApp2))
        adjRec <- as.numeric(input$recInput) * (as.numeric(cpiRec1) / as.numeric(cpiRec2))
        adjCom <- as.numeric(input$comInput) * (as.numeric(cpiCom1) / as.numeric(cpiCom2))
        adjMed <- as.numeric(input$medInput) * (as.numeric(cpiMed1) / as.numeric(cpiMed2))
        adjTrans <- as.numeric(input$transInput) * (as.numeric(cpiTransport1) / as.numeric(cpiTransport2))
        adjTotal <- (adjHouse + adjCloth + adjRec + adjCom + adjMed + adjTrans)
        print(paste(adjTotal))
        
        inflation_rate <- ((unadjTotal - adjTotal) / adjTotal)
        print(paste("Personal Inflation Rate: ", percent(inflation_rate, accuracy = 0.01)))
      }
    })
    
    
  })
  
  # US national average
  # output$usRate <- renderText({
  #   paste("U.S. Average Inflation Rate: ", input$clothInput)
  # })
  # 
  # # Weighted Individual Inflation Rate
  # output$weightRate <- renderText({
  #   paste("Your Weighted Individual Inflation Rate: ", input$recInput)
  # })
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

