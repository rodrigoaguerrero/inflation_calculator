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

        # Main Panel
        
        #selectInput for user to select their state
        
        mainPanel(
            selectInput("state",
                        "Select your state or territory of residence",
                        all_states),
            conditionalPanel(
                condition = "input.state == 'Alaska'",
                selectInput("metro_area", "Select your metro area of residence, if applicable", c("None", "Metro Alaska"),
                ),
                a("Click here to view a map of counties and Metro Areas in Alaska",target="_blank",href="Alaska.pdf"),
                textOutput("AK_txtOutput"),
            ),
            conditionalPanel(
                condition = "input.state == 'Arizona'",
                selectInput("metro_area", "Select your metro area of residence, if applicable", c("None", "Phoenix-Mesa-Scottsdale"),
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
                            c("None", "Philadelphia-Camden-Wilmington"),
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
            ),
            ),
            conditionalPanel(
                condition = "input.state == 'Hawaii'",
                selectInput("metro_area", "Select your metro area of residence, if applicable", 
                            c("None", "Urban Hawaii"),
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
    )
)