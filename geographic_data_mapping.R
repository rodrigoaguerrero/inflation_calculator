#Load csv of states found in regional resources of BLS with sub regions

all_states = read.csv("/Users/rodrigoguerrero/Documents/GitHub/inflation_calculator/full_state_list.csv")

print(all_states)

states_w_metro_areas = read.csv("/Users/rodrigoguerrero/Documents/GitHub/inflation_calculator/states_w_metro_areas.csv")

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

ui <- fluidPage(
  selectInput(inputId = "select", 
              label = "Choose your state or territory of residence", 
              choices = full_state_list)
  
  #NEED SOME SORT OF CONDITIONAL PANEL HERE
)

server <- function(input, output, session){
  
}




shinyApp(ui,server = server)

#EXAMPLE

library(shiny)

shinyUI(fluidPage(
  titlePanel("Demonstration of the selectInput UI widget in shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("states","Select the state",all_states)
    ),
    mainPanel()
  )
))
shiny::runApp()

