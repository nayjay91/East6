#
# App to visualize population metrics, business and school metrics and maps of the South Bend districts. 
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Moving South Bend Forward"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "district",
                        "Council Disctrict:",
                        choices = c("Tim Scott",
                                    "Regina Williams",
                                    "Sharon McBride",
                                    "Jo M. Broden",
                                    "Dr. David Varner",
                                    "Oliver Davis",
                                    "All"
                                    ),#this may be fine, but for programing would not want to hard code
                        selected = "All"
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Demographics", verbatimTextOutput("demographics")),
                        tabPanel("Schools and Businesses", verbatimTextOutput("ammenities")),
                        tabPanel("Map", verbatimTextOutput("maps"))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$demographics <- renderPrint("Demographics graphs")
    output$ammenities <- renderPrint("All sorts of public interest graphs")
    output$maps <- renderPrint("coolest map")
}

# Run the application 
shinyApp(ui = ui, server = server)
