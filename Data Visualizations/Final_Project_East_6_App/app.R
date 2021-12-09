#
# App to visualize population metrics, business and school metrics and maps of the South Bend districts. 
#

#setwd('~/notre-dame/data_viz/East6/Data Visualizations/Final_Project_East_6_App/')

library(shiny)
library(leaflet)
library(dplyr)
library(sf)

# loads in all data and builds summary tables 
source('./read_data.R')

## Function for custom legends 
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, position){
    colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
    labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                             sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                             labels, "</div>")
    
    return(addLegend(map, colors = colorAdditions, 
                     labels = labelAdditions, opacity = opacity, position = position))
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Moving South Bend Forward"),

    
    headerPanel(
            selectInput(inputId = "district",
                        "Council Disctrict:",
                        choices = demographics_sf$Council_Me,
                        selected = "All"
            )
        ),
    mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("demographics", fluidPage(plotOutput("renters"),
                                                       plotOutput("age"),
                                                       HTML("<p style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:37px;line-height:107%;\">Population density is</span></p>
<p id=\"isPasted\" style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:120px;line-height:107%;color:#2E75B6;\">Test</span></p>
                                                                <p id=\"isPasted\" style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:37px;line-height:107%;\">Per Square Mile</span></p>"))),
                    tabPanel("schools and businesses", verbatimTextOutput("ammenities")),
                    tabPanel("Map", leafletOutput(outputId = "map"))
        )
    )
    
    ## second layout option (highlight + ctrl shift C to uncomment)
    # sidebarLayout(
    #     sidebarPanel(
    #         selectInput(inputId = "district",
    #                     "Council Disctrict:",
    #                     choices = c("Tim Scott",
    #                                 "Regina Williams",
    #                                 "Sharon McBride",
    #                                 "Jo M. Broden",
    #                                 "Dr. David Varner",
    #                                 "Oliver Davis",
    #                                 "All"
    #                     ),#this may be fine, but for programming would not want to hard code
    #                     selected = "All"
    #                     )
    #     ),
    #     mainPanel(
    #         tabsetPanel(type = "tabs",
    #                     tabPanel("demographics", verbatimTextOutput("demographics")),
    #                     tabPanel("schools and businesses", verbatimTextOutput("ammenities")),
    #                     tabPanel("map", verbatimTextOutput("maps"))
    #         )
    #     )
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$demographics <- renderPrint("Demographics graphs")
    output$age <- renderPlot(demographics_sf %>% 
                                 as_tibble() %>% 
                                 filter(Council_Me == input$district) %>% 
                                 select(starts_with("age"), Council_Me) %>% 
                                 rename('Under 5' = 'age_u5',
                                        '5-9' = 'age_5_9',
                                        '11-14' = 'age_11_14',
                                        '15-17' = 'age_15_17',
                                        '18-24' = 'age_18_24',
                                        '25-34' = 'age_25_34',
                                        '35-44' = 'age_35_44',
                                        '45-54' = 'age_45_54',
                                        '55-64' = 'age_55_64',
                                        '65-74' = 'age_65_74',
                                        '75-84' = 'age_75_84',
                                        'Over 85' = 'age_o85') %>%
                                 pivot_longer(cols = -Council_Me) %>% 
                                 mutate(`Age Group` = factor(name, ordered = TRUE, 
                                               levels = c('Under 5',
                                                          '5-9',
                                                          '11-14',
                                                          '15-17',
                                                          '18-24',
                                                          '25-34',
                                                          '35-44',
                                                          '45-54',
                                                          '55-64',
                                                          '65-74',
                                                          '75-84',
                                                          'Over 85')),
                                        `Pupulation Count` = value) %>% 
                                 ggplot(aes(x=`Age Group`, y=`Pupulation Count`)) +
                                 geom_bar(width = 1,stat = 'identity')
                             )
    output$renters <- renderPlot(demographics_sf %>% 
                                     as_tibble() %>% 
                                     filter(Council_Me == input$district) %>% 
                                     rename('Owner Occupied' = 'owner_occupied','Renter Occupied' = 'renter_occupied') %>% 
                                     pivot_longer(cols = c('Owner Occupied','Renter Occupied'),
                                                  names_to = "Residence Type")  %>% 
                                     group_by(`Residence Type`) %>% 
                                     summarize(total = sum(value)) %>% 
                                     mutate(prop = total/sum(total)) %>% 
                                     arrange(desc(`Residence Type`)) %>%
                                     mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
                                     ggplot(aes(x="", y=prop, fill=`Residence Type`)) +
                                     geom_bar(width = 1, stat = "identity", color = "white") +
                                     geom_text(aes(y = lab.ypos, label = total), color = "white") +
                                     coord_polar("y", start=0) +
                                     theme_void()               
                                )
    #output$density <- render
    output$ammenities <- renderPrint("All sorts of public interest graphs")
    
    output$map <- renderLeaflet({
        ## TODO: 
        ## [] Population density layer (& fix All polygons)
        ## [] Fix colors... add symbols? 
        ## [] Filter by types?? separate layer controls?
        
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 17)) %>%
            addTiles() %>% 
            addPolygons(data = filter(demographics_sf, Council_Me == input$district), fill = 0) %>%
            ## Businesses
            addCircleMarkers(group = "Businesses", 
                             data = if (input$district == 'All') bus_in_district_sf else filter(bus_in_district_sf, Council_Me == input$district), 
                             color = "red", radius = 2, opacity = .5, popup = ~paste0("Name: ",Business_N, "<br>Type: ", business_type)) %>% 
            ## Facilities
            addCircleMarkers(group = "Facilities", 
                             data = if (input$district == 'All') public_facilities_sf else filter(public_facilities_sf, Council_Me == input$district), 
                             color = "black", radius = 2, opacity = .5, popup = ~paste0("Name: ",POPL_NAME, "<br>Type: ", POPL_TYPE)) %>% 
            ## Parks
            addCircleMarkers(group = "Parks", 
                             data = if (input$district == 'All') parks_sf else filter(parks_sf, Council_Me == input$district), 
                             color = "green", radius = 2, opacity = .5, popup = ~paste0("Name: ",Park_Name, "<br>Type: ", Park_Type)) %>% 
            ## Schools 
            addCircleMarkers(group = "Schools", 
                             data = if (input$district == 'All') school_boundaries_sf else filter(school_boundaries_sf, Council_Me == input$district), 
                             color = "pink", radius = 2, opacity = .5, popup = ~paste0("Name: ",School, "<br>Type: ", SchoolType)) %>% 
            
            addLayersControl(overlayGroups = c("Businesses", 'Facilities', "Parks", 'Schools'), 
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            addLegendCustom(colors = c("red", "black", "green", "pink"), 
                            labels = c("Businesses", "Facilities", "Parks", "Schools"), 
                            sizes = c(10, 10, 10, 10), position = "bottomright")
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
