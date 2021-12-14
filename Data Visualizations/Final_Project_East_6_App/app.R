#
# App to visualize population metrics, business and school metrics and maps of the South Bend districts. 
#

#setwd('~/notre-dame/data_viz/East6/Data Visualizations/Final_Project_East_6_App/')

library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(tidyverse)

# loads in all data and builds summary tables 
source('./read_data.R')


 ui <- fluidPage(

    sidebarLayout(
        sidebarPanel(width = 200, 
            selectInput(inputId = "district",
                        "Council Disctrict:",
                        choices = demographics_sf$Council_Me,
                        selected = "All"
                        )
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Demographics", fluidPage(htmlOutput('density'),
                                                           plotOutput("renters"),
                                                           plotOutput("age"))),
                        
                        tabPanel("Commercial and Public Entities",
                                 plotOutput("business"),
                                 plotOutput("facilities"),
                                 plotOutput("parks"),
                                 plotOutput("schools")),
                        
                        tabPanel("Map of South Bend", 
                                 radioButtons(inputId = "map_density", "Show Population Density", choices = c("Yes","No"), inline = TRUE), 
                                 checkboxGroupInput(inputId = "map_bus_type", "Filter Business Types", inline = TRUE, 
                                                    choices = c('Food','Retail','Service','Entertainment'), 
                                                    selected = c('Food','Retail','Service','Entertainment')),
                                 leafletOutput(outputId = "map", width = "100%", height = 800))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
     # Use this theme for all plots 
    theme_update(plot.title = element_text(hjust = 0.5,size = 24))

    ### Demographics Tab - Nathan
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
                                        `Population Count` = value) %>% 
                                 ggplot(aes(x=`Age Group`, y=`Population Count`)) +
                                 geom_bar(width = 1,stat = 'identity', fill = '#00BFC4', color = 'white') + 
                                 scale_color_brewer(palette = "PuOr") + ggtitle("Distribution of South Bend Residents' Ages") 
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
                                     theme_void()  + ggtitle("Residences Owned vs. Rented") + theme(plot.title = element_text(size = 24))
                                )
    
    output$density <- renderText(paste("<p style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:37px;line-height:107%;\">Population density is</span></p>
<p id=\"isPasted\" style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:120px;line-height:107%;color:#00BFC4;\">",
                                    demographics_sf %>%
                                        as_tibble() %>%
                                        filter(Council_Me == input$district) %>%
                                        pull(pop_density) %>% 
                                        round(),"</span></p>
                                                                <p id=\"isPasted\" style='margin-top:0in;margin-right:0in;margin-bottom:8.0pt;margin-left:0in;line-height:107%;font-size:15px;font-family:\"Calibri\",sans-serif;'><span style=\"font-size:37px;line-height:107%;\">Per Square Mile</span></p>"))

    
    ### Commercial and Public Entities Tab - Shannon
    output$business <- renderPlot(bus_summary %>% 
                                     as_tibble() %>% 
                                      {if (input$district == 'All') as_tibble(bus_summary) else filter(as_tibble(bus_summary),Council_Me == input$district)}
                                  %>% 
                                     mutate(`Business Type` = factor(business_type, ordered = TRUE, 
                                                                  levels = c('Food',
                                                                             'Retail',
                                                                             'Service',
                                                                             'Entertainment',
                                                                             'Transportation',
                                                                             'Charitable',
                                                                             'Unclassified')),
                                            `Business Count` = n) %>%
                                      ggplot(aes(x=`Business Type`, y=`Business Count`)) +
                                      geom_bar(width = 0.8,position = position_dodge(width=0.2),stat = 'identity', fill = '#00BFC4') + 
                                      scale_color_brewer(palette = "PuOr") + ggtitle("Number of Businesses by Business Type")

    )
    
    output$facilities <- renderPlot(public_facilities_summary %>% 
                                      as_tibble() %>% 
                                        {if (input$district == 'All') as_tibble(public_facilities_summary) else filter(as_tibble(public_facilities_summary),Council_Me == input$district)} %>% 
                                      mutate(`Facilties Type` = POPL_TYPE,
                                             `Facilities Count` = n) %>%
                                      ggplot(aes(x=`Facilties Type`, y=`Facilities Count`)) +
                                      geom_bar(width = 0.8,position = position_dodge(width=0.2),stat = 'identity', fill = '#00BFC4') + 
                                      scale_color_brewer(palette = "PuOr") + ggtitle("Number of Public Facilities by Facility Type") 
                                    )
    
    output$parks <- renderPlot(parks_summary %>% 
                                      as_tibble() %>% 
                                   {if (input$district == 'All') as_tibble(parks_summary) else filter(as_tibble(parks_summary),Council_Me == input$district)} %>% 
                                      mutate(`Parks Type` = Park_Type,
                                             `Parks Count` = n) %>%
                                      ggplot(aes(x=`Parks Type`, y=`Parks Count`)) +
                                      geom_bar(width = 0.8,position = position_dodge(width=0.2),stat = 'identity', fill = '#00BFC4') + 
                                      scale_color_brewer(palette = "PuOr") + ggtitle("Number of Parks by Park Type") 
                               )
    
    output$schools <- renderPlot(school_summary %>% 
                                      as_tibble() %>% 
                                     {if (input$district == 'All') as_tibble(school_summary) else filter(as_tibble(school_summary),Council_Me == input$district)} %>% 
                                      mutate(`School Type` = SchoolType,
                                             `School Count` = n) %>%
                                      ggplot(aes(x=`School Type`, y=`School Count`)) +
                                      geom_bar(width = 0.8,position = position_dodge(width=0.2),stat = 'identity', fill = '#00BFC4') + 
                                      scale_color_brewer(palette = "PuOr") + ggtitle("Number of Private and Public Schools") 
                                 )
    
    
    ## Reactive spatial data for map tab 
    filtered_demographics_sf <- reactive({
        if (input$district == 'All') filter(demographics_sf, Dist != 0)  else filter(demographics_sf, Council_Me == input$district)
    })
    
    filtered_businesses <- reactive({
        bus_in_district_sf %>% filter(business_type %in% input$map_bus_type)
    })
    
    ### Map Tab - Eli
    output$map <- renderLeaflet({
        
        leaflet(options = leafletOptions(minZoom = 11, maxZoom = 17)) %>%
            addTiles() %>% 
            addPolygons(data = filtered_demographics_sf(), 
                        fillColor = ~pop_pal(pop_density),
                        fillOpacity = if (input$map_density == 'Yes') .7 else ~0) %>%
            ## Businesses
            addCircleMarkers(group = "Businesses", 
                             data = if (input$district == 'All') filtered_businesses() else filter(filtered_businesses(), Council_Me == input$district), 
                             color = "#CC6677", radius = 2, opacity = .5, popup = ~paste0("Name: ",Business_N, "<br>Type: ", business_type)) %>% 
            ## Facilities
            addCircleMarkers(group = "Facilities", 
                             data = if (input$district == 'All') public_facilities_sf else filter(public_facilities_sf, Council_Me == input$district), 
                             color = "#332288", radius = 2, opacity = .5, popup = ~paste0("Name: ",POPL_NAME, "<br>Type: ", POPL_TYPE)) %>% 
            ## Parks
            addCircleMarkers(group = "Parks", 
                             data = if (input$district == 'All') parks_sf else filter(parks_sf, Council_Me == input$district), 
                             color = "#117733", radius = 2, opacity = .5, popup = ~paste0("Name: ",Park_Name, "<br>Type: ", Park_Type)) %>% 
            ## Schools 
            addCircleMarkers(group = "Schools", 
                             data = if (input$district == 'All') school_boundaries_sf else filter(school_boundaries_sf, Council_Me == input$district), 
                             color = "#999933", radius = 2, opacity = .5, popup = ~paste0("Name: ",School, "<br>Type: ", SchoolType)) %>% 
            
            addLayersControl(overlayGroups = c("Businesses", 'Facilities', "Parks", 'Schools'), 
                             options = layersControlOptions(collapsed = FALSE)) %>% 
            addLegendCustom(colors = c("#CC6677", "#332288", "#117733", "#999933"), 
                            labels = c("Businesses", "Facilities", "Parks", "Schools"), 
                            sizes = c(10, 10, 10, 10), position = "bottomright") %>% 
            addLegend(group = 'dens_legend', position = 'bottomleft', title = "Pop Per Sq. Mile",
                      pal = pop_pal, values = c('1000', '1500', '2000', '2500', '3000', '3500', '4000', '4500')) 
        
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
