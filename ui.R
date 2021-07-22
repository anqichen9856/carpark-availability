library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)

locations <- read.csv("data/data-processed/locations.csv")
carpark_types <- c('HDB Carparks','Shopping Mall/Hotel/Tourist Attraction Carparks')
relevant_info <- sort(unique(locations$category))
locations <- as.data.table(locations$name) 
names(locations) <- "Type or Select Location"

sg_area <- as.data.table(c("Ang Mo Kio", "Bedok", "Bishan", "Boon Lay", "Bukit Batok", "Bukit Merah", 
                           "Bukit Panjang", "Bukit Timah", "Central Water Catchment", "Changi", 
                           "Choa Chu Kang", "Clementi", "City", "Geylang", "Hougang", "Jalan Bahar",
                           "Jurong East", "Jurong Island", "Jurong West", "Kallang", "Lim Chu Kang",
                           "Mandai", "Marine Parade", "Novena", "Pasir Ris", "Paya Lebar", "Pioneer",
                           "Pulau Tekong", "Pulau Ubin", "Punggol", "Queenstown", "Seletar", "Sembawang",
                           "Sengkang", "Sentosa", "Serangoon", "Southern Islands", "Sungei Kadut", "Tampines",
                           "Tanglin", "Tengah", "Toa Payoh", "Tuas", "Western Islands", "Western Water Catchment",
                           "Woodlands", "Yishun"))
names(sg_area) <- "Type or Select Region"

header <- dashboardHeader(title = "DBA3702 SA1 Team 7")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Reaching Destination', tabName='reaching', icon=icon('parking')),
    menuItem('Planning Before Departure', tabName='planning', icon=icon('map-marked-alt')),
    menuItem('2-Hour Weather Forecast', tabName='weather', icon=icon('cloud')),
    menuItem('About This App', tabName='about', icon=icon('info-circle'))
  )
)

body <- dashboardBody(
  tabItems(
    # Tab 1: Reaching Destination
    tabItem(tabName = 'reaching',
            fluidRow(
              column(width=8,
                     box(width = NULL, status="primary", leafletOutput('map_reaching', height=514))
              ),
              column(width=4,
                     box(width=NULL, height=NULL, status="warning",
                         selectizeInput('dest_reaching', 'Destination:',
                                        choices=locations, 
                                        multiple=TRUE,
                                        options=list(placeholder='e.g. VivoCity', maxItems=1)),
                         checkboxGroupInput('carpark_type', "Carpark Type:",
                                            choices=carpark_types),
                         checkboxGroupInput('relevant_info', "Show Nearby:",
                                            choices=relevant_info)
                     )
                )
          ),
          fluidRow(
            column(width=12,
                   box(width=NULL, height=NULL, status="info",
                       title="Tick the checkboxes on the right to see relevant information:",
                       htmlOutput('capitaland_lots'),
                       htmlOutput('result_text')
                       ))
          )
    ),

    # Tab 2: Planning Before Departure
    tabItem(tabName = 'planning',
            fluidRow(
              column(width=8,
                     box(width = NULL, status="primary", leafletOutput('map_planning', height=514))
              ),
              column(width=4,
                     box(width=NULL, height=NULL, status="warning",
                         textInput("start_loc", "Starting Point:", placeholder="e.g. The Mayfair"),
                         selectizeInput('dest_planning', 'Destination:',
                                        choices=locations,
                                        multiple=TRUE,
                                        options=list(placeholder='e.g. VivoCity', maxItems=1)),
                         checkboxGroupInput('carpark_type2', "Carpark Type:", choices=carpark_types),
                         htmlOutput('timetaken')
                     ))

            ),
            fluidRow(
              column(width=12,
                     box(width=NULL, height=NULL, status="info",
                         title="Tick the checkboxes on the right to see relevant information:",
                         htmlOutput('capitaland_lots2'),
                         htmlOutput('result_text2')
                     ))
            )
    ),

    
    #Tab 3: 2 Hour Weather Forecast
    tabItem(tabName = 'weather',
            fluidRow(
              column(width=8,
                     box(width = NULL, status="primary", leafletOutput('map_forecast', height=600))
              ),
              column(width=4,
                    box(width=NULL, height=NULL, status="warning",
                    selectizeInput('forecast', 'Weather Forecast for Area:',
                                    choices=sg_area,
                                    multiple=TRUE,
                                    options=list(placeholder='e.g. Bedok', maxItems=1)),
                         htmlOutput('weathertext'),
                         htmlOutput('result_text3')
                     ))
            )
    ),
    
    
    # Tab 4: About This App
    tabItem(tabName = 'about', uiOutput('about'))
  )
)

dashboardPage(header, sidebar, body, skin='red')
