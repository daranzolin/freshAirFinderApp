library(sf)
library(osrm)
library(mapview)
library(shiny)
library(stringr)
library(sf)
library(waiter)
library(shiny)
library(leaflet)
library(shinyWidgets)

cities <- st_read("https://raw.githubusercontent.com/daranzolin/dailyPurpleAir/main/data/cities.geojson", crs = 3310)
tin_polygons <- st_read("https://raw.githubusercontent.com/daranzolin/dailyPurpleAir/main/data/TIN_polygons.geojson", crs = 3310)
tin_centroids <- st_read("https://raw.githubusercontent.com/daranzolin/dailyPurpleAir/main/data/TIN_centroids.geojson", crs = 3310)

city_choices <- sort(str_to_title(cities$city))
mv_route <- function(...) {
  mapview(..., legend = FALSE, homebutton = FALSE, label = NULL, popup = FALSE)
}

pal <- colorFactor(
  palette = c("green", "yellow", "orange", "red", "purple"),
  domain = c("Good", "Moderate", "Unhealthy for Sensitive", "Unhealthy", "Very Unhealthy")
)

ui <- fluidPage(
  tags$head(tags$style('
                  #container {
                    display: inline;
                  }
                  #title {
                    color: white;
                    font-style: italic;
                    font-family: cursive;
                    text-decoration: overline;
                    text-decoration-style: wavy;
                  }')),
  use_waitress(),
  #baeaff
  #"#dce9f5"
  setBackgroundColor("#bfe4f5"),
  br(),
  h1(id="title", "Fresh Air Finder"),
  fluidRow(
    column(3),
    sidebarPanel(width = 6,
                 selectInput("city", "Your Location:", choices = city_choices),
                 sliderInput("aqiThreshold", "AQI Threshold:",
                             min = 5, max = 250,
                             value = 40, step = 5),
                 actionButton("btn1", "Find Fresh Air",
                              icon = icon("air-freshener"),
                              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 h4("Route Info"),
                 p(id = "container", strong("Distance: "), textOutput("distance", inline = TRUE)),
                 br(),
                 p(id = "container", strong("Duration: "), textOutput("duration", inline = TRUE))
    ),
    column(3)
  ),
  fluidRow(
    column(6,
           h4("Use the inputs to find your route!"),
           mapviewOutput("route"),
           style='border-right: 1px solid white'
    ),
    column(6,
           h4("Bay Area AQI Status:", Sys.Date()),
           leafletOutput("map")
    )
  ),
)

server <- function(input, output) {

  output$map <- renderLeaflet({
    tin_polygons %>%
      st_transform(crs = 4326) %>%
      leaflet() %>%
      addPolygons(color = ~pal(AQI_STATUS), stroke = FALSE) %>%
      addLegend("bottomleft", pal = pal, values = ~AQI_STATUS,
                title = "AQI",
                opacity = 1
      ) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })

  observeEvent(input$btn1, {
    waitress <- Waitress$new("#route")
    waitress$start(h3("Calculating Route..."))
    output$route <- renderMapview({
      start_city <- cities[cities$city == toupper(input$city),]
      fresh_locations <- tin_centroids[tin_centroids$AQI <= input$aqiThreshold,]
      distance_to_fresh_locations <- st_distance(start_city, fresh_locations, by_element = TRUE)
      closest <- fresh_locations[which.min(distance_to_fresh_locations),]
      route <- osrmRoute(src = start_city, dst = closest, overview = "simplified", returnclass = "sf")
      output$duration <- renderText({
        paste("About", round(route$duration), "minutes")
      })
      output$distance <- renderText({
        paste(round(route$distance, 2), "miles")
      })
      mv_out <- mv_route(route) + mv_route(start_city) + mv_route(closest)
      mv_out
    })
    waitress$close()
    mapviewOutput("route")
  })

  output$route = renderUI({
    if ("plot" %in% input$checkboxes) {
      plotOutput(outputId = "plotdf1")
    }
  })
}

shinyApp(ui = ui, server = server)
