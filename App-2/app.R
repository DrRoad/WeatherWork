#2nd Attempt
library(maps)
library(mapproj)
library(shiny)
library(ggplot2)
load("FlightDays.Rda")

point.cloud <- function(long, lat, fdays) {
  pointsLat <- rnorm(1000, mean = lat, sd = .5)
  pointsLong <- rnorm(1000, mean = long, sd = .5)
  distance <- sqrt((pointsLat - lat) ^ 2 + (pointsLong - long) ^ 2)
  points <- data.frame(pointsLong, pointsLat, distance)
  return(geom_point(
    data = points,
    aes(x = pointsLong, y = pointsLat),
    alpha = -abs(tanh(distance)) / 2 + .5
  ))
}

point.radar <- function(long, lat, fDays) {
  rad.length = 50
  distances <- matrix(seq(from = .1, to = 3, by = .6), nrow = 1)
  radians <- matrix(seq(0, 2 * pi, length = rad.length), ncol = 1)
  pointsLat <- c(sin(radians) %*% distances + lat)
  pointsLong <- c(cos(radians) %*% distances * 1.3 + long)
  distance <-
    c(1 - (matrix(1, rad.length, 1) %*% distances) / (max(distances) + 1))
  return(points <-
           data.frame(
             pointsLong,
             pointsLat,
             distance,
             flight.days = matrix(fDays, length(pointsLat), 1)
           ))
}




ui <- fluidPage(titlePanel("Forraging Days by Year"),
                sidebarLayout(sidebarPanel(
                  sliderInput(
                    "Year",
                    h3("Year"),
                    min = 1900,
                    max = 1999,
                    value = c(1999)
                  )
                ),
                
                
                mainPanel(plotOutput("map"))))
# Define server logic ----
server <- function(input, output) {
  State <- map_data("state")
  G <- ggplot() + geom_polygon(
    data = State,
    aes(x = long, y = lat, group = group),
    fill = "grey96",
    color = "grey45"
  ) + coord_fixed(1.3, xlim = c(-122.5,-68), ylim = c(25, 50)) + theme_bw() + scale_color_gradient2(
    low = "black",
    mid = "blue",
    high = "red",
    midpoint = 260,
    limits = c(150, 366)
  )
  dat <- list()
  FlightDays$Year <- as.double(FlightDays$Year)
  for (yeari in seq(from = 1900, to = 1999, by = 1)) {
    subset <- FlightDays[FlightDays$Year == yeari, ]
    df <-
      mapply(point.radar,
             subset$Longitude,
             subset$Latitude,
             subset$`Flight Days`)
    df <-
      data.frame(
        Long = unlist(df[1, ]),
        Lat = unlist(df[2, ]),
        distance = unlist(df[3, ]),
        fdays = unlist(df[4, ])
      )
    dat[[yeari]] <- df
  }
  output$map <- renderPlot({
    G + geom_point(
      data = dat[[input$Year]],
      aes(x = Long, y = Lat, color = fdays),
      alpha = dat[[input$Year]]$distance,
      size = .00001
    )
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)