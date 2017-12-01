library(maps)
library(mapproj)
library(shiny)
#source("helpers.R")
#load("cleanData.Rda")
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      selectInput("abOrDif",
        label = "Year Display Type",
        choices = c("Absolute","Difference")),
      selectInput("eleSele", 
        label = "Weather element",
        choices = c("Precipitation",
                    "Average Temperature", 
                    "PDSI",
                    "PHDI",
                    "ZNDX",
                    "PMDI",
                    "Heating Degree Days",
                    "Cooling Degree Days",
                    "Maximum Temperature",
                    "Minimum Temperature",
                    "1-month Standardized Precipitation Index",
                    "2-month Standardized Precipitation Index",
                    "3-month Standardized Precipitation Index",
                    "6-month Standardized Precipitation Index",
                    "9-month Standardized Precipitation Index",
                    "12-month Standardized Precipitation Index",
                    "24-month Standardized Precipitation Index")),
      selectInput("Month", 
                  label = "Month",
                  choices = c("January", "February", "March", "April",  "May",  "June", "July", "August", "September", "October", "November", "December")),

      sliderInput("AbsoluteYear",h3("Absolute Year"),min=1900,max = 2017,value = 2017),
      sliderInput("DifferenceYear",h3("Difference Year"), min =1900,max = 2017,value = c(1900,2017))), 
      
  
    mainPanel( h3("test"),plotOutput("map"))
  )
)
# Define server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    #input element type
    cleanDataTemp<-cleanData[cleanData$Year==input$AbsoluteYear,]
    args <- switch(input$eleSele,
                   "Precipitation"= list(cleanDataTemp[cleanDataTemp$Element=="Precipitation",c(input$Month,"State")],"blue", "Precipitation Stats"),
                   "Average Temperature"= list(cleanDataTemp[cleanDataTemp$Element=="Average Temperature",c(input$Month,"State")],"blue", "Average Temperature Stats"), 
                   "PDSI"= list(cleanDataTemp[cleanDataTemp$Element=="PDSI",c(input$Month,"State")],"blue", "PDSI Stats"),
                   "PHDI"= list(cleanDataTemp[cleanDataTemp$Element=="PHDI",c(input$Month,"State")],"blue", "PHDI Stats"),
                   "ZNDX"= list(cleanDataTemp[cleanDataTemp$Element=="ZNDX",c(input$Month,"State")],"blue", "ZNDX Stats"),
                   "PMDI"= list(cleanDataTemp[cleanDataTemp$Element=="PMDI",c(input$Month,"State")],"blue", "PMDI Stats"),
                   "Heating Degree Days"= list(cleanDataTemp[cleanDataTemp$Element=="Heating Degree Days",c(input$Month,"State")],"blue", "Heating Degree Day Stats"),
                   "Cooling Degree Days"= list(cleanDataTemp[cleanDataTemp$Element=="Cooling Degree Days",c(input$Month,"State")],"blue", "Cooling Degree Day Stats"),
                   "Maximum Temperature"= list(cleanDataTemp[cleanDataTemp$Element=="Maximum Temperature",c(input$Month,"State")],"blue", "Maximum Temperature Stats"),
                   "Minimum Temperature"= list(cleanDataTemp[cleanDataTemp$Element=="Minimum Temperature",c(input$Month,"State")],"blue", "Minimum Temperature Stats"),
                   "1-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="1-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "1-month Standardized Precipitation Index Stats"),
                   "2-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="2-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "2-month Standardized Precipitation Index Stats"),
                   "3-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="3-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "3-month Standardized Precipitation Index Stats"),
                   "6-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="6-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "6-month Standardized Precipitation Index Stats"),
                   "9-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="9-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "9-month Standardized Precipitation Index Stats"),
                   "12-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="12-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "12-month Standardized Precipitation Index Stats"),
                   "24-month Standardized Precipitation Index"= list(cleanDataTemp[cleanDataTemp$Element=="24-month Standardized Precipitation Index",c(input$Month,"State")],"blue", "24-month Standardized Precipitation Index Stats"))
    args$min<-0
    args$max<-100
    do.call(percent_map,args)
    
  })
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)