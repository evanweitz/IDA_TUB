#check for required packages

if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(leaflet)) {
  install.packages("leaflet")
  require(leaflet)
}

if (!require(DT)) {
  install.packages("DT")
  require(DT)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

# Define UI for application
ui <- fluidPage(
    titlePanel("Affected Vehicles in Germany"),
    
    #asks to show the entire map with popup labels
    checkboxInput(inputId = "all",
                  label = "Show all affected vehicles",
                  value = FALSE),
    
    #shows map
    leafletOutput("map"),
    br(),
    
    #this conditional panel triggers if the box is unchecked (default)
    #and asks for numeric input for number of circles and their radii
    #then it displays the bargraph and table with relevant information
    conditionalPanel(condition = "input.all == false",
                     fluidRow(
                       column(3,
                              #user defines the number of circles they want to input
                              numericInput(inputId = "circles",
                                           label = "Number of concentric areas:", min = 1, value = 1
                                           ),
                              actionButton("submit", "Submit Radii")
                       ),
                       column(3,
                              #outputs the fields for radii input
                              uiOutput("radii_list") 
                       ),
                       column(6,
                              tabsetPanel(
                                #shows the bar graph
                                tabPanel("Graph",
                                         br(),
                                         plotlyOutput("bargraph")
                                ),
                                #shows the DataTable with the affected municipalities
                                tabPanel("Table",
                                         br(),
                                         DTOutput("aff_mun")
                                )
                              ) #end tabs
                        ) #end columns
                     )) #end conditional panel

) #end ui

# Define server logic
server <- function(input, output) {
    
    #outputs UI element: fields for radii input
    output$radii_list <- renderUI({
        num_circles <- as.integer(input$circles)
        lapply(1:num_circles, function(i) { #creates new input variable for each field with name radii[i]
            numericInput(inputId = paste0("radii", i),
                         label = paste("Radius of circle", i, "(km)"), min = 0, value = 0)
        })
    })
    
    #creates a reactive list for each radius input, which updates when the button is pushed
    #and a list for the municipalities within each circle
    radii_group <- reactiveValues(x = NULL)
    list_of_munis_in_circles <- reactiveValues(x =NULL)
    
    #initializes a list of all affected municipalities with a "null point" that prevents addCircleMarkers from bugging
    affected_munis <- reactiveValues(
      x = data.frame(municipality = "", count = 0, latitude = 52.518623, longitude = 13.376198, distance_Berlin = 0, opacity = 0))

    #nubmer_submitted takes the default value of 1 input circle and recalculates whenever the submit button is pushed
    #necessary to avoid a bug in circle and plot generation, so they only regenerate when the button is pushed
    number_submitted <- eventReactive(input$submit, {input$circles}, ignoreNULL = FALSE)
    
    #when the submit button is pressed, function takes the radii inputs and puts them into a list
    #then compares the cars_by_muni dataframe and creates a new list of dataframes
    observeEvent(input$submit, {
      lapply(1:number_submitted(), function(i) {
          radii_group$x[[i]] <- input[[paste0("radii", i)]] #this is a list of the input radii
          list_of_munis_in_circles$x[[i]] <- cars_by_muni %>%
            filter(distance_Berlin <= radii_group$x[[i]]) #filtered dataframe of municipalities within each radius
      })
      
      #calculations for table and bar graphs   
      circle_name <- reactive({c(1:number_submitted())}) #names the circles, in case they are not monotonically increasing
      radius_size <- reactive({sapply(1:number_submitted(), function(i) {
          radii_group$x[[i]]
        })
      })
      
      #outputs numeric value, total number affected within each circle
      num_affected <- reactive({as.numeric(
        sapply(1:number_submitted(), function(i) {
          list_of_munis_in_circles$x[[i]] %>%
            summarise(count = sum(count))
        })) 
      })
      
      #calculates affected municipalities for addCircleMarkers
      affected_munis$x <- cars_by_muni %>%
        filter(distance_Berlin <= max(radius_size())) %>% #filters out by the largest circle
        mutate(opacity = count/(25 + count)) #adds opacity column for non-null points proportionate to size
      
      #table of municipalities within the circles, selecting relevant info for display
      output$aff_mun <- renderDT({
        datatable(
          affected_munis$x %>%
            select(municipality, count, distance_Berlin) %>%
            mutate(distance_Berlin = round(distance_Berlin, 1)),
          colnames = c("Municipality", "Number of Affected Vehicles", "Distance from Berlin (km)")
        )
      })
        
      #bar graph, one bar per circle
      output$bargraph <- renderPlotly({
        plot_ly(
          x = as.factor(radius_size()),
          y = num_affected(),
          type = "bar",
          marker = list(color = 'rgb(158,202,225)',
              line = list(color = 'rgb(8,48,107)', width = 1.5))
        ) %>%
          layout(
            title = "Number of Affected Vehicles",
            xaxis = list(
              title = "Radius of Recall (km)")
          ) %>%
          config(
            displayModeBar = FALSE
          )
          
      }) #end plotly bargraph
     }) #end submit-triggered events
    

    #outputs maps
    output$map <- renderLeaflet({

        #if the box is checked output a map of all municipalities with circles proportional
        # to the number of cars affected
        if (input$all == TRUE){ 
        leaflet() %>% 
            setView(lng = 10.283810, lat = 51.432020, zoom = 7) %>%
            addCircleMarkers(
                lng = cars_by_muni$longitude, 
                lat = cars_by_muni$latitude, 
                radius = (cars_by_muni$count / 25), #scaled for count
                color = "Navy",
                label = paste(cars_by_muni$municipality, cars_by_muni$count, sep = ": ")
                ) %>%
            addProviderTiles("Stamen.TonerLite")
        } else { #map with circles and their radii
        leaflet() %>% 
            setView(lng = 13.376198, lat = 52.518623, zoom = 7) %>% 
            addCircles( #these are the circles centered at Berlin Reichstaggebauede
                lng = 13.376198,
                lat = 52.518623,
                stroke = FALSE,
                radius = lapply(1:number_submitted(), function(i){
                  #for each circle, accesses the corresponding item in radii_group
                  radii_group$x[[i]] * 1000 #converted to km from meters 
                }),
                color = "blue"
                ) %>%
            addCircleMarkers( #these are the municipalities within the circles
                lng = affected_munis$x[["longitude"]],
                lat = affected_munis$x[["latitude"]],
                radius= log(affected_munis$x[["count"]]) + affected_munis$x[["count"]]/120 + 1, #scales for count
                color = "red",
                weight = 1,
                opacity = affected_munis$x[["opacity"]], #scaled for count
                label = paste(affected_munis$x[["municipality"]], affected_munis$x[["count"]], sep = ": ")
                ) %>%
            addProviderTiles("Stamen.TonerLite")
        }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)