library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Load the data
crime<- read.csv("crimedata_csv_AllNeighbourhoods_2021.csv")
crime<- crime %>% select(- X, - Y)

ui <- fluidPage(
  # Feature 1: I added a shiny theme to make the website look nicer.
  theme = shinytheme("united"),
  titlePanel("Vancouver Crime Data 2021"),
  sidebarLayout(
    sidebarPanel(
      # Feature 2: I added multiple sliderInputs to allow users to manipulate the data.
      sliderInput("monthInput", "Month", 1 , 12, c(2,5)),
      sliderInput("dayInput", "Day", 1, 31, c(10,20)),
      sliderInput("hourInput", "Hour", 0, 23, c(7,16)),
      sliderInput("minInput", "Minute", 0, 59, c(20,30)),
      # Feature 3: The uiOutput function can make user select multiple inputs.
      uiOutput("crimeOutput"),
      uiOutput("areaOutput")
    ),
    mainPanel(
      tabsetPanel(
        # This panel is the introduction of the app.
        tabPanel("Introduction",
                 p("Welcome to the Vancouver crimes visualisation app! To use this app,
        manipulate the widgets on the side to change the filtered results according
        to your preference! You can also download the raw table, graphs and filtered
        table by clicking the download button in each page."),
                 tags$ul(
                   tags$li(tags$b("TYPE"), "The type of crime activities"),
                   tags$li(tags$b("YEAR"), "The year when the reported crime activity occured"),
                   tags$li(tags$b("MONTH"), "The month when the reported crime activity occured"),
                   tags$li(tags$b("DAY"), "The day when the reported crime activity occured"),
                   tags$li(tags$b("HOUR"), "The hour when the reported crime activity occured"),
                   tags$li(tags$b("MINUTE"), "The minute when the reported crime activity occured"),
                   tags$li(tags$b("HUNDRED_BLOCK"), "Generalized location of the reported crime activity"),
                   tags$li(tags$b("NEIGHBOURHOOD"), "Neighbourhoods in Vancouver")
                 ),
                 br(),
                 p("The table below shows the raw data."),
                 tableOutput("head"),
                 #Feature 4: Users can download the raw data.
                 downloadButton("download2","Download Raw Data")),
        # This panel shows the graph of crimes by neighborhoods. Users can download the graph
        # by clicking the button.
        tabPanel("Crimes by Neighbourhood", plotOutput("plot1"),
                 br(),
                 downloadButton("download3","Download Graph")),
        # This panel shows the graph of crimes by types. Users can download the graph
        # by clicking the button.
        tabPanel("Crimes by Type", plotOutput("plot2"),
                 br(),
                 downloadButton("download4","Download Graph")),
        # This panel shows the filtered table. User can download the table by 
        # clicking the button.
        tabPanel("Filtered Table", textOutput("filterResult"),
                 #Feature 5: This feature allows users to interact with the table.
                 DT::dataTableOutput("results"),
                 downloadButton("download1","Download Table")
                 )
      ),
    )
  )
)
server <- function(input, output) {
  output$crimeOutput <- renderUI({
    selectInput("crimeInput", "Crime Type",
                sort(unique(crime$TYPE)),
                multiple = TRUE,
                selected = c("Theft from Vehicle","Break and Enter Commercial"))
  })
  
  output$areaOutput <- renderUI({
    selectInput("areaInput", "Neighbourhood",
                sort(unique(crime$NEIGHBOURHOOD)),
                multiple = TRUE,
                selected = c("West End","Central Business District"))
  })
  
  # This is the raw data table.
  output$head <- renderTable({
    head(crime)
  })
  
  # This is the filtered table.
  filtered <- reactive({
    if (is.null(input$crimeInput)) {
      return(NULL)
    }
    if (is.null(input$areaInput)) {
      return(NULL)
    }
    
    crime %>%
      filter(MONTH >= input$monthInput[1],
             MONTH <= input$monthInput[2],
             DAY >= input$dayInput[1],
             DAY <= input$dayInput[2],
             HOUR >= input$hourInput[1],
             HOUR <= input$hourInput[2],
             MINUTE >= input$minInput[1],
             MINUTE <= input$minInput[2],
             TYPE %in% input$crimeInput,
             NEIGHBOURHOOD %in% input$areaInput
      )
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  })
  
  # This shows the number of filtered results.
  output$filterResult<-
    renderText({
      numResults <- nrow(filtered())
      if (is.null(numResults)) {
        numResults<-0
      }
      paste("We found ", numResults, " crimes in the selected time and neighbourhood!")
    })
  
  # Allow users to download the filtered data.
  output$download1 <- downloadHandler(
    filename = function() {
      "Vancouver_Crime_Results.csv"
    },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
  
  # Allow users to download the raw data.
  output$download2 <- downloadHandler(
    filename = function() {
      "Vancouver_Crime_2021.csv"
    },
    content = function(file) {
      write.csv(crime, file)
    }
  )
  
  # Plot of result group by neighbourhoods.
  neighbour_plot<-reactive({
    if (is.null(input$crimeInput)) {
      return(NULL)
    }
    if (is.null(input$areaInput)) {
      return(NULL)
    }
    crime %>%
      filter(MONTH >= input$monthInput[1],
             MONTH <= input$monthInput[2],
             DAY >= input$dayInput[1],
             DAY <= input$dayInput[2],
             HOUR >= input$hourInput[1],
             HOUR <= input$hourInput[2],
             MINUTE >= input$minInput[1],
             MINUTE <= input$minInput[2],
             TYPE %in% input$crimeInput,
             NEIGHBOURHOOD %in% input$areaInput
      ) %>% group_by(NEIGHBOURHOOD) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = NEIGHBOURHOOD, y = count, fill = NEIGHBOURHOOD)) +
      geom_bar(stat = "identity") + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      geom_text(aes(label = count), position = position_dodge(width = 0.2), vjust = -0.25)
  })
  
  output$plot1 <- renderPlot({
    neighbour_plot()
  })

  # Allow users to download the graph.
  output$download3 <- downloadHandler(
    filename = function() {
      "Crimes By Neighbourhood.png"
    },
    content = function(file) {
      ggsave(file,
             neighbour_plot())
    }
  )
  
  # Plot of result group by crime types.
  type_plot<-reactive({
    if (is.null(input$crimeInput)) {
      return(NULL)
    }
    if (is.null(input$areaInput)) {
      return(NULL)
    }
    crime %>%
      filter(MONTH >= input$monthInput[1],
             MONTH <= input$monthInput[2],
             DAY >= input$dayInput[1],
             DAY <= input$dayInput[2],
             HOUR >= input$hourInput[1],
             HOUR <= input$hourInput[2],
             MINUTE >= input$minInput[1],
             MINUTE <= input$minInput[2],
             TYPE %in% input$crimeInput,
             NEIGHBOURHOOD %in% input$areaInput
      ) %>% group_by(TYPE) %>%
      summarize(count = n()) %>%
      ggplot(aes(x = TYPE, y = count, fill = TYPE)) +
      geom_bar(stat = "identity") + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      geom_text(aes(label = count), position = position_dodge(width = 0.2), vjust = -0.25)
  })
  
  output$plot2 <- renderPlot({
    type_plot()
  })
  
  # Allow users to download the graph.
  output$download4 <- downloadHandler(
    filename = function() {
      "Crimes By Type.png"
    },
    content = function(file) {
      ggsave(file,
             type_plot())
    }
  )
}

shinyApp(ui = ui, server = server)