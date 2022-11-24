library(shiny)
library(ggplot2)
library(dplyr)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      # Feature 5: Allow user to select multiple types
      uiOutput("typeOutput"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      # Feature 1: This feature shows the number of results to user, so user
      # can decide to narrower or wider their filter range
      textOutput("filterResult"),
      # Feature 4: Add more graphs and allow user to select
      tabsetPanel(
        tabPanel("Distribution of Alcohol Content", plotOutput("coolplot")),
        tabPanel("Distribution of Sweetness", plotOutput("sweetplot")),
        tabPanel("Distribution of Price", plotOutput("priceplot"))
      ),
      # Feature 2: This feature allow user to download their filtered results
      downloadButton("download","Download Results"),
      br(), br(),
      #Feature 3: This feature allow user to interactive the table.
      DT::dataTableOutput("results")
    )
  )
)

server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })  
  # Feature 5:
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product Type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = "WINE")
  })  
  
  filtered <- reactive({
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    if (is.null(input$typeInput)) {
      return(NULL)
    }
    
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  #Feature 4:
  output$priceplot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    ggplot(filtered(), aes(Price, fill = Type)) +
      geom_histogram() +
      xlab("Price(CAD)")
  })
  #Feature 4:
  output$sweetplot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    ggplot(filtered(), aes(Sweetness, fill = Type)) +
      geom_histogram() +
      xlab("Sweetness")
  })
  
  output$coolplot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    ggplot(filtered(), aes(Alcohol_Content, fill = Type)) +
      geom_histogram() +
      xlab("Alcohol Content")
  })
  # Feature 3:
  output$results <- DT::renderDataTable({
    filtered()
  })
  
  # Feature 1:
  output$filterResult<-
    renderText({
      numResults <- nrow(filtered())
      if (is.null(numResults)) {
        numResults<-0
      }
      paste("We found ", numResults, " options for you!")
    })
  
  # Feature 2:
  output$download <- downloadHandler(
    filename = function() {
      "BC_Liquor_Results.csv"
    },
    content = function(file) {
      write.csv(filtered(), file)
    }
  )
}

shinyApp(ui = ui, server = server)