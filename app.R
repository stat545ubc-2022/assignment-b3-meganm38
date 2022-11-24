library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  # Feature 7: I added a shiny theme to make the website look nicer.
  theme = shinytheme("superhero"),
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      # Feature 1: I added an uiOutput to allow user to select multiple types, 
      # this is useful since users might want to search all types of liquor.
      uiOutput("typeOutput"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      # Feature 2: This feature shows the number of results to users, so users
      # can decide to narrow down or widen their filter range.
      textOutput("filterResult"),
      # Feature 3: I added a tabsetpanel and multiple tabpanels to allow users to 
      # select the specific graph they want to see.
      tabsetPanel(
        tabPanel("Distribution of Alcohol Content", plotOutput("coolplot")),
        tabPanel("Distribution of Sweetness", plotOutput("sweetplot")),
        tabPanel("Distribution of Price", plotOutput("priceplot"))
      ),
      # Feature 4: This feature allows users to download their filtered results;
      # this is useful because they can bring the downloaded sheet to buy liquors
      # they want.
      downloadButton("download","Download Results"),
      br(), br(),
      #Feature 5: This feature allows users to interactive the table. This is useful
      # since users can search the results and choose the number of results showing 
      # in one page.
      DT::dataTableOutput("results")
    )
  )
)

# Feature 6: This feature allows users to select multiple countries if they like.
server <- function(input, output) {
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                multiple = TRUE,
                selected = c("CANADA","UNITED STATES OF AMERICA"))
  })  
  # Feature 1: I set the default selected to "WINE", because otherwise the graph and result 
  # will be blank.
  output$typeOutput <- renderUI({
    selectInput("typeInput", "Product Type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("WINE","BEER"))
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
             Type %in% input$typeInput,
             Country %in% input$countryInput
      )
  })
  #Feature 3: This is the price distribution plot.
  output$priceplot <- renderPlot({
    if (is.null(filtered())) {
      return(NULL)
    }
    ggplot(filtered(), aes(Price, fill = Type)) +
      geom_histogram() +
      xlab("Price(CAD)")
  })
  #Feature 3: This is the sweetness distribution plot.
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
  # Feature 5: I used the DT package to create an interactive table for this app.
  output$results <- DT::renderDataTable({
    filtered()
  })
  
  # Feature 2: 
  output$filterResult<-
    renderText({
      numResults <- nrow(filtered())
      if (is.null(numResults)) {
        numResults<-0
      }
      paste("We found ", numResults, " options for you!")
    })
  
  # Feature 4: I used downloadHandler to allow users to download the result they need.
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