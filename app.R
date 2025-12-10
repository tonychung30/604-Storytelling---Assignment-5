# ---- Libraries ----
library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)

# ---- Load Data ----
df <- read_xlsx("London Data.xlsx")

# ---- UI ----
ui <- fluidPage(
  titlePanel("Sentiment Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "sentiment",
        label = "Choose sentiment column:",
        choices = c(
          "textblob_polarity",
          "vader_sentiment_compound",
          "vader_sentiment_positive",
          "vader_sentiment_neutral",
          "vader_sentiment_negative",
          "RuSentiment_positive",
          "RuSentiment_neutral",
          "RuSentiment_negative"
        )
      )
    ),
    
    mainPanel(
      plotOutput("sentPlot"),
      tableOutput("statsTable")
    )
  )
)

# ---- SERVER ----
server <- function(input, output) {
  
  # Reactive data based on selected sentiment
  selected_data <- reactive({
    col <- input$sentiment
    
    # Extract numeric sentiment column safely
    vals <- suppressWarnings(as.numeric(df[[col]]))
    
    # Remove NAs so stats & plots don’t break
    vals <- vals[!is.na(vals)]
    
    return(vals)
  })
  
  # ---- Plot ----
  output$sentPlot <- renderPlot({
    vals <- selected_data()
    
    validate(
      need(length(vals) > 0, "No numeric values.")
    )
    
    ggplot(data.frame(values = vals), aes(x = values)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(
        title = paste("Distribution of", input$sentiment),
        x = "Sentiment Value",
        y = "Count"
      )
  })
  
  # ---- Summary Table ----
  output$statsTable <- renderTable({
    vals <- selected_data()
    
    data.frame(
      Min = min(vals),
      Mean = mean(vals),
      Median = median(vals),
      Max = max(vals)
    )
  })
}

# ---- Run App ----
shinyApp(ui = ui, server = server)
