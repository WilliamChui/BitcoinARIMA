require(shiny)
require(crypto2)

# Data pre-processing ----
btc <- crypto_history(crypto_list()[1, ])
timestamp <- as.Date(btc$timestamp, "%Y%m%d")


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bitcoin"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("method", "Method", 
                  c("Closed Price" = "price",
                    "Growth Rate" = "return")),
      
      # Input: Selector for variable to plot against mpg ----
      sliderInput("p", "Order p", min = 0, max = 10, value = 0, step = 1, ticks = FALSE),
      sliderInput("d", "Order d", min = 0, max = 5, value = 0, step = 1, ticks = FALSE),
      sliderInput("q", "Order q", min = 0, max = 10, value = 0, step = 1, ticks = FALSE),
      sliderInput("P", "Seasonal P", min = 0, max = 10, value = 0, step = 1, ticks = FALSE),
      sliderInput("D", "Seasonal D", min = 0, max = 10, value = 0, step = 1, ticks = FALSE),
      sliderInput("Q", "Seasonal Q", min = 0, max = 10, value = 0, step = 1, ticks = FALSE),
      sliderInput("period", "Seasonal Period", min = 0, max = 10, value = NA, step = 1, ticks = FALSE),
      sliderInput("date", "Date", min = min(timestamp), 
                  max = max(timestamp), step = 1, value = c(min(timestamp), max(timestamp))),
      sliderInput("nStep", "n-step forecast", min = 1, max = 365, value = 1, step = 1, ticks = FALSE),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot ----
      plotOutput("plot")
      
    )
  )
)



# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption functions
  formulaText <- reactive({
    paste0("ARIMA(", input$p, ", ", input$d, ", ", input$q, ")")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot ----
  output$plot <- renderPlot({
    startIdx <- which(timestamp == input$date[1])
    endIdx <- which(timestamp == input$date[2])

    closeprice <- NULL
    if (input$method == "price") {
      closeprice <- btc$close[startIdx:endIdx]
    } else {
      closeprice <- log(btc$close[startIdx:endIdx])
    }
    
    model <- arima(closeprice, order = c(input$p, input$d, input$q), 
                   seasonal = list(order = c(input$P, input$D, input$Q), period = input$period))
    aic <- model$aic
    fc <- forecast(model, h = input$nStep)
    
    future <- timestamp[endIdx] + 1:input$nStep
    x <- c(timestamp[startIdx:endIdx], future)
    y <- c(closeprice, fc$mean)
    
    plot(x = timestamp[startIdx:endIdx], y = closeprice, type = "l",
         xlim = c(min(x), max(x)), 
         ylim = c(min(y, min(fc$lower)), max(y, max(fc$upper))), 
         axes = FALSE, xlab = "", ylab = "")
    par(new = TRUE)
    plot(x = future, y = fc$mean, type = "l",
         col = "red",
         xlim = c(min(x), max(x)), 
         ylim = c(min(y, min(fc$lower)), max(y, max(fc$upper))), 
         axes = TRUE, lwd = 1, xlab = "Date", 
         ylab = ifelse(input$method == "price", "Closed price ($)", "Growth rate"))
    polygon(c(future, rev(future)), c(fc$lower[, 1], rev(fc$upper[, 1])), border = 8, col = gray(0.6, alpha = 0.2))
    polygon(c(future, rev(future)), c(fc$lower[, 2], rev(fc$upper[, 2])), border = 8, col = gray(0.4, alpha = 0.1))
  }, height = 600)
}

shinyApp(ui, server)
