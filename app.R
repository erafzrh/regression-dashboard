# Load Package
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(gridExtra)
library(shinythemes)
library(dplyr)
library(broom)
library(car)
library(corrplot)
library(lmtest)
library(nortest)

# Load data
data <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "REGRESSION DASHBOARD"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Regression Analysis", tabName = "regression"),
      menuItem("Prediction", tabName = "prediction")
    )
  ),
  dashboardBody(
    tabItems(
      # Tab regression
      tabItem(
        tabName = "regression",
        fluidPage(
          titlePanel(title = div("Multiple Linear Regression Analysis", 
                                 style = "font-size: 30px; font-weight: bold; text-align: center; height: 50px")),
          theme = "cerulean",
          sidebarLayout(
            sidebarPanel(
              selectInput("dependen_vars", "Dependent Variables", choices = "y"),
              selectInput("independent_vars", "Independent Variables", choices = "x1"),
              selectInput("independent_vars", "Independent Variables", choices = "x2"),
              selectInput("independent_vars", "Independent Variables", choices = "x3"),
              selectInput("independent_vars", "Independent Variables", choices = "x4"),
              selectInput("independent_vars", "Independent Variables", choices = "x5")),
            mainPanel(
              tabsetPanel(
                tabPanel("Descriptive Statistics", verbatimTextOutput("desc_stats"),
                         plotlyOutput("boxplot_desc_stats")), # Perubahan dari plotOutput menjadi plotlyOutput
                tabPanel("Full Model Summary", verbatimTextOutput("model_summary")),
                tabPanel("Autocorrelation", verbatimTextOutput("autocorrelation")),
                tabPanel("Homoskedasticity", verbatimTextOutput("homoskedasticity")),
                tabPanel("Normality of Residuals", plotOutput("normality_residuals")),
                tabPanel("Multikolinieritas", verbatimTextOutput("multikolinieritas")),
                tabPanel("Questions", verbatimTextOutput("questions"))
              )
            )
          )
        )
      ),
      # Tab prediction
      tabItem(
        tabName = "prediction",
        fluidPage(
          titlePanel(title = div("Monthly Sales Volume Prediction", 
                                 style = "font-size: 30px; font-weight: bold; text-align: center; height: 50px")),
          sidebarLayout(
            sidebarPanel(
              numericInput("x1_input", "Number of Website Visitors per Month:", value = 270000),
              numericInput("x2_input", "Number of Monthly Transactions:", value = 15500),
              numericInput("x3_input", "Average Number of Items per Transaction:", value = 5.8),
              numericInput("x4_input", "Customer Satisfaction Rating (scale 1-10):", value = 8.8),
              numericInput("x5_input", "Number of Online Advertisements Run per Month:", value = 65000),
              actionButton("volume_button", "Sales Volume", class = "btn-primary")
            ),
            mainPanel(
              plotOutput("sales_plot"),
              verbatimTextOutput("analysis_output")
            )
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output) {
  
  # Multiple Linear Regression Analysis
  output$desc_stats <- renderPrint({
    summary(data)
  })
  
  output$model_summary <- renderPrint({
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    summary(model)
  })
  
  output$boxplot_desc_stats <- renderPlotly({ # Perubahan dari renderPlot menjadi renderPlotly
    p <- plot_ly()
    for (col in names(data)[-1]) {
      p <- add_trace(p, y = data[[col]], name = col, type = 'box')
    }
    
    p <- layout(p, title = "Boxplot of All Variables")
    p <- p %>% layout(hovermode = "closest") %>% layout(showlegend = TRUE)
    p
  })
  
  ##Checking Assumptions ##
  #Uji Autokorelasi  
  output$autocorrelation <- renderPrint({
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    dwtest(model)
  })
  
  #Uji Homoskedastisitas
  output$homoskedasticity <- renderPrint({
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    bptest(model, studentize = TRUE, data = data)
  })
  
  #Uji Normalitas
  output$normality_residuals <- renderPlot({
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    qqnorm(model$residuals, pch = 1, frame = FALSE)
    qqline(model$residuals, col = "steelblue", lwd = 2)
  })
  
  # Uji Multikolinieritas
  output$multikolinieritas <- renderPrint({
    model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
    vif(model)
  })
  
  # Questions 
  output$questions <- renderPrint({
    model1 <- lm(y ~ x1 + x5, data = data)
    result_summary <- summary(model1)
    
    # Menambahkan kalimat sebelum hasil summary
    cat("PERTANYAAN:\n")
    cat("Bagaimana cara memeriksa apakah ada interaksi yang signifikan antara jumlah\n")
    cat("pengunjung situs web(x1) dan jumlah iklan(x5) yang mempengaruhi penjualan?\n")
    
    # Menampilkan hasil summary
    print(result_summary)
  })
  
  # Prediction
  # Regression model
  modl <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = data)
  
  observeEvent(input$volume_button, {
    req(input$x1_input, input$x2_input, input$x3_input, input$x4_input, input$x5_input)
    
    new_data <- data.frame(
      x1 = input$x1_input,
      x2 = input$x2_input,
      x3 = input$x3_input,
      x4 = input$x4_input,
      x5 = input$x5_input
    )
    
    # Predicted monthly sales volume
    predicted_sales <- predict(modl, newdata = new_data)
    predicted_sales <- abs(predicted_sales)
    predicted_sales <- round(predicted_sales)
    
    output$sales_plot <- renderPlot({
      # Scatter plot for monthly sales volume prediction results
      plot(data$x1, data$y, col = "black", xlab = "Number of Website Visitors per Month", 
           ylab = "Monthly Sales Volume (in thousands of USD)", 
           main = "Predicted Monthly Sales Volume", xlim = c(150000, 350000), ylim = c(0, 500))
      points(new_data$x1, predicted_sales, col = "blue", pch = 20)
      legend("topright", legend = c("Actual Data", "Predicted Data"), col = c("black", "blue"), pch = c(1, 20))
    })
    
    output$analysis_output <- renderPrint({
      cat("Predicted Monthly Sales Volume: ", predicted_sales, " (in thousands of USD)\n")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)



