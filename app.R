# Load packages ----
library(shiny)
library(quantmod)
library(forecast)

# Source helpers ----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel(title=div("EpsilonCoding Stock Forecaster",img(src="epsilon.png", height = 82, width = 82)), windowTitle = "EpsilonCoding Stock Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      em(helpText("Created & Maintained by Ryan Dundun and Richard Feng")),
      br(),
      p(strong("Select a Stock ticker to Web-Scrape, Decompose, and Forecast: ")),
      #p("Examples: 'AAPL'(Apple), 'B'(Barnes Group), 'GE' (General Electric), 'JPM'(JP Morgan)")),
      
      textInput("symb", "Ticker Symbol:", "SPY"),
      
      selectInput("Public Indicator",
                  label= "Public Indicator (used for 'VARMAX Forecast' only)",
                  choices = list("GDP","Unemployment Rate", "S&P 500", "Dow Jones", "Oil Price", 
                                 "Consumer Price Index","US Disposable Income")),
      
      dateRangeInput("dates", 
                     "Date range:",
                     start = "2010-01-01", 
                     end = as.character(Sys.Date())),
      
      br(),
      p(em("In partnership with,")),
      img(src="icarted.png", height = 75, width = 75),
      p(em("iCarted")),
      br(),
      helpText("DISCLAIMER: Past stock performance is not indicative of future price action. 
               This site if for educational purposes only. Ryan Dundun, Richard Feng, iCarted, and 
               EpsilonCoding do not accept any liability for your use of this information.")
      
      #checkboxInput("log", "Plot y axis on log scale", 
                    #value = FALSE),
      
      #checkboxInput("adjust", 
                    #"Adjust prices for inflation", value = FALSE)
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Historical Stock Prices",
          h3(textOutput("text1")),
          em(helpText("Stock Prices below:")),
          plotOutput("plot"),
          p(strong("Additional Information:")," The chart above gives the stock prices for your chosen stock ticker. Note, you can change the time frame
            from the 'Date Range' pannel on the left.")),
        tabPanel("Decomposition", 
          h3(textOutput("text2")),
          em(helpText("Time Series Decomposed into its three components: 'seasonal', 'trend', and 'remainder'")),
          plotOutput("theDecomp"),
          p(strong("Additional Information:")," This is the decomposition, the first step in making an ARIMA forecast. Here you can see the 
            components of the time series seperately. These are used as the building blocks to create the ARIMA forecast. Click on the 'ARIMA Forecast'
            tab to see the forecast.")),
        tabPanel("ARIMA Forecast", 
          h3(textOutput("text3")),
          plotOutput("theArima"),
          p(strong("Additional Information:")," Stock prices are inherently difficult to predict. If the ARIMA forecast returns 
            a straight horizontal line, this indicates the time series has entropy values that are too high. In other words, the time series 
            can't be successfully predicted by ARIMA.")),
        tabPanel("VARMAX Forecast",
                 br(),
                 img(src="construction.png", height = 250, width=500)),
        tabPanel("Accuracy of Forecast (MAPE value)",
                 br(),
                 img(src="construction.png", height = 250, width=500)),
        tabPanel("FAQ", 
                 h4("Should I place trades/investments based off these forecasts?"),
                    p("NO. This site has been designed for informational and educational purposes 
                      only. All investors are advised to conduct their own independent research into 
                      individual stocks before making a purchase decision. In addition, it is important to 
                      note that past stock performance is not indicative of future price action. Ryan Dundun, Richard Feng, and EpsilonCoding do not guarantee the validity or accuracy of any 
                      model and do not accept any liability for your use of this information."),
                 br(),
                 h4("What is an ARIMA forecast?"),
                 p("An ARIMA forecast is an univariate, parametric, statistical model that builds its
                   forecast from its own inertia. ARIMA models decompose a time series into seasonal,
                   cycle, and trend components. These components are then used as building blocks to 
                   create the forecast."),
                 br(),
                 h4("What is a VARMAX forecast?"),
                 p("In a way, a VARMAX forecast is an extended version of an ARIMA forecast. VARMAX is 
                   a multivariate, parametric, statistical model that incorporates exogenous input in 
                   an attempt to further improve the models accuracy beyond the ARIMA."),
                 br(),
                 h4("What is the 'Accuracy of Forecast (MAPE)' tab all about?"),
                 p("MAPE stands for Mean Absolute Percentage Error. MAPE is used to determine the accuracy 
                   of a forecast. Our MAPE values are calculated by deleting a year's worth of data, performing 
                   a one year forecast, then finally comparing the forecast against reality. Within that year of 
                   comparison, a MAPE value can be calculated from the MAPE equation below:"),
                 img(src="mape.png", height = 100, width= 325))
      ))
  ))

# Server logic

#ARIMA and Decomp


#ServerFunction
server <- function(input, output) {
  
  yourChosenStock <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  dataInput2 <- reactive({
    getSymbols(input$symb, src = "yahoo", 
               #from = "2010-01-01",
               from = input$dates[1],
               to=as.character(Sys.Date()),
               auto.assign = FALSE)
    
  })
  
 
  output$plot <- renderPlot({
    
    chartSeries(yourChosenStock(), theme = chartTheme("white"), 
                type = "line", TA = NULL)
  })
  
  output$theDecomp <- renderPlot({
    theDataOpen <- dataInput2()
    theDataSeries <-
      ts(
        theDataOpen[,1],
        start=c(2010,1),
        end=c(2018,9),
        frequency=12
      )
    decomp <- stl(theDataSeries[,1], s.window="periodic")
    plot(decomp)
  })
  
  output$theArima <- renderPlot({
    theData <- dataInput2()
    theDataOpen=theData[,1]
    nextData = theDataOpen[,1]
    data.monthly <- nextData[ endpoints(nextData, on="months", k=1), ]
    theDataFit <- auto.arima(data.monthly, seasonal=TRUE)
    theDataForecast <- forecast(theDataFit)
    plot(theDataForecast, xlab="Year", ylab="Stock Price")
    
  })
    
  
  output$text1 <- renderText({paste("Your selected ticker symbol: ", input$symb)})
  
  output$text2 <- renderText({paste("Decomposition for Symbol: ", input$symb)})
  
  output$text3 <- renderText({paste("ARIMA Forecast for Symbol: ", input$symb)})
}

# Run the app
shinyApp(ui, server)
