library(tidyverse)
library(tidyquant)
library(quantmod)
library(lubridate)
library(riingo)
library(httr)
library(jsonlite)
library(usethis)
library(ggplot2)
library(shiny)
# url: 'https://api.tiingo.com'
# api token: 064b143dbfe6e294acfda1803caae634f6981273
riingo_set_token('064b143dbfe6e294acfda1803caae634f6981273')
start_day <- "2020-01-01"
ticker <- (c("GME", "AMC", "NOK", "SNDL"))
market_stats <- (c("close", "high", "low", "open", "volume"))

ui <- fluidPage(
    
    # Application title
    titlePanel("Stonks"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('stock', "Select Stock name",
                        choices = ticker,
                        selected = ticker[1]),
            selectInput('stat', 'Select Market Data',
                        choices = market_stats,
                        selected = market_stats[1]),
            dateInput('start', "Choose Start Date",
                      value = "2020-01-01",
                      min = "2020-01-01", 
                      max = today()),
            dateInput("end", "Choose End Date", 
                      value = today(),
                      min = "2020-01-02",
                      max = today()),
            tableOutput("summaryTable")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(textOutput("text1"),
                  plotOutput("distPlot"),
                  textOutput("text2"),
                  tableOutput("table1")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        chosen_stock_data <- riingo_prices(symbol, start_date = start_date, end_date = today(), resample_frequency = "daily") %>% 
            select(ticker, date, close, high, low, open, volume)
        
        chosen_stock_data$date <- as.Date(chosen_stock_data$date)
        chosen_period <- chosen_stock_data[chosen_stock_data$date<end_date,]
        
        chosen_stat <- chosen_period %>% select(date, contains(mkt_stat))
        dates <- chosen_stat$date %>% unlist()
        mark_stat <- chosen_stat[,2] %>% unlist()
        
        plot_ts1 <- chosen_stat %>% 
            ggplot(aes(x = dates, y = mark_stat))+
            geom_line()
        plot_ts1
        
        
    })
    
    output$table1 <- renderTable({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        chosen_stock_data <- riingo_prices(symbol, start_date = start_date, end_date = today(), resample_frequency = "daily") %>% 
            select(ticker, date, close, high, low, open, volume)
        chosen_period <- chosen_stock_data[chosen_stock_data$date<end_date,]
        
        
        
        
    })
    
    output$summaryTable <- renderTable({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        chosen_stock_data <- riingo_prices(symbol, start_date = start_date, end_date = today(), resample_frequency = "daily") %>% 
            select(ticker, date, close, high, low, open, volume)
        
        chosen_stock_data$date <- as.Date(chosen_stock_data$date)
        chosen_period <- chosen_stock_data[chosen_stock_data$date<end_date,]
        
        chosen_stat <- chosen_period %>% select(date, contains(mkt_stat))
        dates <- chosen_stat$date %>% unlist()
        mark_stat <- chosen_stat[,2]
        summary(mark_stat)
        
        
    })
    output$text1 <- renderText({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        paste("You have selected the time series plot of", input$stock, "for daily market", input$stat)
        
    })
    output$text2 <- renderText({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        paste("Table of", symbol, "values for given market statistics")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
