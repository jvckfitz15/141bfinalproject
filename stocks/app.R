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
library(bslib)
# url: 'https://api.tiingo.com'
# api token: 064b143dbfe6e294acfda1803caae634f6981273
riingo_set_token('064b143dbfe6e294acfda1803caae634f6981273')
start_day <- "2020-01-01"
ticker <- (c("GME", "AMC", "NOK", "SNDL", "SPCE", "KOSS", 'JAGX', "ZOM", "EXPR", "NIO", "TSLA", "QS",
             "OCGN", "SENS", "QS", "ANCN", "HEAR", "ONLN", "SSY"))
market_stats <- (c("close", "high", "low", "open", "volume"))

ui <- fluidPage(
    
    theme = bs_theme(version = 4, bootswatch = "darkly"),
    # Application title
    titlePanel("Time Series Analysis of Personal Portfolio"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('stock', "Select Stock Symbol",
                        choices = ticker,
                        selected = ticker[1]),
            selectInput('stat', 'Select Desired Market Data',
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
            textOutput("summaryText"),
            tableOutput("summaryTable")
            
        ),
        
        # Show plots, text and tables in main panel
        # use tabPanel to create a readme tab for users
        mainPanel(
            tabsetPanel(
                tabPanel(title = "Main", textOutput("text1"),
                         plotOutput("distPlot"),
                         textOutput("text2"),
                         tableOutput("table1")),
                tabPanel(title = "READ ME", textOutput('readmeText'))
            )
            
        )
    )
)

# Define server logic required to create necessary plots, tables and text
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
        
        paste("You have selected the time series of", input$stock, "for daily market", input$stat)
        
    })
    output$text2 <- renderText({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        paste("Table of", symbol, "values for given market statistics")
    })
    output$summaryText <- renderText({
        symbol = input$stock
        mkt_stat = input$stat
        start_date = input$start
        end_date = input$end
        
        paste("Summary statistics of", mkt_stat, "for", symbol)
    })
    output$readmeText <- renderText({
        print("This is an app designed to track certain market statistics for my specific portfolio. This app is mainly useful to myself only but if the API allowed so, it would be possible to use this app for all stock symbols in the market.
              The market statistics used in this application are market close, high, low, open, and volume. Once you have chosen a stock symbol, and then selected one of the statistics you will then have output in the main panel.
              This output will be a time series of the stock for that statistic starting from 01/01/2020 and ending on the current days date. From here you may change the dates to select a certain period of the data and the graph will change.
              Additionally, once you have selected a stock symbol, a complete data table with all of the possible market statistics for that date range and symbol will be displayed.
              The final piece of output will display under the choices in the side panel, and this will display summary statistics for that stock, and market statistic that has been chosen, over the chosen time period. In order to change the list to
              change as my portfolio changes sizes, I would just need to add or take out stock symbols in the source code. For my personal portfolio this is helpful. If able to track all symbols, which isn't allowed by the API this could be applicable to any user to monitor certain stocks. 
              Happy trading.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
