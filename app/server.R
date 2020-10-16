library(shiny)
library(plotly)
library(dplyr)
library(QuantTools)
library(shiny)
library(bit64)
library(pracma)
library(quantmod)
library(data.table)




shinyServer(function(input, output) {

  
  
  
  pricesreactive <- reactive({
    
    prices <- getSymbols(input$symb, auto.assign = FALSE)
    prices<- as.data.frame(prices)
    prices$time <- rownames(prices)
    colnames(prices) <- c("open","high","low","close","volume","adjusted","time")
    prices<- as.data.frame(prices)
    
    
  })
  
  
  
  dataInput <- reactive({

    prices <- pricesreactive()


      if ( input$mediamovil1 == "sma" ){
        prices$ma1 <- sma(prices$close, input$periods1 )
      } else if (input$mediamovil1 == "ema") { prices$ma1 <- ema(prices$close, as.numeric(input$periods1))
      } else {
        prices$ma1 <- movavg(prices$close, input$periods1, type=c("w"))
      }

      if ( input$mediamovil2 == "sma" ){
        prices$ma2 <- sma(prices$close, input$periods2)
      } else if (input$mediamovil2 == "ema") { prices$ma2 <- ema(prices$close, as.numeric( input$periods2))
      } else {
        prices$ma2 <- movavg(prices$close, input$periods2, type=c("w"))
      }

prices <- prices[complete.cases(prices), ]

  })
  
  
  
  
  
  
  
  resumen <- eventReactive(input$symb, {
    
    resumen <- NULL
    
  })
  
  
  
  

  resumen <- reactiveVal( setNames(data.frame(matrix(ncol = 9, nrow = 1)), c( "open","high","close","low","position","acumprofit","ma1","ma2","time"  )) )
  
  
  
  observeEvent(input$symb, {
    
    resumen <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), c( "open","high","close","low","position","acumprofit","ma1","ma2","time"  )) 
    
    
    resumen(resumen)
    
  })
  
  
  
  
  
  
  
  
  
  
  observeEvent(input$borrstrat, {
    
    resumen <- setNames(data.frame(matrix(ncol = 9, nrow = 1)), c( "open","high","close","low","position","acumprofit","ma1","ma2","time"  )) 
    
    
    resumen(resumen)
    
  })
  
  
  
  
  
  
  observeEvent(input$addstrat, {
    
    fee= input$fee 
    feev = (100-fee) / 100

    tsize <- input$size
    
    prices <- dataInput()
    
    DT <- data.table(prices )
    
    DT<-DT[ (( ma1 >= ma2) & ( shift(ma1,1) < shift(ma2,1 ))) , position := "buy"  ]
    DT<-DT[ (( ma1 <= ma2) & ( shift(ma1,1) > shift(ma2,1 ))) , position := "sell" ]
    
    
    
    DT<-na.omit(DT)
    
    DT<-DT[ , profit := ifelse( shift(position,1) == "buy", ((tsize*feev/shift(close,1))*close)-tsize  , 0  )]
    # DT<-DT[ , profit := ifelse( shift(position,1) == "buy", ((tsize*feev/shift(close,1))*close)-tsize  ,  tsize*feev/shift(close,1)*close*feev  )]
    
    
    DT<-DT[2:nrow(DT)  , acumprofit := cumsum(profit) ][2:nrow(DT)]
    
    resumen <- as.data.frame(DT)
    
    resumen(resumen)
    
  })
  
  
  
  
  
  

  
    output$plot <- renderPlotly({

      prices <- dataInput()

      resumen <- resumen()
      
      resumenc <- resumen[ resumen$position == "buy" , ] 
      resumenv <- resumen[ resumen$position == "sell" , ] 
      
      
      
      y1 = list(side = "left", title="Precio", type =input$scale )
      
      y2 = list(overlaying = "y", side = "right",zeroline = FALSE,showgrid = FALSE, title="Ganancia acumulada $")

      

      plot_ly(prices, x= ~time ,type="candlestick",
              open=~open,close=~close,high=~high,low=~low, name = isolate(  input$symb) )%>%
        layout(title="Visor", yaxis = y1,  yaxis2 = y2  )%>%
        add_trace(cliponaxis = TRUE, x = prices$time, y = prices$ma1, type = "scatter", mode = "lines",line = list(color = 'rgb(0, 51, 204)', width = 2), color = 'rgb(0, 0, 153)', name = isolate( paste(input$mediamovil1, input$periods1 ) ))%>%
        add_trace(cliponaxis = TRUE, x = prices$time, y = prices$ma2, type = "scatter", mode = "lines",line = list(color = 'rgb(204, 51, 0)', width = 2), color = 'rgb(0, 0, 153)', name = isolate( paste(input$mediamovil2, input$periods2 ) ))%>%
        add_trace(cliponaxis = TRUE, x = resumenv$time, y = resumenv$close, yaxis="y1", type = "scatter", mode = "markers", marker = list(size = 10,color = 'rgb(255, 100, 100)',
                                                                                                                                                               line = list(color = 'rgb(0, 0, 0)',
                                                                                                                                                                           width = 3)),name = "sell" )%>%
        add_trace(cliponaxis = TRUE, x = resumenc$time, y = resumenc$close,  yaxis="y1", type = "scatter", mode = "markers", marker = list(size = 10,color = 'rgb(50, 255, 100)',
                                                                                                                                                          line = list(color = 'rgb(0, 0, 0)',
                                                                                                                                                                      width = 3)),name = "buy"  )%>%
        add_trace(cliponaxis = TRUE, x = resumen$time, y = (resumen$acumprofit) , yaxis="y2", type = "scatter", mode = "lines",line = list(color = 'rgb(80, 51, 100)', width = 2), color = 'rgb(0, 0, 153)',name = "Ganancia acumulada $" )
      
      })
})








