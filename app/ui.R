library(shiny)
library(plotly)
library(dplyr)
library(QuantTools)
library(shiny)
library(bit64)
library(pracma)
library(quantmod)




shinyUI(fluidPage(
  titlePanel("Graficador con MAs crossover strategy"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
		    helpText("Ingrese un ticker válido y a continuación elija una combinación de medias móviles para visualizar la estrategia de cruces."),
        textInput("symb", label = h3("Ingrese un ticker"), value = "GGAL"),
		    hr(),
        selectInput( inputId ="mediamovil1",label = "Seleccione MA1", choices = c("ema","sma","wma" ) , selected ="sma"  ),
        
		    sliderInput(inputId = "periods1",
		                label = "Window MA1",
		                min =  1,
		                max = 250,
		                value = 20, step = 1),
		    
        selectInput(inputId ="mediamovil2",label = "Seleccione MA2", choices = c("ema","sma","wma" ), selected ="ema" ),
        
		    sliderInput(inputId = "periods2",
		                label = "Window MA2",
		                min =  1,
		                max = 250,
		                value = 40, step = 1),      
		    
		    hr(),
		    numericInput("fee", label = "Fee (%) ", value = 0.5, step= 0.05),
		    numericInput("size", label = "Tamaño de la posición", value = 10000),
		    actionButton("addstrat", "Ver esta estrategia"),
		    actionButton("borrstrat", "Borrar estrategia", step=100),
		    hr(),
		    
		    selectInput("scale", "Seleccione la escala", choices = c("linear","log" ))
		    
		    
        ) ,

      mainPanel(plotlyOutput("plot",width = 1200, height = 900))

  )
  
))
