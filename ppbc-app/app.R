library(shiny)
library(tidyverse)

#setwd('./ppbc-app/')

source("heatmap.R")
crime_time = readRDS("data/day_week.rds")

crimes = unique(crime_time$OffenseCategory)

ui <- fluidPage(
  titlePanel("Cases Reported in Portland, OR during January & February 2023"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var", strong("Offense Category: "),
                  choices = crimes, selected = "Larceny Offenses")
    ),
    mainPanel(plotOutput("map"))
  )
)

###

server <- function(input, output) 
{
  output$map <- renderPlot({
    
    data <- switch(input$var,
                   "Larceny Offenses" = filter(crime_time, OffenseCategory == crimes[[1]]),
                   "Vandalism" = filter(crime_time, OffenseCategory == crimes[[2]]),
                   "Motor Vehicle Theft" = filter(crime_time, OffenseCategory == crimes[[3]]),
                   "Assault Offenses" = filter(crime_time, OffenseCategory == crimes[[4]]), 
                   "Burglary" = filter(crime_time, OffenseCategory == crimes[[5]]), 
                   "Fraud Offenses" = filter(crime_time, OffenseCategory == crimes[[6]]), 
                   "Robbery" = filter(crime_time, OffenseCategory == crimes[[7]]),
                   "Sex Offenses" = filter(crime_time, OffenseCategory == crimes[[8]]), 
                   "Arson" = filter(crime_time, OffenseCategory == crimes[[9]]),
                   "Drug/Narcotic Offenses" = filter(crime_time, OffenseCategory == crimes[[10]]), 
                   "Weapon Law Violations" = filter(crime_time, OffenseCategory == crimes[[11]]),
                   "Kidnapping/Abduction" = filter(crime_time, OffenseCategory == crimes[[12]]),
                   "Counterfeiting/Forgery" = filter(crime_time, OffenseCategory == crimes[[13]]),
                   "Homicide Offenses" = filter(crime_time, OffenseCategory == crimes[[14]]),  
                   "Pornography/Obscene Material" = filter(crime_time, OffenseCategory == crimes[[15]]),
                   "Stolen Property Offenses" = filter(crime_time, OffenseCategory == crimes[[16]]),
                   "Human Trafficking Offenses" = filter(crime_time, OffenseCategory == crimes[[17]]), 
                   "Prostitution Offenses" = filter(crime_time, OffenseCategory == crimes[[18]]),  
                   "Embezzlement" = filter(crime_time, OffenseCategory == crimes[[19]]),
                   "Extortion/Blackmail" = filter(crime_time, OffenseCategory == crimes[[20]]), 
                   
                   )
    heatmap(data)
  })
  
}

shinyApp(ui = ui, server = server)