library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(ggvis)

tradeA <- read_csv('tradeA.csv')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Trade, Production, and Protection"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
       selectInput("country",
                   label = "Choose Country:",
                   choices = unique(tradeA$ccode),
                   selected = 'USA'),
       numericInput("maxxval",
                   label = "Max x value",
                   value = 100,
                   min = 50,
                   max = 5000,
                   step = 50),
       selectInput("imp_comp",
                   label = "Only Import Competers?",
                   choices = c('yes', 'no'),
                   selected = 'no'),
       selectInput("tar_type",
                   label = "Applied or MFN rates?",
                   choices = c('Applied', 'MFN'),
                   selected = 'Applied'),
       selectInput("ntb",
                   label = "Include NTBs?",
                   choices = c('yes', 'no'),
                   selected = 'yes'),
       width = 3
    ),
    
    mainPanel(
      h3(textOutput('plot1head'), align = 'center'),
      ggvisOutput('all'),
      h3(textOutput('plot2head'), align = 'center'),
      ggvisOutput('n')
    )
  )
))