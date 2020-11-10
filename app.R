library(shiny)
library(dplyr)
library(stringr)
library(sistec)
library(utils)

options(shiny.maxRequestSize =  as.integer(100*1024^2)) 
ui <- aria_ui()
#server <- aria_server(version = "online")
server <- aria_server(version = "online")
shinyApp(ui, server)

