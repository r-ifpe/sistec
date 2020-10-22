library(shiny)
library(dplyr)
options(shiny.maxRequestSize = 100*1024^2) 

sistec::aria(version = "online", test_mode = FALSE)
