#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load Server Libraries ---------------------------------------------------

library(shiny)
library(tidyverse)
library(bslib)


# Start the server section of app -----------------------------------------

shinyServer(function(input, output, session) {

  output$about <- renderUI({
    text <- paste0("In this section, we are going to examine how much (or what percentage of) a NFL quarterback's salary is represented by the NFL's salary cap maximum. We are going to see how much performance can affect this number. Can we then use a quarterback's statistics to try to predict what percentage of the NFL cap hit he really should be making in a given year?")
    h3(text)
  })

})
