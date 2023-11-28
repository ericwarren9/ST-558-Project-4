#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Needed libraries --------------------------------------------------------

library(shiny)
library(tidyverse)
library(bslib)


# The application ---------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  titlePanel(
    div(span("Quarterbacks Cap Hit Percentage by Performance", style = "color:black", align = "center"),
        align = "center",
        br(),
    ), 
    windowTitle =  "Quarterbacks Cap Hit Percentage by Performance"
  )
)
