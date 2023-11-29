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
  ),
  shinytitle::use_shiny_title(),
  # Make into different tabs
  tabsetPanel(
    type = "tabs",
    # About tab
    tabPanel(
      "About", 
      uiOutput("about_text1"),
      br(),
      uiOutput("about_text2"),
      br(),
      uiOutput("about_text3"),
      br(),
      # This formats the images how we want
      fluidRow(
        column(3, NULL),
        column(3,
               imageOutput("about_image1")
        ),
        column(3,
               imageOutput("about_image2")
        ),
        column(3, NULL
        )
      )
    ),
    # Data exploration tab
    tabPanel(
      "Data Exploration",
      tabsetPanel(
        type = "tabs",
        # graphing tab
        tabPanel(
          "Different EDA Graphs",
          sidebarPanel(
            h3(
              "Select the Exploratory Data Analysis Features You Would Like to See"
            ),
            selectInput(
              "var", "Graphical Display to View",
              choices = c(
                "Scatter Plot" = "scatter",
                "Bar Plot" = "bar",
                "Histogram" = "hist",
                "Box Plot" = "box"
              ),
              selected = "scatter"
            )
          )
        ),
        # Summary tab
        tabPanel(
          "Different EDA Summaries"
        )
      )
    ),
    # Modeling tab
    tabPanel(
      "Modeling",
      # Make tabs within the Modeling tab
      tabsetPanel(
        type = "tabs",
        # Modeling Info tab
        tabPanel(
          "Modeling Info"
        ),
        # Modeling Fitting tab
        tabPanel(
          "Modeling Fitting"
        ),
        # Prediction tab
        tabPanel(
          "Prediction"
        )
      )
    )
  )
)
