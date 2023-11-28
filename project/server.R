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

  output$about_text1 <- renderUI({
    paste0("In this section, we are going to examine how much (or what percentage of) a NFL quarterback's salary is represented by the NFL's salary cap maximum. We are going to see how much performance can affect this number. Can we then use a quarterback's statistics to try to predict what percentage of the NFL cap hit he really should be making in a given year? We are going to use data from the NFL seasons of 2007 to 2022 (excluding 2010 since it was a lockout year and salaries and player statistics are not representative of a normal year) to answer this question.")
  })
  
  output$about_text2 <- renderUI({
    # Get links
    link1 <- a("the source for player statistics each season", href = "https://www.fantasypros.com/nfl/stats/qb.php?year=2007")
    link2 <- a("the source for player's salaries each season", href = "https://overthecap.com/position/quarterback/2007#google_vignette")
    link3 <- a("the source for NFL salary cap maximum's for each season", href = "https://www.spotrac.com/nfl/cba/")
    
    # Make text for this section
    paste0("To gather this data, we had to use three sources to get the information we wanted on a player's statistics, a player's salary, and the NFL's salary cap maximum in a given season so we know what percent the player was making from the team's budget. Please feel free to observe the corresponding links that will take you to ", link1, ", ", link2, ", and ", link3, ".")
  })

})
