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
    link4 <- a("the R file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/File%20to%20Combine%20Data%20into%20One%20Source.R")
    link5 <- a("CSV file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/player_stats_and_salary.csv")
    link6 <- a("RDS file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/player_stats_and_salary.rds")
    
    # Make text for this section
    tagList("To gather this data, we had to use three sources to get the information we wanted on a player's statistics, a player's salary, and the NFL's salary cap maximum in a given season so we know what percent the player was making from the team's budget. Please feel free to observe the corresponding links that will take you to ", link1, ", ", link2, ", and ", link3, ". We then cleaned our data and filtered it in a way that can be used analysis. If you are interested in how this was done (with the corresponding R code), please feel free to check out ", link4, ". If you would like to see the analyzed ", link5, " or the ", link6, " , please feel free to view them as well.")
  })
  
  output$about_text3 <- renderUI({
    HTML(paste0("As you are going through this application, you might notice that there are 3 tabs. The first tab, the one you are currently on (which is the ", strong("About"), " tab). This tab will give you information about what that project is, the data sources, functions of each tab, and an image that will help you better understand what this project is. The next tab is the ", strong("Data Exploration"), " tab. This section will allow the user to create different numerical and graphical summaries from our data. This section is an interactive exploratory data analysis in which the user can gain their own conclusions from the data, based on what they want to look at. Lastly, there is a ", strong("Modeling"), " tab. This section allows the user to fit two types of supervised learning models. In our case, we will be looking at using a multiple linear regression model and a random forest model. The user will be able to select which variables we would like to use as predictors which in return will help fit an optimal optimial model that will be used in making predictions on what percentage of a NFL quarterback's salary should be allocated to them from the team's salary cap budget. I hope as the user that you enjoy the application and can live your dream by being a NFL General Manager who can make decisions on what to do with your favorite's quarterback situation."))
  })
  
  output$about_image1 <- renderImage({
    list(src = "Shiny App picture.png",
         width = "100%",
         height = "100%")
  }, deleteFile = F)
  
  output$about_image2 <- renderImage({
    list(src = "NFL logo.png",
         width = "100%",
         height = "100%")
  }, deleteFile = F)

})
