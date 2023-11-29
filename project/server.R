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
library(shinyWidgets)


# Read data in ------------------------------------------------------------

final_player_info <- read_rds("player_stats_and_salary.rds") %>%
  mutate(
    cap_hit_group = ifelse(cap_percent > 10, "high",
                           ifelse(cap_percent >= 5, "medium", "low")),
    missed_games = ifelse((as.numeric(year) < 2021) & (games == 16), "no",
                          ifelse((as.numeric(year) > 2021) & (games == 17), "no", "yes")),
    cap_hit_group = factor(cap_hit_group, levels = c("low", "medium", "high")),
    cap_hit_group = relevel(cap_hit_group, "medium"),
    cap_hit_group = relevel(cap_hit_group, "low"),
    missed_games = factor(missed_games),
    year = factor(year)
  )


# Start the server section of app -----------------------------------------

shinyServer(function(input, output, session) {

  # Get text for first paragraph for about tab
  output$about_text1 <- renderUI({
    paste0("In this section, we are going to examine how much (or what percentage of) a NFL quarterback's salary is represented by the NFL's salary cap maximum. We are going to see how much performance can affect this number. Can we then use a quarterback's statistics to try to predict what percentage of the NFL cap hit he really should be making in a given year? We are going to use data from the NFL seasons of 2007 to 2022 (excluding 2010 since it was a lockout year and salaries and player statistics are not representative of a normal year) to answer this question.")
  })
  
  # Get text for second paragraph for about tab
  output$about_text2 <- renderUI({
    # Get links
    link1 <- a("the source for player statistics each season", href = "https://www.fantasypros.com/nfl/stats/qb.php?year=2007")
    link2 <- a("the source for player's salaries each season", href = "https://overthecap.com/position/quarterback/2007#google_vignette")
    link3 <- a("the source for NFL salary cap maximum's for each season", href = "https://www.spotrac.com/nfl/cba/")
    link4 <- a("the R file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/File%20to%20Combine%20Data%20into%20One%20Source.R")
    link5 <- a("CSV file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/player_stats_and_salary.csv")
    link6 <- a("RDS file", href = "https://github.com/ericwarren9/ST-558-Project-4/blob/main/player_stats_and_salary.rds")
    
    # Make text for this section
    tagList("To gather this data, we had to use three sources to get the information we wanted on a player's statistics, a player's salary, and the NFL's salary cap maximum in a given season so we know what percent the player was making from the team's budget. Please feel free to observe the corresponding links that will take you to ", link1, ", ", link2, ", and ", link3, ". We then cleaned our data and filtered it in a way that can be used analysis. Please note that the player's statistics are on a per game basis so the player will not be penalized more in the analysis for missing more games. This helps make predictoin more accurate and does not weigh the number of games variable even more. Instead this essentially standardizes our data. If you are interested in how this was done (with the corresponding R code), please feel free to check out ", link4, ". If you would like to see the analyzed ", link5, " or the ", link6, " , please feel free to view them as well.")
  })
  
  # Get text for third paragraph for about tab
  output$about_text3 <- renderUI({
    HTML(paste0("As you are going through this application, you might notice that there are 3 tabs. The first tab, the one you are currently on (which is the ", strong("About"), " tab). This tab will give you information about what that project is, the data sources, functions of each tab, and an image that will help you better understand what this project is. The next tab is the ", strong("Data Exploration"), " tab. This section will allow the user to create different numerical and graphical summaries from our data. This section is an interactive exploratory data analysis in which the user can gain their own conclusions from the data, based on what they want to look at. Lastly, there is a ", strong("Modeling"), " tab. This section allows the user to fit two types of supervised learning models. In our case, we will be looking at using a multiple linear regression model and a random forest model. The user will be able to select which variables we would like to use as predictors which in return will help fit an optimal optimial model that will be used in making predictions on what percentage of a NFL quarterback's salary should be allocated to them from the team's salary cap budget. I hope as the user that you enjoy the application and can live your dream by being a NFL General Manager who can make decisions on what to do with your favorite's quarterback situation."))
  })
  
  # Get first image for about section
  output$about_image1 <- renderImage({
    list(src = "Shiny App picture.png",
         width = "100%",
         height = "100%")
  }, deleteFile = F)
  
  # Get second image for about section
  output$about_image2 <- renderImage({
    list(src = "NFL logo.png",
         width = "100%",
         height = "100%")
  }, deleteFile = F)
  
  # Check which plot is selected
  plotCheckGraph <- reactive({
    req("input$var")
    input$var
  })
  
  # Make facet checkbox condition
  facetCheck <- reactive({
    req("input$facetPlot")
    if(input$facetPlot == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Make facet variable reactive
  facetVariable <- reactive({
    req("input$facetChoices")
    input$facetChoices
  })
  
  # Make boxplot checkbox condition
  boxplotGroupingCheck <- reactive({
    req("input$boxplotGrouping")
    if(input$boxplotGrouping == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Make boxplot grouping variable
  boxplotGroupingVariable <- reactive({
    req("input$boxplotChoice")
    input$boxplotChoice
  })
  
  # Make Boxplot and Histogram regular variable
  histogramBoxplotVariable <- reactive({
    req("input$histogramChoice")
    input$histogramChoice
  })
  
  # Make Barplot regular variable
  barplotVariable <- reactive({
    req("input$barplotChoice")
    input$barplotChoice
  })
  
  # Make barplot checkbox condition
  barplotGroupingCheck <- reactive({
    req("input$barplotGrouping")
    if(input$barplotGrouping == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Make barplot grouping variable
  barplotGroupingVariable <- reactive({
    req("input$barplotChoice2")
    input$barplotChoice2
  })
  
  # Make slider for the number of bins for histogram
  numberOfBins <- reactive({
    req("input$bins")
    input$bins
  })
  
  # Make the correct graph
  output$graph <- renderPlot({
    
    # Get variables
    plotChosen <- plotCheckGraph()
    variablePlot <- histogramBoxplotVariable()
    groupingBoxplotCheckPlot <- boxplotGroupingCheck()
    groupingBoxplotVariablePlot <- boxplotGroupingVariable()
    facetCheckPlot <- facetCheck()
    facetVariablePlot <- facetVariable()
    barplotVariablePlot <- barplotVariable()
    groupingBarplotCheckPlot <- barplotGroupingCheck()
    groupingBarplotVariablePlot <- barplotGroupingVariable()
    numberOfHistogramBins <- numberOfBins()
    
    # Make boxplot
    if(plotChosen == "Boxplot"){
      # Make graph based on conditions
      if(groupingBoxplotCheckPlot == "no") {
        if(facetCheckPlot == "no") {
          final_player_info %>%
            ggplot(aes_string(y = variablePlot)) +
            geom_boxplot() +
            theme_bw()
        } else {
          final_player_info %>%
            ggplot(aes_string(y = variablePlot)) +
            geom_boxplot() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      } else {
        if(facetCheckPlot == "no") {
          final_player_info %>%
            ggplot(aes_string(x = groupingBoxplotVariablePlot, y = variablePlot, fill = groupingBoxplotVariablePlot)) +
            geom_boxplot() +
            guides(fill = "none") +
            theme_bw()
        } else {
          final_player_info %>%
            ggplot(aes_string(x  = groupingBoxplotVariablePlot, y = variablePlot, fill = groupingBoxplotVariablePlot)) +
            geom_boxplot() +
            guides(fill = "none") +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      }
      # Now make the barplot graph
    } else if(plotChosen == "Barplot") {
      # Make graph based on conditions
      if(groupingBarplotCheckPlot == "no") {
        if(facetCheckPlot == "no") {
          final_player_info %>%
            ggplot(aes_string(x = barplotVariablePlot)) +
            geom_bar() +
            theme_bw()
        } else {
          final_player_info %>%
            ggplot(aes_string(x = barplotVariablePlot)) +
            geom_bar() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      } else {
        if(facetCheckPlot == "no") {
          final_player_info %>%
            ggplot(aes_string(x = barplotVariablePlot, fill = groupingBarplotVariablePlot)) +
            geom_bar() +
            theme_bw()
        } else {
          final_player_info %>%
            ggplot(aes_string(x = barplotVariablePlot, fill = groupingBarplotVariablePlot)) +
            geom_bar() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      }
    } else if(plotChosen == "Histogram") {
      if(facetCheckPlot == "no") {
        final_player_info %>%
          ggplot(aes_string(x = variablePlot)) +
          geom_histogram(bins = numberOfHistogramBins) +
          theme_bw()
      } else {
        final_player_info %>%
          ggplot(aes_string(x = variablePlot)) +
          geom_histogram(bins = numberOfHistogramBins) +
          facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
          theme_bw()
      }
    } else {
      NULL
    }
    
  })
    
  
})
