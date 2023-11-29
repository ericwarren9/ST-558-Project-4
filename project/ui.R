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
          sidebarLayout(
            sidebarPanel(
              h3(
                "Select the Exploratory Data Analysis Features You Would Like to See"
              ),
              sliderInput(
                "slider1", "QB's Cap Hit for Particular NFL Season",
                min = min(final_player_info$cap_percent),
                max = max(final_player_info$cap_percent),
                value = c(min(final_player_info$cap_percent),
                          max(final_player_info$cap_percent))
              ),
              sliderInput(
                "slider2", "QB's Passing Yards per Game for Particular NFL Season",
                min = min(final_player_info$passing_yards_per_game),
                max = max(final_player_info$passing_yards_per_game),
                value = c(min(final_player_info$passing_yards_per_game),
                          max(final_player_info$passing_yards_per_game))
              ),
              sliderInput(
                "slider3", "QB's Rushing Yards per Game for Particular NFL Season",
                min = min(final_player_info$rushing_yards_per_game),
                max = max(final_player_info$rushing_yards_per_game),
                value = c(min(final_player_info$rushing_yards_per_game),
                          max(final_player_info$rushing_yards_per_game))
              ),
              pickerInput(
                "picker1", "Select NFL Season(s)",
                choices = str_sort(unique(final_player_info$year), decreasing = T),
                selected = unique(final_player_info$year),
                options = list(`actions-box` = TRUE,
                               create = FALSE,
                               placeholder = "Please Select a Position",
                               onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                               onType = I("function (str) {if (str === \"\") {this.close();}}"),
                               onItemAdd = I("function() {this.close();}")),
                multiple = T
              ),
              selectInput(
                "var", "Graphical Display to View",
                choices = c(
                  "Scatterplot",
                  "Barplot",
                  "Histogram",
                  "Boxplot"
                ),
                selected = "Scatterplot"
              ),
              # Make condition on graph selected checked
              conditionalPanel(
                condition = "input.var == 'Barplot'",
                selectInput(
                  "barplotChoice", "Pick the Variable to Display",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "year"
                )
              ),
              conditionalPanel(
                condition = "['Histogram', 'Boxplot'].indexOf(input.var) > -1",
                selectInput(
                  "histogramChoice", "Pick the Variable to Display",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.numeric))
                  ),
                  selected = "passing_yards_per_game"
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Boxplot'",
                checkboxInput(
                  "boxplotGrouping", "Do you want a boxplot grouping variable?",
                  value = FALSE
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Boxplot' && input.boxplotGrouping == 1",
                selectInput(
                  "boxplotChoice", "Pick a Grouping Variable to Display",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "cap_hit_group"
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Barplot'",
                checkboxInput(
                  "barplotGrouping", "Do you want a barplot grouping variable?",
                  value = FALSE
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Barplot' && input.barplotGrouping == 1",
                selectInput(
                  "barplotChoice2", "Pick a Grouping Variable to Display",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "cap_hit_group"
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Histogram'",
                sliderInput(
                  "bins", "Select the Number of Bins for the Histogram",
                  min = 5,
                  max = 30,
                  value = 10
                )
              ),
              conditionalPanel(
                condition = "input.var == 'Scatterplot'",
                selectInput(
                  "scatterChoice1", "Pick the Variable to Display on the x-axis",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.numeric))
                  ),
                  selected = "passing_yards_per_game"
                ),
                selectInput(
                  "scatterChoice2", "Pick the Variable to Display on the y-axis",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.numeric))
                  ),
                  selected = "passing_tds_per_game"
                ),
                checkboxInput(
                  "scatterCheck1", "Do you want to change the color of points based on a variable?",
                  value = FALSE
                ),
                checkboxInput(
                  "scatterCheck2", "Do you want to change the size of points based on a variable?",
                  value = FALSE
                )
              ),
              checkboxInput(
                "facetPlot", 
                "Do you want to separate (facet) our plot by another variable?",
                value = FALSE
              ),
              conditionalPanel(
                condition = "input.scatterCheck1 == 1 && input.var == 'Scatterplot'",
                selectInput(
                  "scatterChoice3", "Pick the Variable to Display as the color",
                  choices = c(
                    colnames(final_player_info)
                  ),
                  selected = "cap_hit_group"
                )
              ),
              conditionalPanel(
                condition = "input.scatterCheck2 == 1 && input.var == 'Scatterplot'",
                selectInput(
                  "scatterChoice4", "Pick the Variable to Display as the size",
                  choices = c(
                    colnames(final_player_info)
                  ),
                  selected = "turnovers_per_game"
                )
              ),
              conditionalPanel(
                condition = "input.facetPlot == 1",
                selectInput(
                  "facetChoices", "Pick a variable to facet our plots on",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "missed_games"
                )
              )
            ),
            mainPanel(
              plotOutput("graph")
            )
          )
        ),
        
        # Summary tab
        tabPanel(
          "Different EDA Summaries",
          sidebarPanel(
            h3(
              "Select the Exploratory Data Analysis Features You Would Like to See"
            ),
            sliderInput(
              "slider1", "QB's Cap Hit for Particular NFL Season",
              min = min(final_player_info$cap_percent),
              max = max(final_player_info$cap_percent),
              value = c(min(final_player_info$cap_percent),
                        max(final_player_info$cap_percent))
            ),
            sliderInput(
              "slider2", "QB's Passing Yards per Game for Particular NFL Season",
              min = min(final_player_info$passing_yards_per_game),
              max = max(final_player_info$passing_yards_per_game),
              value = c(min(final_player_info$passing_yards_per_game),
                        max(final_player_info$passing_yards_per_game))
            ),
            sliderInput(
              "slider3", "QB's Rushing Yards per Game for Particular NFL Season",
              min = min(final_player_info$rushing_yards_per_game),
              max = max(final_player_info$rushing_yards_per_game),
              value = c(min(final_player_info$rushing_yards_per_game),
                        max(final_player_info$rushing_yards_per_game))
            ),
            pickerInput(
              "picker1", "Select NFL Season(s)",
              choices = str_sort(unique(final_player_info$year), decreasing = T),
              selected = unique(final_player_info$year),
              options = list(`actions-box` = TRUE,
                             create = FALSE,
                             placeholder = "Please Select a Position",
                             onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                             onType = I("function (str) {if (str === \"\") {this.close();}}"),
                             onItemAdd = I("function() {this.close();}")),
              multiple = T
            ),
            selectInput(
              "var2", "Numeric Display to View",
              choices = c(
                "Five Number Summary",
                "Contingency Table",
                "Mean and Standard Deviation",
                "Quantiles"
              ),
              selected = "Five Number Summary"
            ),
            # Make condition on graph selected checked
            conditionalPanel(
              condition = "['Five Number Summary', 'Mean and Standard Deviation', 'Quantiles'].indexOf(input.var2) > -1",
              selectInput(
                "numericChoice1", "Pick the Variable to Get Summary On",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.numeric))
                ),
                selected = "passing_yards_per_game"
              ),
              checkboxInput(
                "numericCheckbox1", 
                "Would you like to add a grouping variable?",
                value = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.numericCheckbox1 == 1 && ['Five Number Summary', 'Mean and Standard Deviation', 'Quantiles'].indexOf(input.var2) > -1",
              selectInput(
                "groupingVariablesForSummaries", 
                "Select your grouping variable",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "cap_hit_group"
              )
            ),
            conditionalPanel(
              condition = "input.var2 == 'Contingency Table'",
              selectInput(
                "contingencyTableSize",
                "What kind of contingency table do you want?",
                choices = c(
                  "One Way", "Two Way", "Three Way"
                ),
                selected = "One Way"
              )
            ),
            conditionalPanel(
              condition = "input.var2 == 'Contingency Table' && input.contingencyTableSize == 'One Way'",
              selectInput(
                "oneWay",
                "Select the Variable for Your One Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "cap_hit_group"
              )
            ),
            conditionalPanel(
              condition = "input.var2  ==  'Contingency Table' && input.contingencyTableSize == 'Two Way'",
              selectInput(
                "twoWay1",
                "Select the First Variable for Your Two Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "cap_hit_group"
              ),
              selectInput(
                "twoWay2",
                "Select the Second Variable for Your Two Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "missed_games"
              )
            ),
            conditionalPanel(
              condition = "input.var2  ==  'Contingency Table' && input.contingencyTableSize == 'Three Way'",
              selectInput(
                "threeWay1",
                "Select the First Variable for Your Three Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "cap_hit_group"
              ),
              selectInput(
                "threeWay2",
                "Select the Second Variable for Your Three Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "missed_games"
              ),
              selectInput(
                "threeWay3",
                "Select the Third Variable for Your Three Way Table",
                choices = c(
                  colnames(dplyr::select_if(final_player_info, is.factor))
                ),
                selected = "year"
              )
            )
          )
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
