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
library(caret)
library(data.table)
library(rvest)
library(janitor)
library(shinytitle)
library(randomForest)


# Read data in ------------------------------------------------------------

final_player_info <- read_rds("player_stats_and_salary.rds") %>%
  mutate(
    cap_hit_group = ifelse(cap_percent > 10, "high",
                           ifelse(cap_percent >= 5, "medium", "low")),
    missed_games = ifelse((as.numeric(year) < 2021) & (games == 16), "no",
                          ifelse((as.numeric(year) >= 2021) & (games == 17), "no", "yes")),
    cap_hit_group = factor(cap_hit_group, levels = c("low", "medium", "high")),
    cap_hit_group = relevel(cap_hit_group, "medium"),
    cap_hit_group = relevel(cap_hit_group, "low"),
    missed_games = factor(missed_games),
    year = factor(year)
  )

# Get salary cap numbers for league for later
salary_cap <- (read_html("https://www.spotrac.com/nfl/cba/") %>% 
                 html_table())[[1]] %>%
  janitor::clean_names() %>%
  dplyr::select(year, cap_maximum) %>%
  mutate(year = as.numeric(year)) %>%
  dplyr::filter((year >= 2007) & (year != 2010) & (year <= 2023)) %>%
  mutate(cap_maximum = parse_number(cap_maximum),
         year = as.character(year))


# The application ---------------------------------------------------------

ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  titlePanel(
    div(span("NFL Quarterbacks Predicted Cap Hit by Performance", style = "color:black", align = "center"),
        align = "center",
        br(),
    ), 
    windowTitle =  "NFL Quarterbacks Predicted Cap Hit by Performance"
  ),
  shinytitle::use_shiny_title(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             ".shiny-output-warning { visibility: hidden; }",
             ".shiny-output-warning:before { visibility: hidden; }"
  ),
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
                min = min(final_player_info$cap_percent) - 1,
                max = max(final_player_info$cap_percent) + 1,
                value = c(min(final_player_info$cap_percent) - 1,
                          max(final_player_info$cap_percent) + 1)
              ),
              sliderInput(
                "slider2", "QB's Passing Yards per Game for Particular NFL Season",
                min = min(final_player_info$passing_yards_per_game) - 1,
                max = max(final_player_info$passing_yards_per_game)+ 1,
                value = c(min(final_player_info$passing_yards_per_game) - 1,
                          max(final_player_info$passing_yards_per_game) + 1)
              ),
              sliderInput(
                "slider3", "QB's Rushing Yards per Game for Particular NFL Season",
                min = min(final_player_info$rushing_yards_per_game) - 1,
                max = max(final_player_info$rushing_yards_per_game) + 1,
                value = c(min(final_player_info$rushing_yards_per_game) - 1,
                          max(final_player_info$rushing_yards_per_game) + 1)
              ),
              pickerInput(
                "picker1", "Select NFL Season(s)",
                choices = str_sort(unique(final_player_info$year), decreasing = T),
                selected = unique(final_player_info$year),
                options = list(`actions-box` = TRUE,
                               create = FALSE,
                               placeholder = "Please Select a Season",
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
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "missed_games"
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
          sidebarLayout(
            sidebarPanel(
              h3(
                "Select the Exploratory Data Analysis Features You Would Like to See"
              ),
              sliderInput(
                "slider4", "QB's Cap Hit for Particular NFL Season",
                min = min(final_player_info$cap_percent) - 1,
                max = max(final_player_info$cap_percent) + 1,
                value = c(min(final_player_info$cap_percent) - 1,
                          max(final_player_info$cap_percent) + 1)
              ),
              sliderInput(
                "slider5", "QB's Passing Yards per Game for Particular NFL Season",
                min = min(final_player_info$passing_yards_per_game) - 1,
                max = max(final_player_info$passing_yards_per_game) + 1,
                value = c(min(final_player_info$passing_yards_per_game) - 1,
                          max(final_player_info$passing_yards_per_game) + 1)
              ),
              sliderInput(
                "slider6", "QB's Rushing Yards per Game for Particular NFL Season",
                min = min(final_player_info$rushing_yards_per_game) - 1,
                max = max(final_player_info$rushing_yards_per_game) + 1,
                value = c(min(final_player_info$rushing_yards_per_game) - 1,
                          max(final_player_info$rushing_yards_per_game) + 1)
              ),
              pickerInput(
                "picker2", "Select NFL Season(s)",
                choices = str_sort(unique(final_player_info$year), decreasing = T),
                selected = unique(final_player_info$year),
                options = list(`actions-box` = TRUE,
                               create = FALSE,
                               placeholder = "Please Select a Season",
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
                ),
                checkboxInput(
                  "numericCheckbox2", 
                  "Would you like to add a second grouping variable?",
                  value = FALSE
                )
              ),
              conditionalPanel(
                condition = "input.numericCheckbox1 == 1 && input.numericCheckbox2 == 1 && ['Five Number Summary', 'Mean and Standard Deviation', 'Quantiles'].indexOf(input.var2) > -1",
                selectInput(
                  "groupingVariablesForSummaries2", 
                  "Select your second grouping variable",
                  choices = c(
                    colnames(dplyr::select_if(final_player_info, is.factor))
                  ),
                  selected = "year"
                )
              ),
              conditionalPanel(
                condition = "input.var2 == 'Mean and Standard Deviation'",
                sliderInput(
                  "trim", "Select the fraction of observations to be trimmed from each end before the mean is computed. 0 calculates all observations and 0.5 will calculate the median",
                  min = 0,
                  max = 0.5,
                  value = 0
                )
              ),
              conditionalPanel(
                condition = "input.var2 == 'Quantiles'",
                textInput(
                  "quantiles", 
                  "Quantile percentiles (enter numbers between 0 and 1 and separate values by a comma)", 
                  placeholder = "Enter values separated by a comma..."
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
            ),
            mainPanel(
              tableOutput("numericalSummary")
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
          "Modeling Info",
          uiOutput("model_about1"),
          br(),
          withMathJax(uiOutput("model_about2")),
          br(),
          uiOutput("model_about3"),
          br(),
          uiOutput("model_about4"),
          br(),
          uiOutput("model_about5"),
          br(),
          withMathJax(uiOutput("model_about6")),
          br(),
          uiOutput("model_about7"),
          br(),
          uiOutput("model_about8")
        ),
        # Modeling Fitting tab
        tabPanel(
          "Model Fitting",
          sidebarLayout(
            sidebarPanel(
              h3(
                "Select Different Modeling Procedures you Want to Control"
              ),
              sliderInput(
                "modelSlider1", 
                "Proportion of data going to training set (rest goes to testing set)",
                min = 0,
                max = 1,
                value = .75
              ),
              pickerInput(
                "modelPicker1", "Select columns to include in models",
                choices = colnames(dplyr::select(final_player_info, -c(cap_hit_group, missed_games, cap_percent))),
                selected = colnames(dplyr::select(final_player_info, -c(cap_hit_group, missed_games, cap_percent))),
                options = list(`actions-box` = TRUE,
                               create = FALSE,
                               placeholder = "Please Select Variables to Include",
                               onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                               onType = I("function (str) {if (str === \"\") {this.close();}}"),
                               onItemAdd = I("function() {this.close();}")),
                multiple = T
              ),
              sliderInput(
                "modelSlider2", 
                "Pick the range for the number of predictors the random forest model uses (remember to choose at least one less than the number of predictors chosen)",
                min = 1,
                max = ncol(dplyr::select(final_player_info, -c(cap_hit_group, missed_games))) - 1,
                value = c(1, ceiling(sqrt(ncol(dplyr::select(final_player_info, -c(cap_hit_group, missed_games))) - 1)))
              ),
              sliderInput(
                "modelSlider3", 
                "Number of times to cross validate our data",
                min = 3,
                max = 10,
                value = 5
              ),
              sliderInput(
                "modelSlider4", 
                "Number of times to repeat the cross validation of data",
                min = 1,
                max = 3,
                value = 1
              ),
            ),
            mainPanel(
              tableOutput("rmseTable"),
              br(),
              uiOutput("better_model1"),
              br(),
              uiOutput("model_text1"),
              verbatimTextOutput("summaryMLR"),
              br(),
              uiOutput("model_text2"),
              plotOutput("varImpPlot")
            )
          )
        ),
        # Prediction tab
        tabPanel(
          "Prediction",
          sidebarLayout(
            sidebarPanel(
              h3(
                "Build Your Own NFL QB by Inputting Their Statistics in a Given Season"
              ),
              pickerInput(
                "modelPicker2", "Select what year our 'player' played",
                choices = unique(final_player_info$year),
                selected = "2022",
                options = list(`actions-box` = TRUE,
                               create = FALSE,
                               placeholder = "Please Select a Season",
                               onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
                               onType = I("function (str) {if (str === \"\") {this.close();}}"),
                               onItemAdd = I("function() {this.close();}")),
                multiple = F
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('passing_percentage') >= 0",
                numericInput(
                  "modelSlider5", 
                  "What is the quarterback's passing percentage? Between 0 and 100.",
                  value = round(mean(final_player_info$passing_percentage), 2),
                  min = 0,
                  max = 100
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('passing_yards_per_game') >= 0",
                numericInput(
                  "modelSlider6", 
                  "What is the quarterback's average passing yards per game? Between 0 and 400.",
                  min = 0,
                  max = 400,
                  value = round(mean(final_player_info$passing_yards_per_game), 2)
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('passing_yards_per_attempt') >= 0",
                numericInput(
                  "modelSlider7", 
                  "What is the quarterback's passing yards per attempt? Between 0 and 15.",
                  min = 0,
                  max = 15,
                  value = round(mean(final_player_info$passing_yards_per_attempt), 2)
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('passing_tds_per_game') >= 0",
                numericInput(
                  "modelSlider8", 
                  "What is the quarterback's passing touchdowns for the season? Integer between 0 and 65",
                  min = 0,
                  max = 65,
                  value = ceiling(mean(final_player_info$passing_tds_per_game) * mean(final_player_info$games))
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('rushing_yards_per_game') >= 0",
                numericInput(
                  "modelSlider9", 
                  "What is the quarterback's rushing yards per game? Between -10 and 100.",
                  min = -10,
                  max = 100,
                  value = round(mean(final_player_info$rushing_yards_per_game), 2)
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('rushing_tds_per_game') >= 0",
                numericInput(
                  "modelSlider10", 
                  "What is the quarterback's rushing touchdowns for the season? Integer between 0 and 34.",
                  min = 0,
                  max = 34,
                  value = ceiling(mean(final_player_info$rushing_tds_per_game) * mean(final_player_info$games))
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('games') >= 0",
                numericInput(
                  "modelSlider11", 
                  "What is the quarterback's number of games played in a season? (We will assume they were the starter and not injured much.) Integer between 10 and 17",
                  min = 10,
                  max = 17,
                  value = ceiling(mean(final_player_info$games))
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('sacks_per_game') >= 0",
                numericInput(
                  "modelSlider12", 
                  "What is the quarterback's number of sacks for the season? Integer between 0 and 100",
                  min = 0,
                  max = 100,
                  value = ceiling(mean(final_player_info$sacks_per_game) * mean(final_player_info$games))
                )
              ),
              conditionalPanel(
                condition = "input.modelPicker1.indexOf('turnovers_per_game') >= 0",
                numericInput(
                  "modelSlider13", 
                  "What is the quarterback's number of turnovers for the season? Integer between 0 and 45.",
                  min = 0,
                  max = 45,
                  value = ceiling(mean(final_player_info$turnovers_per_game) * mean(final_player_info$games))
                )
              )
            ),
            mainPanel(
              tableOutput("predictionTable"),
              br(),
              uiOutput("better_model2")
            )
          )
        )
      )
    )
  )
)
