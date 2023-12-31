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


# Start the server section of app -----------------------------------------

shinyServer(function(input, output, session) {

  # Get text for first paragraph for about tab
  output$about_text1 <- renderUI({
    paste0("In this section, we are going to examine how much (or what percentage of) a NFL quarterback's salary is represented by the NFL's salary cap maximum. We are going to see how much performance can affect this number. Can we then use a quarterback's statistics to try to predict what their salary should be in a given year? We are going to use data from the NFL seasons of 2007 to 2022 (excluding 2010 since it was a lockout year and salaries and player statistics are not representative of a normal year) to answer this question. Please note that all data for quarterbacks is filtered ahead of time for players who are deemed 'starters' which means they played at least ten games and had at least 100 passing attempts. 2007 was a weird year with many injuries so not as much 'valid' data is present for us to analyze.")
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
    tagList("To gather this data, we had to use three sources to get the information we wanted on a player's statistics, a player's salary, and the NFL's salary cap maximum in a given season (use to calculate what percentage the player was making from the team's available budget). Please feel free to observe the corresponding links that will take you to ", link1, ", ", link2, ", and ", link3, ". Data was then cleaned and filtered it in a way that can be used analysis. Please note that the player's statistics are on a per game basis so the player will not be penalized more in the analysis for missing more games. This helps make predictoin more accurate and does not weigh the number of games variable even more. Instead this essentially standardizes our data. If you are interested in how this was done (with the corresponding R code), please feel free to check out ", link4, ". If you would like to see the analyzed ", link5, " or the ", link6, " , please feel free to view them as well.")
  })
  
  # Get text for third paragraph for about tab
  output$about_text3 <- renderUI({
    HTML(paste0("As you are going through this application, you might notice that there are three tabs. The first one is the ", strong("About"), " tab, which is the one you are currently on. This tab will give you information about what the project is about, the data sources, functions of each tab, and images at the end of the page to help you understand what the purpose of the project. The next tab is the ", strong("Data Exploration"), " tab. This section will allow the user to create different numerical and graphical summaries from our data. This section's purpose is to perform the user's own version of exploratory data analysis in which they can gain their own conclusions. There are many different plots and options the user can select, which might allow different interpretations of the data. Lastly, there is a ", strong("Modeling"), " tab. This section allows the user to fit two types of supervised learning models. In our case, we will be looking at using a multiple linear regression model and a random forest model. The user will be able to select which variables they would like to use as predictors which in return will help fit an optimal model. This selected model will be used in making predictions and finding a NFL quarterback's monetary worth to their team. I hope as the user that you enjoy the application and gain some insights on how much a NFL quarterback should be earning."))
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
  
  # Get the x variable in the scatterplot
  scatterX <- reactive({
    req("input$scatterChoice1")
    input$scatterChoice1
  })
  
  # Get the y variable in the scatterplot
  scatterY <- reactive({
    req("input$scatterChoice2")
    input$scatterChoice2
  })
  
  # Check to see if we should add color to the scatterplot
  scatterplotColorCheck <- reactive({
    req("input$scatterCheck1")
    if(input$scatterCheck1 == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Figure out the column used for color of scatterplot
  scatterplotColorColumn <- reactive({
    req("input$scatterChoice3")
    input$scatterChoice3
  })
  
  # Check to see if we should add size to the scatterplot
  scatterplotSizeCheck <- reactive({
    req("input$scatterCheck2")
    if(input$scatterCheck2 == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Figure out the column used for size of scatterplot
  scatterplotSizeColumn <- reactive({
    req("input$scatterChoice4")
    input$scatterChoice4
  })
  
  # Filter data for graphs
  data_filtered <- reactive({
    final_player_info %>%
      filter(
        (cap_percent >= input$slider1[1]) & 
        (cap_percent <= input$slider1[2]) & 
        (passing_yards_per_game >= input$slider2[1]) &
        (passing_yards_per_game <= input$slider2[2]) & 
        (rushing_yards_per_game >= input$slider3[1]) & 
        (rushing_yards_per_game <= input$slider3[2]) &
        (year %in% input$picker1)
      )
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
    scatterXVar <- scatterX()
    scatterYVar <- scatterY()
    scatterColorCheck <- scatterplotColorCheck()
    scatterColorColumn <- scatterplotColorColumn()
    scatterSizeCheck <- scatterplotSizeCheck()
    scatterSizeColumn <- scatterplotSizeColumn()
    filtered_data <- data_filtered()
    
    # Make boxplot
    if(plotChosen == "Boxplot"){
      
      # Make graph based on conditions
      if(groupingBoxplotCheckPlot == "no") {
        
        if(facetCheckPlot == "no") {
          
          filtered_data %>%
            ggplot(aes_string(y = variablePlot)) +
            geom_boxplot() +
            theme_bw()
          
        } else {
          
          filtered_data %>%
            ggplot(aes_string(y = variablePlot)) +
            geom_boxplot() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
          
        }
      } else {
        
        if(facetCheckPlot == "no") {
          
          filtered_data %>%
            ggplot(aes_string(x = groupingBoxplotVariablePlot, y = variablePlot, fill = groupingBoxplotVariablePlot)) +
            geom_boxplot() +
            guides(fill = "none") +
            theme_bw()
          
        } else {
          
          filtered_data %>%
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
          filtered_data %>%
            ggplot(aes_string(x = barplotVariablePlot)) +
            geom_bar() +
            theme_bw()
          
        } else {
          
          filtered_data %>%
            ggplot(aes_string(x = barplotVariablePlot)) +
            geom_bar() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      } else {
        
        if(facetCheckPlot == "no") {
          
          filtered_data %>%
            ggplot(aes_string(x = barplotVariablePlot, fill = groupingBarplotVariablePlot)) +
            geom_bar() +
            theme_bw()
          
        } else {
          
          filtered_data %>%
            ggplot(aes_string(x = barplotVariablePlot, fill = groupingBarplotVariablePlot)) +
            geom_bar() +
            facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
            theme_bw()
        }
      }
    } else if(plotChosen == "Histogram") {
      
      if(facetCheckPlot == "no") {
        
        filtered_data %>%
          ggplot(aes_string(x = variablePlot)) +
          geom_histogram(bins = numberOfHistogramBins) +
          theme_bw()
        
      } else {
        
        filtered_data %>%
          ggplot(aes_string(x = variablePlot)) +
          geom_histogram(bins = numberOfHistogramBins) +
          facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
          theme_bw()
      }
    } else {
      
      if(scatterColorCheck == "no") {
        
        if(scatterSizeCheck == "no") {
          
          if(facetCheckPlot == "no") {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar)) +
              geom_point(alpha = 0.3) +
              theme_bw()
            
          } else {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar)) +
              geom_point(alpha = 0.3) +
              facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
              theme_bw()
          }
          
        } else {
          
          if(facetCheckPlot == "no"){
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, size = scatterSizeColumn)) +
              geom_point(alpha = 0.3) +
              theme_bw()
            
          } else {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, size = scatterSizeColumn)) +
              geom_point(alpha = 0.3) +
              facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
              theme_bw()
          }
        }
      } else {
        
        if(scatterSizeCheck == "no") {
          
          if(facetCheckPlot == "no") {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, color = scatterColorColumn)) +
              geom_point(alpha = 0.3) +
              theme_bw()
          } else {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, color = scatterColorColumn)) +
              geom_point(alpha = 0.3) +
              facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
              theme_bw()
          }
          
        } else {
          
          if(facetCheckPlot == "no"){
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, size = scatterSizeColumn, color = scatterColorColumn)) +
              geom_point(alpha = 0.3) +
              theme_bw()
            
          } else {
            
            filtered_data %>%
              ggplot(aes_string(x = scatterXVar, y = scatterYVar, size = scatterSizeColumn, color = scatterColorColumn)) +
              geom_point(alpha = 0.3) +
              facet_wrap(~ get(facetVariablePlot), labeller = label_value) +
              theme_bw()
          }
        }
      }
    }
  })
  
  # Filter data for graphs
  data_filtered2 <- reactive({
    final_player_info %>%
      filter(
        (cap_percent >= input$slider4[1]) & 
        (cap_percent <= input$slider4[2]) & 
        (passing_yards_per_game >= input$slider5[1]) &
        (passing_yards_per_game <= input$slider5[2]) & 
        (rushing_yards_per_game >= input$slider6[1]) & 
        (rushing_yards_per_game <= input$slider6[2]) &
        (year %in% input$picker2)
      )
  })
  
  # Check which summary is selected
  plotCheckSummary <- reactive({
    req("input$var2")
    input$var2
  })
  
  # Check which column is used for summary
  summaryVariable <- reactive({
    req("input$numericChoice1")
    input$numericChoice1
  })
  
  # Make grouping variable checkbox condition
  groupingSummaryCheck <- reactive({
    req("input$numericCheckbox1")
    if(input$numericCheckbox1 == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Get grouping variable for summaries
  groupingColumnSummary <- reactive({
    req("input$groupingVariablesForSummaries")
    input$groupingVariablesForSummaries
  })
  
  # Make second grouping variable checkbox condition
  groupingSummaryCheck2 <- reactive({
    req("input$numericCheckbox2")
    if(input$numericCheckbox2 == 1){
      "yes"
    } else {
      "no"
    }
  })
  
  # Get second grouping variable for summaries
  groupingColumnSummary2 <- reactive({
    req("input$groupingVariablesForSummaries2")
    input$groupingVariablesForSummaries2
  })
  
  # Make trimmed mean and sd checkbox condition
  trimmedValues <- reactive({
    req("input$trim")
    input$trim
  })
  
  # Make quantile readability number function
  quantileExtract <- function(text) {
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed = FALSE)[[1]]
    numbers <- as.numeric(split)
    unlist(numbers)
  }
  
  # Get quantile values from input values
  quantileValuesInput <- reactive({
    req("input$quantiles")
    input$quantiles
  })
  
  # Get the type of contingency table
  whichTableInput <- reactive({
    req("input$contingencyTableSize")
    input$contingencyTableSize
  })
  
  # Get value of one way table
  oneWayTableValue <- reactive({
    req("input$oneWay")
    input$oneWay
  })
  
  # Get first value of two way table
  twoWayTableValue1 <- reactive({
    req("input$twoWay1")
    input$twoWay1
  })
  
  # Get second value of two way table
  twoWayTableValue2 <- reactive({
    req("input$twoWay2")
    input$twoWay2
  })
  
  # Get first value of three way table
  threeWayTableValue1 <- reactive({
    req("input$threeWay1")
    input$threeWay1
  })
  
  # Get second value of three way table
  threeWayTableValue2 <- reactive({
    req("input$threeWay2")
    input$threeWay2
  })
  
  # Get third value of three way table
  threeWayTableValue3 <- reactive({
    req("input$threeWay3")
    input$threeWay3
  })
  
  # Make table of summaries
  output$numericalSummary <- renderTable({
    
    # Get variables needed
    filtered_data2 <- data_filtered2()
    summaryUsed <- plotCheckSummary()
    variableSummary <- summaryVariable()
    groupSummaryCheck <- groupingSummaryCheck()
    groupColumnSummary <- groupingColumnSummary()
    groupSummaryCheck2 <- groupingSummaryCheck2()
    groupColumnSummary2 <- groupingColumnSummary2()
    trimValues <- trimmedValues()
    quantileValues <- quantileValuesInput()
    tableSize <- whichTableInput()
    oneWayValue <- oneWayTableValue()
    twoWayValue1 <- twoWayTableValue1()
    twoWayValue2 <- twoWayTableValue2()
    threeWayValue1 <- threeWayTableValue1()
    threeWayValue2 <- threeWayTableValue2()
    threeWayValue3 <- threeWayTableValue3()
    
    # Make correct table
    
    # Do 5 number summary
    if(summaryUsed == "Five Number Summary") {
      
      if(groupSummaryCheck == "no") {
        
        filtered_data2 %>%
          summarize(
            n = n(),
            min = min(get(variableSummary)),
            Q1 = quantile(get(variableSummary), 0.25),
            median = median(get(variableSummary)),
            Q3 = quantile(get(variableSummary), 0.75),
            max = max(get(variableSummary)),
          ) %>%
          distinct()
        
      } else {
        
        if(groupSummaryCheck2 == "no") {
          
          filtered_data2 %>%
            group_by("Values from Grouping Variable Selected" = get(groupColumnSummary)) %>%
            summarize(
              n = n(),
              min = min(get(variableSummary)),
              Q1 = quantile(get(variableSummary), 0.25),
              median = median(get(variableSummary)),
              Q3 = quantile(get(variableSummary), 0.75),
              max = max(get(variableSummary)),
            ) %>%
            distinct()
          
        } else {
          filtered_data2 %>%
            group_by("Values from First Grouping Variable Selected" = get(groupColumnSummary), "Values from Second Grouping Variable Selected" = get(groupColumnSummary2)) %>%
            summarize(
              n = n(),
              min = min(get(variableSummary)),
              Q1 = quantile(get(variableSummary), 0.25),
              median = median(get(variableSummary)),
              Q3 = quantile(get(variableSummary), 0.75),
              max = max(get(variableSummary)),
            ) %>%
            distinct()
        }
      }
      
      # Do Mean and sd next
    } else if(summaryUsed == "Mean and Standard Deviation") {
      
      if(groupSummaryCheck == "no") {
        
        filtered_data2 %>%
          summarize(
            n = n(),
            mean = mean(get(variableSummary), na.rm = TRUE, trim = trimValues),
            sd = sd(get(variableSummary), na.rm = TRUE)
          ) %>%
          distinct()
          
        } else {
          
          if(groupSummaryCheck2 == "no") {
          
          filtered_data2 %>%
            group_by("Values from Grouping Variable Selected" = get(groupColumnSummary)) %>%
            summarize(
              n = n(),
              mean = mean(get(variableSummary), na.rm = TRUE, trim = trimValues),
              sd = sd(get(variableSummary), na.rm = TRUE)
            ) %>%
            distinct()
          } else {
            
            filtered_data2 %>%
              group_by("Values from First Grouping Variable Selected" = get(groupColumnSummary), "Values from Second Grouping Variable Selected" = get(groupColumnSummary2)) %>%
              summarize(
                n = n(),
                mean = mean(get(variableSummary), na.rm = TRUE, trim = trimValues),
                sd = sd(get(variableSummary), na.rm = TRUE)
              ) %>%
              distinct()
        }
      }
      # Do the quartiles next
    } else if(summaryUsed == "Quantiles") {
      
      if(groupSummaryCheck == "no") {
        filtered_data2 %>%
          summarize(
            quantile = quantileExtract(quantileValues),
            result = quantile(get(variableSummary), 
                              probs = quantileExtract(quantileValues))
          ) %>%
          distinct()
        
      } else {
        
        if(groupSummaryCheck2 == "no") {
          
          filtered_data2 %>%
            group_by("Values from Grouping Variable Selected" = get(groupColumnSummary)) %>%
            summarize(
              quantile = quantileExtract(quantileValues),
              result = quantile(get(variableSummary), 
                                probs = quantileExtract(quantileValues))
            ) %>%
            distinct()
          
        } else {
          
          filtered_data2 %>%
            group_by("Values from First Grouping Variable Selected" = get(groupColumnSummary), "Values from Second Grouping Variable Selected" = get(groupColumnSummary2)) %>%
            summarize(
              quantile = quantileExtract(quantileValues),
              result = quantile(get(variableSummary), 
                                probs = quantileExtract(quantileValues))
            ) %>%
            distinct()
        }
      }
      # Do the Contingency Table last
    } else {
      
      if(tableSize == "One Way") {
        
        with(filtered_data2, table(droplevels(get(oneWayValue))))
        
      } else if(tableSize == "Two Way") {
        
        with(filtered_data2, table(droplevels(get(twoWayValue1)), droplevels(get(twoWayValue2))))
        
      } else {
        
        with(filtered_data2, table(droplevels(get(threeWayValue1)), droplevels(get(threeWayValue2)), droplevels(get(threeWayValue3))))
      }
    }
  })
  
  # Start the write up for the about section of the Modeling tab
  output$model_about1 <- renderText({
    HTML(paste0("In this section, we are going to examine two different modeling techniques, which are ", strong("multiple linear regression"), " and  ", strong("random forest"), "."))
  })
  
  output$model_about2 <- renderUI({
    
    withMathJax(HTML(paste0("Multiple linear regression is a statistical modeling technique that uses several explanatory (or also known as predictor) variables to predict the outcome of a response variable. This is an extention of ", strong("simple linear regression"), ", in which we only use one variable to predict our response. In our case, we are trying to predict the ", strong("cap hit percentage"), " of a NFL quarterback based on how well they are performing. If you want to understand the math behind a multiple linear regression model, we can write this model's equation as $$\\hat{y} = \\beta_0 + \\beta_1 x_1 + ... + \\beta_n x_n$$ where each beta value represents the partial slope of each predictor. In less math terms, beta could be said to represent the average amount by which the dependent (or response) variable increases when only that one particular independent (predictor) variable increases one unit AND the other independent (predictor) variables are held constant (meaning they do not change at all). We should also note that each x value represents the variable we are using as predictors and y represents our predicted response value generated from our formula on the right hand side.")))
  })
  
  output$model_about3 <- renderUI({
    
    paste0("In multiple linear regression, there are two major advantages to analyzing data using this modeling approach. The first is the ability to determine the relative influence of one or more predictor variables to the response. For example, we can do hypothesis testing to see which predictors have a partial slope that is statistically different from zero (meaning that the predictor is valuable to used in final prediction purposes). The other advantage is that they are easy to implement and analyze. It is really easy using some basic statistical software (like R that are using now) to make a multiple linear regression model. They are also easy to predict as most users have learned this skill using slope and intercept values (y = mx + b) since middle school.")
  })
  
  output$model_about4 <- renderUI({
    
    paste0("Some disadvantages include that they have some required assumptions that must be met in order to accurately fit the model. The five main assumptions underlying multiple regression models must satisfy are linearity, homoskedasticity, independence of errors, normality, and independence of independent variables. If one of these assumptions are violated then our predicted values will not be reliable. Another disadvantage is that these models are prone to overfitting, which occurs when we include too many independent variables; as a result, leading to unreliable predictions. Despite these limitations, we are still going to select this model as one of our choices to make predictions on our data. In this application, we can use the summary table to eventually select only the 'important' variables, which a rule of thumb is only selecting predictors where their alpha level is less than 0.05. Again this is up to the user, as you might want to keep a variable you think is extremely important.")
  })
  
  output$model_about5 <- renderUI({
    
    paste0("A random forest model is an ensemble learning method for classification, regression and other tasks which operates by making many decision trees on our data. It can also be described as an algorithm that utilizes both repeated sampling and feature randomness to create an uncorrelated forest of decision trees. In this case, we are going to use it for its regression purposes since our response variable is a continuous variable that cannot be classified.")
  })
  
  output$model_about6 <- renderUI({
    
    withMathJax(paste0("How the process of making a random forest model is going to work is that for some value, which we will call b = 1 to B, we will first draw a bootstrap sample of size n from the training data. Then, we will grow a random forest tree (for the purpose of the equations later we will call T) to the bootstrapped data and repeat the following steps for each terminal node of the tree, until the minimum node size is reached: (a) select m variables at random from the p predictor variables and usually this value of m is the square root of p (or the number of predictor variables); (b) pick the best splitting-point among the m variables; and (c) split the node into two 'children' nodes. After doing these steps we will get our ensemble of trees. Then to make a regression prediction at whatever our input data (which we will call x), we will use the equation $$\\hat{y}(x) = \\dfrac{1}{B} \\sum_{b=1}^B T_b(x)$$ where B is the number of trees we made and T is the specific tree itself. This is essentially just taking an average of all of our trees."))
  })
  
  output$model_about7 <- renderUI({
    
    paste0("An advantage of using a random forest model is the power of handling large data sets with higher dimensionality and identifies the most significant variables for us. We do not have to do to do any subsetting or step procedures like with other models and random forest outputs the importance of all the variables in order for most to least, to easily interpret the results of what was being used to others. Another advantage is its effectiveness for estimating missing data and maintaining accuracy when a large proportion of data is missing. The last major advantage of using random forest is how it involves sampling with replacement and here one third of data is not used for training our model. Therefore, it can be used for testing (called out of bag samples), which is as accurate as using a test set for validation and removes the need for a test set.")
  })
  
  output$model_about8 <- renderUI({
    
    paste0("Random forest also has some drawbacks. The first is that random forest for regression does not give as precise predictions as it does in classification. In case of regression, it doesn't predict beyond the range in the training data, which can cause overfitting. Another disadvantage is that random forest modeling can feel like a black box approach, as we have very little control on what the model does. We can try different parameters and random seeds to have some input but otherwise there is little we can do. Thus, it can be hard to interpret results in saying why the model performed specific actions or how it determined what variables were more important. Despite these drawbacks we are still going to use this modeling technique because of how powerful it is.")
  })
  
  # Split data into test and training set
  index <- reactive({
    set.seed(999)
    req("input$modelSlider1")
    createDataPartition(final_player_info$cap_percent, p = input$modelSlider1, list = FALSE)
  })
  
  trainingData <- reactive({
    final_player_info[index(), ]
  })
  
  testingData <- reactive({
    final_player_info[-index(), ]
  })
  
  # Make MLR model
  mlrModel <- reactive({
    train(
      as.formula(paste("cap_percent ~ ",paste(input$modelPicker1, collapse="+"))),
      data = trainingData(),
      method = "lm",
      preProcess = c("scale", "center"),
      trControl = trainControl(method = "repeatedcv", number = input$modelSlider3, repeats = input$modelSlider4)
    )
  })
  
  # Update mtry values
  observe({
    updateSliderInput(
      session, 
      "modelSlider2", 
      min = 1,
      max = max(ceiling(length(input$modelPicker1)) - 1, 1),
      value = c(1, max(ceiling(sqrt(length(input$modelPicker1))), 1))
    )
  })
  
  # Make random forest model
  rfModel <- reactive({
    set.seed(999)
    train(
      as.formula(paste("cap_percent ~ ",paste(input$modelPicker1, collapse="+"))),
      data = trainingData(),
      method = "rf",
      preProcess = c("scale", "center"),
      trControl = trainControl(method = "repeatedcv", 
                               number = input$modelSlider3, 
                               repeats = input$modelSlider4),
      tuneGrid = data.frame(mtry = input$modelSlider2[1]:input$modelSlider2[2])
    )
  })
  
  # Get MLR model predictions
  mlrPreds <- reactive({
    predict(mlrModel(), testingData())
  })
  
  # Get random forest predictions
  rfPreds <- reactive({
    predict(rfModel(), testingData())
  })
  
  # Get RMSE values for table
  output$rmseTable <- renderTable({
    rsme_cats <- c(rep("train", 2), rep("test", 2))
    rmse_model <- rep(c("Multiple Linear Regression", "Random Forest"), 2)
    rmse_values <- c(mlrModel()$resample$RMSE[1], rfModel()$resample$RMSE[1], RMSE(mlrPreds(), testingData()$cap_percent)[1], RMSE(rfPreds(), testingData()$cap_percent)[1])
    
    cbind("Train or Test Data?" = rsme_cats, "Model" = rmse_model, "RMSE Value" = rmse_values)
  })
  
  # Have a line of text that helps with showing which model is better
  output$better_model1 <- renderText({
    if(RMSE(mlrPreds(), testingData()$cap_percent)[1] < RMSE(rfPreds(), testingData()$cap_percent)[1]) {
      
      paste0("The multiple linear regression model is better for predicting since it has a lower test RMSE value.")
      
    } else if(RMSE(mlrPreds(), testingData()$cap_percent)[1] == RMSE(rfPreds(), testingData()$cap_percent)[1]) {
      
      paste0("Both models are deemed equal for predicting purposes since they have the same test RMSE value.")
      
    } else {
      paste0("The random forest model is better for predicting since it has a lower test RMSE value.")
    }
  })
  
  # Title for showing the summary of mlr
  output$model_text1 <- renderText({
    paste0("Please check out the summary of the multiple linear regression model.")
  })
  
  # Get summary values for MLR
  output$summaryMLR <- renderPrint({
    summary(mlrModel())
  })
  
  # Title for showing the variable importance of rf
  output$model_text2 <- renderText({
    paste0("Please check out the following output ranking which variables were the most important for the random forest model.")
  })
  
  # Make the variable importance plot
  output$varImpPlot <- renderPlot({
    plot(varImp(rfModel()))
  })
  
  # Update the number of games slider dependent on the year (played 16 games in 2020 and earlier; 17 games in 2021 and later)
  observe({
    
    if(as.numeric(input$modelPicker2) <= 2020){
      updateSliderInput(
        session, 
        "modelSlider11", 
        value = mean(final_player_info$games),
        min = 10, 
        max = 16,
      )
    } else {
      updateSliderInput(
        session, 
        "sliderValue", 
        value = mean(final_player_info$games),
        min = 10, 
        max = 17,
      )
    }
  })
  
  # Make the actual prediction value for both models
  output$predictionTable <- renderTable({
    
    # Make the cap percentage prediction for the mlr model
    mlrPrediction <- max(round(predict(mlrModel(), newdata = data.frame(passing_yards_per_game = input$modelSlider6, games = input$modelSlider11, rushing_yards_per_game = input$modelSlider9, passing_percentage = input$modelSlider5, passing_tds_per_game = input$modelSlider8 / input$modelSlider11, passing_yards_per_attempt = input$modelSlider7, rushing_tds_per_game = input$modelSlider10 / input$modelSlider11, turnovers_per_game = input$modelSlider13 / input$modelSlider11, sacks_per_game = input$modelSlider12 / input$modelSlider11,  year = input$modelPicker2)), 3), 0)
    
    # Make the cap percentage prediction for the mlr model
    rfPrediction <- max(round(predict(rfModel(), newdata = data.frame(passing_yards_per_game = input$modelSlider6, games = input$modelSlider11, rushing_yards_per_game = input$modelSlider9, passing_percentage = as.numeric(input$modelSlider5), passing_tds_per_game = input$modelSlider8 / input$modelSlider11, passing_yards_per_attempt = input$modelSlider7, rushing_tds_per_game = input$modelSlider10 / input$modelSlider11, turnovers_per_game = input$modelSlider13 / input$modelSlider11, sacks_per_game = input$modelSlider12 / input$modelSlider11,  year = input$modelPicker2)), 3), 0)
    
    # Make the model types column for the table
    modelType <- c("Multiple Linear Regression", "Random Forest")
    
    # Get the year for the table
    modelYear <- rep(input$modelPicker2, 2)
    
    # Make the season multiplier
    seasonMultiplier <- salary_cap[salary_cap$year == input$modelPicker2, 2][[1]]
    
    # Function to get salaries in dollar amounts
    format_money  <- function(x, ...) {
      paste0("$", formatC(as.numeric(x), format="f", digits=2, big.mark=","))
    }
    
    # Get the chosen year salary
    mlrChosenYearSalary <- format_money(mlrPrediction * seasonMultiplier / 100)
    rfChosenYearSalary <- format_money(rfPrediction * seasonMultiplier / 100)
    
    # Get the 2023 (current season) cap hit salaries
    mlr2023Salary <- format_money(mlrPrediction * salary_cap[salary_cap$year == "2023", 2][[1]] / 100)
    rf2023Salary <- format_money(rfPrediction * salary_cap[salary_cap$year == "2023", 2][[1]] / 100)
    
    # Combine into table
    cbind(
      "Model Type" = modelType, 
      "Year Being Modeled" = modelYear, 
      "Cap Percentage Predicted" = c(mlrPrediction, rfPrediction),
      "Chosen Year Predicted Salary" = c(mlrChosenYearSalary, rfChosenYearSalary),
      "2023 Predicted Salary Equivalent" = c(mlr2023Salary, rf2023Salary)
    )
  })
  
  # Put the reminder text underneath table
  output$better_model2 <- renderText({
    
    if(RMSE(mlrPreds(), testingData()$cap_percent)[1] < RMSE(rfPreds(), testingData()$cap_percent)[1]) {
      
      paste0("Please not that the multiple linear regression model is better for predicting since it had a lower test RMSE value.")
      
    } else if(RMSE(mlrPreds(), testingData()$cap_percent)[1] == RMSE(rfPreds(), testingData()$cap_percent)[1]) {
      
      paste0("Please note that both models are deemed equal for predicting purposes since they had the same test RMSE value.")
      
    } else {
      paste0("Please note that the random forest model is better for predicting since it had a lower test RMSE value.")
    }
  })
  
})