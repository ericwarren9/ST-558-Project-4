# Application Features for Repository

This application created examines how much (or what percentage of) a NFL quarterback's salary is represented by the NFL's salary cap maximum. Our goal is to use a quarterback's statistics to try to predict what their salary should be in a given year and how it would equate to today's terms. Data is from the NFL seasons of 2007 to 2022 (excluding 2010 since it was a lockout year with salaries and player statistics are not representative of a normal year) to answer this question. Please note that all data for quarterbacks is filtered ahead of time for players who are deemed 'starters' which means they played at least ten games and had at least 100 passing attempts. 2007 was a weird year with many injuries so not as much 'valid' data is present for us to analyze.

This application uses two modeling methods -- random forest and multiple linear regression -- to try to predict what a quarterback should be getting paid based on their performance. Users will be able to do their own exploratory data analysis to get a better idea of trends are present and even draw their own inclusions by the user friendly interface that can manipulate results at a touch of a button. This will help the user in the modeling section where they try to create their own optimal random forest and multiple linear regression models to try to get the predicted salary of a quarterback where they input their game statistics. Feel free to input what your favorite quarterback did one season or include your own *dream* statistics that you put up in Madden. Nothing is limiting you from having whatever experience you want.

In order to run this application on your local server there are some things you need to do. First make sure you have (or if you do not install) the following packages below:

- shiny: Used the create the application
- shinyWidgets: Helps make the applicaton more user friendly and look a little more clean
- shinytitle: Makes titles look better and helps with formatting of titles and labels
- bslib: helps Make some of the picker options more user friendly
- data.table: Helps make our tables look neater in our shiny application
- tidyverse: Helps with data manipulations
- caret: Used for our modeling to produce our multiple linear regression and random forest models
- randomForest: Helps let us use tuning optons for our random forest model
- rvest: Used to download and scrape data from the web
- janitor: Used to clean the column names that are messy and not usable (scrapping data from the web can give you messy column names)

If you would like to install all of these packages at once in case you are not sure that you have them you may use the following line of code in your R application: `install.packages(c("shiny", "shinyWidgets", "shinytitle", "bslib", "data.table", "tidyverse", "caret", "randomForest", "rvest", "janitor"))`

Also if you would like to quickly run the application you can also run the following code in R: `shiny::runGitHub("ST-558-Project-4", "ericwarren9", subdir = "project")` or you can access the application directly on an online server (so not locally) [here](https://3foak4-eric-warren.shinyapps.io/Predicting_NFL_QBs_Salaries_by_Performance/).

I hope you enjoy the application and feel free to send me any comments or questions to my [email](mailto:ericwarren09@yahoo.com) or by connecting with me on [LinkedIn](https://www.linkedin.com/in/eric-warren-960037203/).
