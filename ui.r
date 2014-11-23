library(markdown)
shinyUI(
  navbarPage("Navbar!",
  tabPanel("MPG",
  pageWithSidebar(
  headerPanel("Predicting Miles Per Gallon"),
  sidebarPanel(
    checkboxGroupInput('id1', 'Variables in Model', 
                       c('Number of cylinders' = 'cyl',
                         'Displacement (cu.in.)' = 'disp',
                         'Gross horsepower' = 'hp',
                         'Rear axle ratio' = 'drat',
                        'Weight (lb/1000)'  = 'wt', 
                         '1/4 mile time' = 'qsec', 
                        'V/S'  = 'vs', 
                         'Number of forward gears' = 'gear', 
                        'Number of carburetors'  = 'carb')),
    submitButton("Submit")
  ),
  mainPanel(
    h4("Summary of your Model:"),
    verbatimTextOutput("oid1"),
    h4("Variables Included in Model:"),
    verbatimTextOutput("oid3"),
    h4("Percent of Variance Accounted for:"),
    plotOutput('newbar'),
    h4("Variance Inflation Factor:"),
    verbatimTextOutput("oid2"),
    h4("Residual Plots:"),
    plotOutput("newfrp"),
    h4("\n")
  ) #mainpanel
  
  ) #pg with sidebar

             ), # tab mpg
  
  tabPanel("UNEMP",
           pageWithSidebar(
             headerPanel("Predicting Unemployment"),
             sidebarPanel(
               checkboxGroupInput('id11', 'Variables in Model', 
                                  c('GNP implicit price deflator' = 'GNP.deflator',
                                    'Gross National Product' = 'GNP',
                                    'Number of people in the Armed Forces' = 'Armed.Forces',
                                    'Year'  = 'Year', 
                                    'Number of people employed' = 'Employed')), 
               submitButton("Submit")
             ),
             mainPanel(
               h4("Summary of your Model:"),
               verbatimTextOutput("oid11"),
               h4("Variables Included in Model:"),
               verbatimTextOutput("oid13"),
               h4("Percent of Variance Accounted for:"),
               plotOutput('newbar1'),
               h4("Variance Inflation Factor:"),
               verbatimTextOutput("oid12"),
               h4("Residual Plots:"),
               plotOutput("newfrp1"),
               h4("\n")
             ) #mainpanel
             
           ) #pg with sidebar
           
  ), # tab longley  
  
  tabPanel("FRTLE",
           pageWithSidebar(
             headerPanel("Predicting Fertility in 1888 Switzerland"),
             sidebarPanel(
               checkboxGroupInput('id21', 'Variables in Model', 
                                  c('Percent of males in Agriculture' = 'Agriculture',
                                    'Percent of drafties with highest army exam scores' = 'Examination',
                                    'Percent of draftees educated beyond primary school' = 'Education',
                                    'Percent of populace that is catholic' = 'Catholic')),
               submitButton("Submit")
             ),
             mainPanel(
               h4("Summary of your Model:"),
               verbatimTextOutput("oid21"),
               h4("Variables Included in Model:"),
               verbatimTextOutput("oid23"),
               h4("Percent of Variance Accounted for:"),
               plotOutput('newbar2'),
               h4("Variance Inflation Factor:"),
               verbatimTextOutput("oid22"),
               h4("Residual Plots:"),
               plotOutput("newfrp2"),
               h4("\n")
             ) #mainpanel
             
           ) #pg with sidebar
           
  ), # tab swiss
  
  
  
  tabPanel("PRICE",
           pageWithSidebar(
             headerPanel("Predicting Price of Diamonds"),
             sidebarPanel(
               checkboxGroupInput('id31', 'Variables in Model', 
                                  c('Weight of Diamond (carats)' = 'carat',
                                    'Quality of Color' = 'color',
                                    'Level of Clarity' = 'clarity',
                                    'Width of Diamond' = 'x',
                                    'Breadth of Diamond'  = 'y', 
                                    'Hieght of Diamond' = 'z', 
                                    'Total Diamond Depth'  = 'depth', 
                                    'Diamond Table' = 'table')), 
               submitButton("Submit")
             ),
             mainPanel(
               h4("Summary of your Model:"),
               verbatimTextOutput("oid31"),
               h4("Variables Included in Model:"),
               verbatimTextOutput("oid33"),
               h4("Percent of Variance Accounted for:"),
               plotOutput('newbar3'),
               h4("Variance Inflation Factor:"),
               verbatimTextOutput("oid32"),
               h4("Residual Plots:"),
               plotOutput("newfrp3"),
               h4("\n")
             ) #mainpanel
             
           ) #pg with sidebar
           
  ), # tab diamonds
  
  
  tabPanel("YIELD",
           pageWithSidebar(
             headerPanel("Predicting Yield of Barley"),
             sidebarPanel(
               checkboxGroupInput('id41', 'Variables in Model', 
                                  c('Type of Barley' = 'variety',
                                    'Site of Barley Production' = 'site')),
               submitButton("Submit")
             ),
             mainPanel(
               h4("Summary of your Model:"),
               verbatimTextOutput("oid41"),
               h4("Variables Included in Model:"),
               verbatimTextOutput("oid43"),
               h4("Percent of Variance Accounted for:"),
               plotOutput('newbar4'),
               h4("Variance Inflation Factor:"),
               verbatimTextOutput("oid42"),
               h4("Residual Plots:"),
               plotOutput("newfrp4"),
               h4("\n")
             ) #mainpanel
             
           ) #pg with sidebar
           
  ), # tab barley
  
  
  
  tabPanel("About",
           pageWithSidebar(
             headerPanel("Documentation"),
             sidebarPanel(
              # checkboxGroupInput('id41', 'Variables in Model', 
               #                   c('Type of Barley' = 'variety',
                #                    'Site of Barley Production' = 'site')),
               #submitButton("Submit")
             ),
             mainPanel(
               h4("Introduction"),
               p("     The purpose of this app is to illustrate some of the necessary aspects of 
                      chosing variables of a data set to include in a regression model, and some 
                      of the key ways to evaluate the given model. "),           
     
               p("    The data sets in question include *swiss*, *longley* and a few other data 
                      sets that come pre-packaged in R, the lattice library, or the ggplot2 library.
                      Here is information on the data sets taken from the R cran website:  "),

               p("       mtcars: 'The data was extracted from the 1974 Motor Trend US magazine, and 
                                comprises fuel consumption and 10 aspects of automobile design and 
                                performancefor 32 automobiles (1973–74 models).' "),
               p("       longley: 'A macroeconomic data set....' "),
               p("       swiss: 'Standardized fertility measure and socio-economic indicators for 
                                each of 47 French-speaking provinces of Switzerland at about 1888.'  "),
               p("       diamonds: 'A dataset containing the prices and other attributes of almost 
                                54,000 diamonds.'  "),
               p("        barley: 'Total yield in bushels per acre for 10 varieties at 6 sites in each 
                                of two years.'  "),
     
               p("       The app sets up a regression model that attempts to explain a specific variable as a function
                          of the other variables in the data set. "),    
     
               p("       The initial regression model in each case is bivariate - it regresses a 
                      specific response variable against a specific predictor variable and displays
                      a variety of information about the model."),
               h4("How to Use this App"),
               p("Using the checkbox, simply select other variables in the data set to include in 
                 the model as predictor variables. "),
               h6("Note: The VIF will display an error before a second predictor variable is selected."),
               h6("Once at least two predictor variables are selected, the VIF will be displayed without error."),
               h4("What am I Looking at?"),
               p("The app outputs a number of things. First of all, a summary of the regression model apears 
                 to the left, and a basic overview of some diagnostics are displayed (residual 
                 statistics, r-squared, etc) along with the model estimations and uncertainty 
                 (estimated coefficients, t statistics, p-values, etc).  "),
               p("Also, the variables selected as predictor values are printed back to the user 
                 (including any defaults)."),
               p("The percent of variation of the response variable accounted for by the model 
                 (r-squared value) is represented graphically."),
               p("The Variance Inflation Factor for the predictor variables is displayed 
                 (from Wikipedia: 'The square root of the variance inflation factor tells 
                 you how much larger the standard error is, compared with what it would be if 
                 that variable were uncorrelated with the other predictor variables in the 
                 model.')."),
               p("The Residual Plots allow for visual inspection of the residuals vs. fitted values, 
                 normal distribution quantiles, and also graphically illustrate the leverage and 
                 influence of some of the data points in the model. Here, troubling things like 
                 violation of the normality and homoscedasticity requirements can be 
                 discovered. "),
               h4("Right. What was the Point of this again?"),
               p("The aim of this app is to provide an exercise in making the optimum selection of 
                 variables such that your model **explains a large amount of the variation the 
                  response variable**, but does not suffer from things like multicolinearity, 
                  heteroscedasticity, etc. For instance, VIF values larger than 10 indicate 
                  multicolinearity, which may result in unstable parameter estimates and larger 
                  standard errors. Large standard errors prevent precise estimates of regression 
                  coefficients, and thus, loss of statistical significance. "),
               p("Even when variables aren't colinear, variables that do a bad job of accounting for
                 variance also tend to reduce the statistical power of estimating the coeffecient 
                 of other variables (so you lose significance) by obscuring the relative amount of 
                 variance that those other variables explain. Therefore, it is desirable to have 
                 the least amount of variables that explain the most amount of variance."),
               p("Also, pay attention to the residual plots - they change as different variables 
                 are added to the model. The residual statistics in the model summary never tell 
                 the full story, and therefore, looking at the plots to understand what is going 
                 on with the residuals could alert you to how well (or not!) your model fits the 
                 data."),
               h5("Note: An F-Test is commonly used to determine whether a model with more parameters gives a significantly better fit to the data
                  when compared to a model with fewer parameters. No F-test is included in this app but it is an oft
                  used model evaluation/comparison tool."),
               h6("The 'diamonds' data set tab may take quite a bit of time to load on occasion."),
               h1(".  .  ."),
               p(" "),
               p("\n")
             ) #mainpanel
             
           ) #pg with sidebar
           
  ) # tab documentation
  
  
  
  ) #navbar

)