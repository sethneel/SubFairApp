library(shinyjs)
library(shinyalert)
ui <- fluidPage(
  useShinyalert(),
  # Application title
  headerPanel(
    h1("Fairness in Recidivism Survey",style = "font-family: 'Lobster';
       font-weight: 500; line-height: 1.1; 
       color: #000000;")),
  
  mainPanel(textOutput('prompt')),
  
  useShinyjs(), # include useShinyjs() somewhere in UI
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #numericInput("n_pairs", "# of pairs", 30, 1, 1000, width='75px'),
      #numericInput("min_l1", "min l1 dist.", 0, 0, 67, width='75px'),
      # unrestricted sampling
      #numericInput("max_l1", "max l1 dist.", 100, 1, 67, width='75px'),
      textOutput("pairs_left"),
      a("Instructions",target="_blank",href="instructions.pdf"),
      hr(),
      actionButton("start", "Register", icon("paper-plane")) 
      #            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #h5("Race Key: (1: African-American, 2: Asian, 3: Caucasian, 4: Hispanic, 5: Native American)"),
      #h5("Sex Key: (1: F, 2: M)"),
      #h5("Charge Degree Key: (1: Felony, 2: Misdemeanor)")
    ),
    
    mainPanel(
      tableOutput('person_1'),
      textOutput('vs'),
      tableOutput("person_2"),
      #plotOutput("compare_pairs_plot"),
      actionButton("unfair_button", textOutput("unfair_b")),
      actionButton("fair_button", textOutput("fair_b"))
    ), position = 'right'
  ), 
  hr(),
  h5("(C) 2019 Seth Neel (sethneel@wharton.upenn.edu)"),
  h5("Data Source: https://github.com/propublica/compas-analysis"),
  h5("The AlgoWatch Project, Unversity of Pennsylvania")
  )