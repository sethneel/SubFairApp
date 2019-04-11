source('analyze-compas.R')
library(shiny)
library(ggplot2)
library(shinyalert)
library(rdrop2)
library(dplyr)
load('distances.RData')
load('numeric_df.RData')
load('predictor_df.RData')
predictor_df <- predictor_df %>% select(-months_in_jail_so_far)
user_number = 0
pdf = data.frame()
emails = c()
dir.create('sessions')
outputDir = 'Research/Current_Projects/SubjectiveFairness/shiny-app-output'
drop_create(outputDir)
# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyalert(),
  # Application title
  headerPanel(
    h1("Fairness in Recidivism Survey",style = "font-family: 'Lobster', cursive;
            font-weight: 500; line-height: 1.1; 
       color: #000000;")),
  
  mainPanel(h3('In your opinion would it be unfair if either of these two inmates was released and the other was not?', 
            style = "font-family: 'Lobster';
            font-weight: 250; line-height: 1.1; 
            color: #4d3a7d;")),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("n_pairs", "# of pairs", 30, 1, 1000, width='75px'),
      numericInput("min_l1", "min l1 dist.", 0, 0, 67, width='75px'),
      # unrestricted sampling
      numericInput("max_l1", "max l1 dist.", 100, 1, 67, width='75px')
      #actionButton("start", "Start Evaluation", icon("paper-plane"), 
      #            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      #h5("Race Key: (1: African-American, 2: Asian, 3: Caucasian, 4: Hispanic, 5: Native American)"),
      #h5("Sex Key: (1: F, 2: M)"),
      #h5("Charge Degree Key: (1: Felony, 2: Misdemeanor)")
    ),
    
    mainPanel(
      tableOutput('person_1'),
      h5("VS."),
      tableOutput("person_2"),
      #plotOutput("compare_pairs_plot"),
      actionButton("fair_button","Yes"),
      actionButton("unfair_button", "No")
    ), position = 'right'
  ), 
  hr(),
  h5("(C) 2019 Seth Neel (sethneel@wharton.upenn.edu)"),
  h5("Data Source: https://github.com/propublica/compas-analysis"),
  h5("The AlgoWatch Project, Unversity of Pennsylvania")
    )


# Define server logic required to draw a histogram
server <- function(input, output){
  user_number <<- user_number + 1
  pair_num <- reactiveVal(1)
  
  # plot the first example
  # observeEvent(input$start, {
  #   # plot the first pair
  #   pairs = sampler(distances, d_min=input$min_l1, d_max=input$max_l1, input$n_pairs)
  #   pdf <<- cbind(pairs, data.frame(fair=rep(NA, input$n_pairs)))
  #   next_pair = pdf[1, 1:2]
  #   output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
  #   output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
  #   output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df)) 
  # })
  
  
  observeEvent(input$n_pairs, {
    # plot the first pair
    pairs = sampler(distances, d_min=input$min_l1, d_max=input$max_l1,input$n_pairs)
    pdf <<- cbind(pairs, data.frame(fair=rep(NA, input$n_pairs)))
    next_pair = pdf[1, 1:2]
    output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
    output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
    #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df)) 
  })
  
  
  observeEvent(input$fair_button, {
    # update pair data frame
    if(pair_num() >= input$n_pairs){
      pdf[pair_num(),'fair'] <<- 1
      filePath=paste0('sessions/user-',user_number,'-pairs.csv')
      write.csv(pdf, file=filePath)
      drop_upload(filePath, path = outputDir)
      shinyalert("Complete!", "Thank you for your fairness rankings! Enter your email for course credit.", type = "input")
    }else{
      pdf[pair_num(),'fair'] <<- 1
      newval <- pair_num() + 1
      pair_num(newval)
      print(pair_num())
      # get the next pair
      next_pair = pdf[newval,1:2]
      print(next_pair)
      # show the next pair 
      output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
      output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
      #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df))
      if((pair_num() %% 5) == 1){
        filePath=paste0('sessions/user-',user_number,'-pairs.csv')
        write.csv(pdf, file=filePath)
        drop_upload(filePath, path = outputDir)
      }
    }
  })
  
  # save email
  observeEvent(input$shinyalert, {
  emails <<- c(emails, input$shinyalert)
  filePath=paste0('sessions/emails.csv')
  write.csv(data.frame(email=emails), file=filePath)
  drop_upload(filePath, path = outputDir)
  })
  
  observeEvent(input$unfair_button, {
    # update pair data frame
    if(pair_num() >= input$n_pairs){
      pdf[pair_num(),'fair'] <<- 0
      filePath=paste0('sessions/user-',user_number,'-pairs.csv')
      write.csv(pdf, file=filePath)
      drop_upload(filePath, path = outputDir)
      shinyalert("Complete!", "Thank you for your fairness rankings!", type = "success")
    }else{
      pdf[pair_num(),'fair'] <<- 0
      newval <- pair_num() + 1
      pair_num(newval)
      # get the next pair
      next_pair = pdf[newval,1:2]
      print(next_pair)
      print(pdf)
      # show the next pair 
      output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
      output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
      #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df))
      if((pair_num() %% 5) == 1){
        filePath=paste0('sessions/user-',user_number,'-pairs.csv')
        write.csv(pdf, file=filePath)
        drop_upload(filePath, path = outputDir)
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

