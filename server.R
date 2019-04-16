source('analyze-compas.R')
library(shiny)
library(ggplot2)
library(shinyalert)
library(rdrop2)
library(dplyr)
load('distances.RData')
load('numeric_df.RData')
load('predictor_df.RData')
predictor_df <- predictor_df %>% mutate('temp' = ifelse(c_charge_degree == 'F', 'Felony', 'Misdemeanor')) %>% select(-c_charge_degree, -months_in_jail_so_far)
colnames(predictor_df) = c("sex", "age","race","juv. felony count","juv. misdemeanor count","juv. other count","priors count","severity of charge")

pdf = data.frame()
n_pairs = 50
dir.create('sessions')

server <- function(input, output, session){
  output$prompt <- renderText("Please read the Instructions and complete Registration.")
  #load('user_number.RData')
  user_number = session$token
  pair_num <- reactiveVal(1)
  question_1 = ''
  question_2 = ''
  question_3 = ''
  shinyjs::disable("fair_button")
  shinyjs::disable("unfair_button")
  outputDir = paste0('Research/Current_Projects/SubjectiveFairness/shiny-app-output/user', user_number)
  
  # plot the first example
  observeEvent(input$start, {
    # create output directory
    drop_create(outputDir)
    shinyalert("Registration", "Enter your full name", type = "input", inputId = 'register_name')
    shinyalert("Registration", "Enter your college email", type="input", inputId = 'register_email')
    shinyalert("Registration Complete.", "Please allow some time for the first pair to load.", type= 'success', inputId= 'loading')
    # get the pairs
    pairs = sampler(distances, d_min=0, d_max=100, n_pairs)
    pdf <<- cbind(pairs, data.frame(fair=rep(NA, n_pairs)))
    next_pair = pdf[1, 1:2]
    # plot the first pair and display the page
    output$pairs_left <- renderText('50 out of 50 pairs left')
    output$prompt <- renderText('In your view, as a matter of fairness, should the following two individuals recieve the same recidivism
               prediction, or is it ok to give them different predictions?')
    output$vs <- renderText("vs.")
    output$fair_b <- renderText("Ok to treat differently, or no opinion")
    output$unfair_b <- renderText("Should be treated equally")
    output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
    output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
    # enable click through
    shinyjs::enable("fair_button")
    shinyjs::enable("unfair_button")
    shinyjs::disable("start")
    #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df))
  })
  
  
  
  
  # #observeEvent(n_pairs, {
  #   # plot the first pair
  #   pairs = sampler(distances, d_min=input$min_l1, d_max=input$max_l1,n_pairs)
  #   pdf <<- cbind(pairs, data.frame(fair=rep(NA, n_pairs)))
  #   next_pair = pdf[1, 1:2]
  #   output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
  #   output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
  #   #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df)) 
  # })
  # 
  
  observeEvent(input$fair_button, {
    # update pair data frame
   
    if(pair_num() >= n_pairs){
      pdf[pair_num(),'fair'] <<- 0
      filePath=paste0('sessions/user-',user_number,'-pairs.csv')
      write.csv(pdf, file=filePath)
      drop_upload(filePath, path = outputDir)
      shinyalert("Almost Done!", "Now please answer a few short questions.", type = "success")
      shinyalert("Question 1 of 3", "To the extent that you had or developed systematic or informal rules for deciding which pairs should be treated equally, briefly articulate them:",type = "input", inputId = 'question_1')
      shinyalert("Question 2 of 3", "Did you feel that the experimental protocol allowed you to express your subjective notion of fairness?", type = "input", inputId = 'question_2')
      shinyalert("Question 3 of 3", "Any other comments on the experimental protocol or app?", type = "input", inputId = 'question_3')
      shinyalert("Complete!", 'Thank you for your fairness rankings, please exit the app', type = "success")
      shinyjs::disable("fair_button")
      shinyjs::disable("unfair_button")
      }else{
      shinyjs::disable("fair_button")
      shinyjs::disable("unfair_button")
      pdf[pair_num(),'fair'] <<- 0
      newval <- pair_num() + 1
      pair_num(newval)
      print(pair_num())
      # get the next pair
      next_pair = pdf[newval,1:2]
      print(next_pair)
      # show the next pair 
      output$pairs_left <- renderText(paste0(n_pairs-pair_num() + 1, ' out of 50 pairs left'))
      output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
      output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
      #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df))
      if((pair_num() %% 5) == 1){
        filePath=paste0('sessions/user-',user_number,'-pairs.csv')
        write.csv(pdf, file=filePath)
        drop_upload(filePath, path = outputDir)
      }
      shinyjs::enable("fair_button")
      shinyjs::enable("unfair_button")
    }
    
  })
  
  
  observeEvent(input$unfair_button, {
    # update pair data frame
    if(pair_num() >= n_pairs){
      pdf[pair_num(),'fair'] <<- 1
      filePath=paste0('sessions/user-',user_number,'-pairs.csv')
      write.csv(pdf, file=filePath)
      drop_upload(filePath, path = outputDir)
      shinyalert("Almost Done!", "Now please answer a few short questions.", type = "success")
      shinyalert("Question 1 of 3", "To the extent that you ad or developed systematic or informal rules for deciding which pairs should be treated equally, briefly articulate them:",type = "input", inputId = 'question_1')
      shinyalert("Question 2 of 3", "Did you feel that the experimental protocl allowed you to express your subjective notion of fairness?", type = "input", inputId = 'question_2')
      shinyalert("Question 3 of 3", "Any other comments on the experimental protocol or app?", type = "input", inputId = 'question_3')
      shinyalert("Complete!", 'Thank you for your fairness rankings, please exit the app', type = "success")
      shinyjs::disable("fair_button")
      shinyjs::disable("unfair_button")
      
    }else{
      shinyjs::disable("unfair_button")
      shinyjs::disable("fair_button")
      pdf[pair_num(),'fair'] <<- 1
      newval <- pair_num() + 1
      pair_num(newval)
      # get the next pair
      next_pair = pdf[newval,1:2]
      print(next_pair)
      print(pdf)
      # show the next pair 
      output$pairs_left <- renderText(paste0(n_pairs-pair_num() + 1, ' out of 50 pairs left'))
      output$person_1 <- renderTable(predictor_df[next_pair[1,1],])
      output$person_2 <- renderTable(predictor_df[next_pair[1,2],])
      #output$compare_pairs_plot <- renderPlot(plot_pair(next_pair, predictor_df))
      if((pair_num() %% 5) == 1){
        filePath=paste0('sessions/user-',user_number,'-pairs.csv')
        write.csv(pdf, file=filePath)
        drop_upload(filePath, path = outputDir)
      }
      shinyjs::enable("unfair_button")
      shinyjs::enable("fair_button")
    }
  })
  
  
  # save email
  observeEvent(input$register_email, {
    emails = input$register_email
    filePath=paste0('sessions/', 'user-', user_number,'-email.csv')
    write.csv(data.frame(email=emails), file=filePath)
    drop_upload(filePath, path = outputDir)
  })
  
  
  # save name
  observeEvent(input$register_name, {
    names = input$register_name
    filePath=paste0('sessions/', 'user-', user_number,'-name.csv')
    write.csv(data.frame(name=names), file=filePath)
    drop_upload(filePath, path = outputDir)
  })
  
  
  observeEvent(input$question_1, {
    question_1 <<- paste0('user ', user_number, ':', input$question_1)
  })
  
  observeEvent(input$question_2, {
    question_2 <<- paste0('user ', user_number, ':', input$question_2)
  })
  
  observeEvent(input$question_3, {
    question_3 <<- paste0('user ', user_number, ':', input$question_3)
    filePath=paste0('sessions/user-', user_number,'-questions.csv')
    write.csv(data.frame(q1=question_1, q2 = question_2, q3 = question_3), file=filePath)
    drop_upload(filePath, path = outputDir)
  })
  
  #server
  #observe({toggle(id="fair_button", condition=!is.null(input$start))})
  
}