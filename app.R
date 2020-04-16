###########################################################
## Script name: app.R                                    ##
##                                                       ##
## Purpose:     Run impact analysis on candidate scores  ##
##              Input csv file with scores               ##
##              Outputs histogram                        ##
##              Input cut score and SE                   ##
##              Outputs passing rate                     ##
##                                                       ##
## Author:      Natalie Jorion                           ##    
##                                                       ##  
## Date:        2020-03-30                               ##
###########################################################


##### Install required libraries ####

packs <- c("shiny", "datasets", "ggplot2", "gginnards")

# lapply(packs,function(x) {
#   if(x %in% rownames(installed.packages())==FALSE)
#   {install.packages(x)
#     sapply(x,library,character.only=TRUE)}
#   else{sapply(x,library,character.only=TRUE)}
# })

library(shiny)
library(datasets)
library(ggplot2)
library(gginnards)

#Function for ggplot histogram to specify integer bins
int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 

ui <- shinyUI(fluidPage(
    titlePanel("Impact Analysis"),
    tabsetPanel(
        tabPanel("Upload File",
                 titlePanel("Upload a CSV File"),
                 sidebarLayout(
                     sidebarPanel(
                         fileInput('file1', 'Must have two columns: Candidate and Score',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$br(),
                     ),
                     mainPanel(
                         tableOutput('contents')
                     )
                 )
        ),
        tabPanel("Score Distribution",
                 pageWithSidebar(
                     headerPanel(''),
                     sidebarPanel(
                              numericInput("max_score", "Max Score:", 100, min = 1, max = 300, width = '150px'),
                              numericInput("cut_score", "Cut Score:", 0, min = 1, max = 100, width = '150px'),
                              numericInput("standard_error", "Standard Error:", NA, min = 0, max = 25, width = '150px'),                        
                          numericInput("optional_cut", "Potential Cut Score:", NA, min = 0, max = NA, width = '150px'),
                              # selectInput("SE_num", "Number of Standard Errors:",
                              #             c("", "1 Standard Error" = 1,
                              #               "2 Standard Errors" = 2,
                              #               "3 Standard Errors" = 3), width = '150px'), 
                              width = 2
                         
                     ),
                     mainPanel(
                         # htmlOutput ("pass_fail"),
                         # tags$br(),
                         # htmlOutput ("SE"),
                         # tags$br(),
                         plotOutput('MyPlot'),
                         br(),
                         (HTML("<center>")),
                         h3(tableOutput('SE_output')),
                         br(),
                         h3(htmlOutput ("user_cut")),
                         (HTML("</center>"))
                         
                     )
                 )
        )
    )
)
)

server <- shinyServer(function(input, output, session) {
    
    data <- reactive({ 
        req(input$file1) 
        inFile <- input$file1 
        df <- read.csv(inFile$datapath, header = TRUE)
        updateSelectInput(session, inputId = 'ycol', label = 'Scores',
                          choices = names(df), selected = names(df)[2])
        return(df)
    })
    
    output$contents <- renderTable({
        data()
    })
    
    output$MyPlot <- renderPlot({
      
          Scores <- data()[2]
          
          SE_rect1 <- data.frame(xmin=(input$cut_score-0.5)-input$standard_error,
                                xmax=(input$cut_score-0.5)+input$standard_error, 
                                ymin=-Inf, ymax=Inf)
          
          SE_rect2 <- data.frame(xmin=(input$cut_score-0.5)-(2*input$standard_error),
                                 xmax=(input$cut_score-0.5)+(2*input$standard_error), 
                                 ymin=-Inf, ymax=Inf)
          
          SE_rect3 <- data.frame(xmin=(input$cut_score-0.5)-(3*input$standard_error),
                                 xmax=(input$cut_score-0.5)+(3*input$standard_error), 
                                 ymin=-Inf, ymax=Inf)
          
          status <- factor(ifelse(Scores[1]>(input$cut_score-0.01), "Pass", "Fail"))
      
                 p <- ggplot(Scores, aes_string(x=Scores[,1], fill = status)) +
                 geom_histogram(binwidth=1, colour="dark gray", position = "identity") +
                 ylab("Number of Candidates") +
                 xlab("Score") +
                 guides(fill=guide_legend(title="Status")) +
                 scale_fill_manual(values = c("Pass" = "#32CD32", "Fail" = "red")) +
                 scale_x_continuous(limits = c(0,input$max_score), expand = c(0, 0)) +
                 scale_y_continuous(expand = expand_scale(mult=c(0, 0.5)), breaks = int_breaks) +
                 geom_vline(xintercept = input$cut_score-0.5, linetype = "dashed", 
                            color = "red", size = 1) +
                 geom_rect(data=SE_rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, 
                                             ymax=ymax), fill="#2a4d69", alpha=0.3, inherit.aes = FALSE) +
                 geom_rect(data=SE_rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, 
                                               ymax=ymax), fill="#4b86b4", alpha=0.3, inherit.aes = FALSE) +
                geom_rect(data=SE_rect3, aes(xmin=xmin, xmax=xmax, ymin=ymin, 
                                                ymax=ymax), fill="#adcbe3", alpha=0.3, inherit.aes = FALSE) +
                 theme_bw(base_size = 25) 
                 
            print(move_layers(p, "GeomRect", position = "bottom"))

    })
    
    output$user_cut <- renderText({
      if(!is.na(input$cut_score) & !is.na(input$standard_error) & !is.na(input$optional_cut)){
        paste0("When the cut score is set to ",
               "<strong>", input$optional_cut, "</strong>",", ",
               round(((sum(data()[2]>(input$optional_cut-0.01))/nrow(data()))*100),2),
                   "% of candidates will pass the exam.")
        }
        else {
          paste("")
        }
    })
    
    # output$pass_fail <- renderText({
    #     paste0("<h4>",
    #            "When the cut score is set to ", 
    #            "<strong>", input$cut_score, "</strong>",", ", 
    #            (sum(data()[2]>(input$cut_score-0.01))/nrow(data()))*100,
    #            "% of candidates will pass the exam.",
    #            "</h4>")
    # })
    
    # output$SE <- renderText({
    #   
      # if(!is.na(input$standard_error)){
      #   if(!is.na(input$SE_num)){
      #     if(input$SE_num==1){
      #        paste0("<h4>",
      #        "When the cut score is increased by one standard error (", 
      #        "<strong>", input$cut_score+input$standard_error, "</strong>","), ", 
      #        (sum(data()[2]>((input$cut_score-0.01)+input$standard_error))/nrow(data()))*100,
      #        "% of candidates will pass the exam.","<br><br>",
      #        "When the cut score is decreased by one standard error (", 
      #        "<strong>", input$cut_score-input$standard_error, "</strong>","), ", 
      #        (sum(data()[2]>((input$cut_score-0.01)-input$standard_error))/nrow(data()))*100,
      #        "% of candidates will pass the exam.","<br>",
      #        "</h4>")
      #        }
      #     else if(input$SE_num==2){
      #       paste0("<h4>",
      #       "When the cut score is increased by two standard errors (", 
      #        "<strong>", input$cut_score+(input$standard_error*2), "</strong>","), ", 
      #        (sum(data()[2]>((input$cut_score-0.01)+(input$standard_error*2)))/nrow(data()))*100,
      #        "% of candidates will pass the exam.","<br><br>",
      #        "When the cut score is decreased by two standard errors (", 
      #        "<strong>", input$cut_score-(input$standard_error*2), "</strong>","), ", 
      #        (sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*2)))/nrow(data()))*100,
      #        "% of candidates will pass the exam.","<br>",
      #        "</h4>")
      #     }
      #     else if(input$SE_num==3){
      #       paste0("<h4>",
      #              "When the cut score is increased by three standard errors (", 
      #              "<strong>", input$cut_score+input$standard_error, "</strong>","), ", 
      #              (sum(data()[2]>((input$cut_score-0.01)+(input$standard_error*3)))/nrow(data()))*100,
      #              "% of candidates will pass the exam.","<br><br>",
      #              "When the cut score is decreased by three standard errors (", 
      #              "<strong>", input$cut_score-input$standard_error, "</strong>","), ", 
      #              (sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*3)))/nrow(data()))*100,
      #              "% of candidates will pass the exam.","<br>",
      #              "</h4>")
      #     }
      #   }
      # }
      # else {
      #   paste("")
      # }
    # })
    
    
    data_SE <- reactive({
      req(input$cut_score, input$standard_error, cancelOutput = TRUE)
      
       SE_rounded <- input$standard_error
      
      SE_table <- data.frame("Cut Score" = 
                                  round(c(input$cut_score+(SE_rounded*3),input$cut_score+(SE_rounded*2), 
                                          input$cut_score+(SE_rounded*1),input$cut_score,input$cut_score-(SE_rounded*1),
                                          input$cut_score-(SE_rounded*2),input$cut_score-(SE_rounded*3)),0),
                             "Percent Passing" = 
                                  round(c(sum(data()[2]>((input$cut_score-0.01)+(input$standard_error*3)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)+(input$standard_error*2)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)+(input$standard_error*1)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*0)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*1)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*2)))/nrow(data())*100,
                                   sum(data()[2]>((input$cut_score-0.01)-(input$standard_error*3)))/nrow(data())*100), 2),
                             check.names = FALSE)

      # Cut score should not have any decimals -- will have two by default
      SE_table[1,] <- as.character(SE_table[1,])

      row.names(SE_table) <- c("+3 Standard Error", "+2 Standard Error","+1 Standard Error",
                               "Panel-Recommended Raw Cut","-1 Standard Error", "-2 Standard Error", "-3 Standard Error")
      return(SE_table)
    })

    output$SE_output <- renderTable({
      data_SE()},
      rownames = TRUE,
      colnames = TRUE,
      # striped = TRUE,
      hover = TRUE,
      bordered = TRUE, 
    ) 
})

shinyApp(ui, server)
