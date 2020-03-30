library(shiny)
library(datasets)
library(ggplot2)
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
                              numericInput("max_score", "Max Score:", 100, min = 1, max = 300, width = '100px'),
                              numericInput("cut_score", "Cut Score:", 0, min = 1, max = 100, width = '100px'),
                              numericInput("standard_error", "+1 Standard Error:", NA, min = 0, max = 25, width = '100px'),
                         
                     ),
                     mainPanel(
                         htmlOutput ("pass_fail"),
                         tags$br(),
                         htmlOutput ("SE"),
                         tags$br(),
                         plotOutput('MyPlot')
                         
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
          
          status <- factor(ifelse(Scores[1]>input$cut_score, "Pass", "Fail"))
      
                 p <- ggplot(Scores, aes_string(x=Scores[,1], fill = status)) +
                 geom_histogram(binwidth=1, colour="dark gray", position = "identity") +
                 ylab("Number of Candidates") +
                 xlab("Score") +
                 guides(fill=guide_legend(title="Status")) +
                 scale_fill_manual(values = c("Pass" = "#32CD32", "Fail" = "red")) +
                 xlim(0, input$max_score) +
                 scale_y_continuous(expand = expand_scale(mult=c(0, 0.5)), breaks = int_breaks) +
                 geom_vline(xintercept = input$cut_score, linetype = "dashed", color = "red", size = 1) +
                 theme_bw(base_size = 25) 
            
            print(p)

    })
    
    output$pass_fail <- renderText({
        paste0("<h4>",
               "When the cut score is set to ", 
               "<strong>", input$cut_score, "</strong>",", ", 
               (sum(data()[2]>input$cut_score)/nrow(data()))*100,
               "% of candidates will pass the exam.",
               "</h4>")
    })
    
    output$SE <- renderText({
      if(!is.na(input$standard_error)){
      paste0("<h4>",
             "When the cut score is increased by one standard error (", 
             "<strong>", input$cut_score+input$standard_error, "</strong>","), ", 
             (sum(data()[2]>(input$cut_score+input$standard_error))/nrow(data()))*100,
             "% of candidates will pass the exam.","<br><br>",
             "When the cut score is decreased by one standard error (", 
             "<strong>", input$cut_score-input$standard_error, "</strong>","), ", 
             (sum(data()[2]>(input$cut_score-input$standard_error))/nrow(data()))*100,
             "% of candidates will pass the exam.","<br>",
             "</h4>")
      } else {
        paste("")
      }
    })
})

shinyApp(ui, server)
