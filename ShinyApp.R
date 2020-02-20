

#tidytext: reorder_within()
#wordcloud: wordcloud
#ggplot2: ggplot 

ui <- dashboardPage(
  dashboardHeader(
    title = "Social Media Analysis",
    dropdownMenu
    (type = "messages",
      messageItem(from = "Markdown Report",
                  message = "You can find a report on our analysis clicking in this button.", #print the page
                  icon = icon("life-ring"),
                  time = "2020-02-06",
                  href="https://drive.google.com/open?id=1PhGZFqiC9xeDKn2N-_2k-KXkf1etIuyJ"
      ),
      messageItem(from = "Documentation",
                  message = "You can find our documentation for this project clicking in this button.",
                  icon = icon("life-ring"),
                  time = "2020-02-06",
                  href="https://drive.google.com/open?id=1mwj5W0qeUMk10nR-Su7fUlSjkKy7CYrr"
      )
    )
  ),
  dashboardSidebar(
    
    #We create the different sections of our App
    sidebarMenu(
      menuItem("Text Analysis", tabName = "text", icon=icon("dashboard")),
      menuItem("Table of tweets",tabName='tweets',icon=icon("th"))
    )
  ),
  
  #We create the interface in which we are going to put the elements created in the server
  dashboardBody(
    #We need to change the information that is shared in each tabItem
    tabItems(
      tabItem(
        tabName = "text",
        h2("Text Analysis"),
        h5("The purpose of this section is to provide an insight into the sentiments, emotions and frequency of structures of words.
           We found mainly that the comments that mention Citibank are slighly more positive and that the most predominant emotions 
           on the tweets are fear and trust."),
        h5(),
        h5(),
        tabsetPanel(
          tabPanel("Word Cloud",
                   "We find that the most repetitive words are all related to the bank's services as expected and we don't
                   find any negative words. However, it is important to notice the presence of some words 'please', 'now' and
                   help, which might suggest a client trying to reach the customer service.",
                   
                   fluidRow(
                     column(8,
                            
                            # Copy the line below to make a slider bar 
                            sliderInput("slider1", label = h3("Slider"), min = 0, 
                                        max = 50, value = 30)
                     ),
                     
                     plotOutput("sentiment_wordplot"),
                     plotOutput("sentiment_wordplot2")
                     
                     )
                  
                   ),
          tabPanel("Topic Analysis", 
                   "We identify that the three most predominant topics are the following way: 1) Credit Card related, 
                   2) Customers account related, and 3) Citibanks image related.",
                   fluidRow(
                     plotOutput("sentiment_topics")
                   )
                   ),
          tabPanel("Emotion Analysis", 
                   "We found that that the most predominant emotions on the tweets where Citibank is mentioned 
                     are fear and trust and that the less predominant emotions are surprise and anticipation.",
                   fluidRow(
                     plotOutput("sentiment_emotions")
                   )
                   ),
          tabPanel("Negative & Positive Analysis",
                   "We found that the most relevant words related to a positive sentiment are 'like', 'thank' 
                   and 'good'; on the other hand, we found the most relevant ones associated to a negative 
                   sentiment are 'worst', 'scam' and 'issue'.",
                   fluidRow(
                     plotOutput("sentiment_negativepositive")
                   )
                   ),
          tabPanel("Negative & Positive Analysis",
                   "In general, we had discovered that there is a slightly positive amount of tweets, but adding
                   the dimension of time, we could say that the tweets are mainly neutral without a clear 
                   sentiment trend.",
                   fluidRow(
                     plotOutput("sentiment_negativepositivetime")
                   )
                   )
        )
      ),
      tabItem(
        tabName="tweets",
        h2("Table of positive and negative tweets"),
        h5("The purpose of this table is to identify the most negative tweets in order to take inmediate action
           using the URL to answer to these negative tweets. Mixing this tool with real time date, could be a 
           great tool for social media managers and customer service representants to control their interaction with 
           clients and control the reputation of the brand by giving a quick answer."),
        fluidRow(
          dataTableOutput('table')
        )
      )
    )
  )
)

server <- function(input, output,session) {
  setwd("D:/User/Google Drive/Proyectos/Estudios/Maestría/Courses/13. Social Media Analytics/Group assignment/Shared Folder - Social Media/Code/ShinyApp/Data")
  
  for (i in c('DT','shiny','shinydashboard','wordcloud',"ggplot2","tidytext")){
    if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
    require(i, character.only=TRUE)
  }
  set.seed(122)
  histdata <- rnorm(500)
  
  #We create the first output
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(100)]
    hist(data)
  })
  
  #We create the second output
  
  output$table <- renderDataTable({
    load("CommentstoPrint2.Rda")
    datatable(CommentstoPrint2[,drop=FALSE])
    #require(data.table)
    #setDT(Comments)
    #DT::datatable(Comments, options = list(orderClasses = TRUE))
    #citibank_search<-readRDS("Data/citibank_search.rds")
    
    #citibank_search[,c("screen_name","created_at","text","source","lang")]
  })
  
  output$sentiment_wordplot<-renderPlot({
    load("Freq.Rda")
    wordcloud(Freq$word, Freq$freq,
              max.words=input$slider1,
              scale=c(4,1))
  })
  
  output$sentiment_wordplot2<-renderPlot({
    load("BiCount.Rda")
    wordcloud(BiCount$bigram,BiCount$freq,max.words = input$slider1)
  })
  
  output$sentiment_topics<-renderPlot({
    load("top_tweet_terms.Rda")
    top_tweet_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered()
  })
  
  output$sentiment_emotions<-renderPlot({
    load("Sentimentscores.Rda")
    ggplot(data=Sentimentscores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
      theme(legend.position="none")+
      xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets on Citi")
  })
  
  output$sentiment_negativepositive<-renderPlot({
    load("summarySentiment.Rda")
    summarySentiment %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  output$sentiment_negativepositivetime<-renderPlot({
    load("time.Rda")
    load("sentiment.Rda")
    load("lim.Rda")
    plot(1:length(sentiment), 
         sentiment, 
         xaxt="n",
         type="l",
         ylab="Valence",
         xlab="Time",
         main="Sentiment", 
         ylim=c(-lim,lim))
    abline(h = 0, col = "red", lty = 3)
    
    axis(1,at=1:length(sentiment), 
         labels=paste0(substr(time[order(time)],1,10)))
    #,"\n",substr(time[order(time)],12,16)
    
  })
  
  
  #This help us end the program each time someone closes the window
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)

