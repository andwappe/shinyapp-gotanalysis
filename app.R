# ===============================================
# Title: Game Of Thrones Text Analysis
# Description: We can see the sentiment analysis for certain characters in the 
# show across episodes or seasons. We can also input a word and analysis 2 gives
# a word trend analysis across episodes or seasons. 
# Author: Andrew Wapperom
# Date: 1/4/2024
# ===============================================


# ===============================================
# R packages
# ===============================================
library(shiny)
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets


# =======================================================
# Sentiment Lexicons
# =======================================================
bing = read_csv("bing.csv", col_types = "cc")
afinn = read_csv("afinn.csv", col_types = "cc")
nrc = read_csv("nrc.csv", col_types = "cc")
loughran = read_csv("loughran.csv", col_types = "cc")


# ===============================================
# Import data
# ===============================================
 dat = read_csv(
   file = "Game_of_Thrones_Script.csv", 
   col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
   skip = 1,
   col_types = cols(
     Date = col_character(),
     Season = col_character(),
     Episode = col_character(),
     Title = col_character(),
     Name = col_character(),
     Sentence = col_character()
   ))

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Text Analysis of Game of Thrones"),
  hr(),
  
  # -------------------------------------------------------
  # Input widgets 
  # -------------------------------------------------------
  fluidRow(
    column(3,
           p(em("Season Number")),
           sliderInput(inputId = "seasonNumber", 
                        label = "Season Number", 
                        min = 1,
                        max = 8,
                        value = 1)
    ), # closes column 1
    
    column(3,
           p(em("Character Names")),
           selectInput(inputId = "charName", 
                        label = "Character Names", 
                        choices = c("tyrion lannister", "jon snow", "sansa stark", "arya stark"),
                        selected = "tyrion")
    ), # closes column 2
    
    column(3,
           p(em("Word Frequency")),
           textInput(inputId = "wordFreq", 
                        label = "Word Frequency", 
                        value = "love")
    ), # closes column 3
    
    column(3,
           p(em("Group By Episode")),
           checkboxInput(inputId = "groupByEpisode", 
                        label = "Group By Episode", 
                        value = TRUE)
    ) # closes column 4
    
  ), # closes fluidRow
  
  hr(), # horizontal rule
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  tabsetPanel(type = "tabs",
              # Panel for Analysis 1
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              # Panel for Analysis 2
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("plot2"),
                       hr(),
                       dataTableOutput('table2'))
  ) # closes tabsetPanel
  
) # closes ui



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive objects
  # ------------------------------------------------------------
  sentiment_analysis <- reactive({
    if (input$groupByEpisode) {
      seasonNumber = paste("Season", as.character(input$seasonNumber))
      data_table = dat |>
        filter(dat$Name == input$charName & dat$Season == seasonNumber) |>
        select(Episode, Sentence) |>
        unnest_tokens(word, Sentence) |>
        inner_join(afinn) |>
        group_by(Episode) |>
        summarize(sentiment_score = sum(as.integer(value))) |>
        mutate(Episode = as.integer(str_extract(Episode, "\\d+")))
      
    } else {
      data_table = dat |>
        filter(dat$Name == input$charName) |>
        select(Season, Sentence) |>
        unnest_tokens(word, Sentence) |>
        inner_join(afinn) |>
        group_by(Season) |>
        summarize(sentiment_score = sum(as.integer(value))) |>
        mutate(Season = as.integer(str_extract(Season, "\\d+")))
    }
    return(data_table)
      
  })
  
  word_trend_analysis <- reactive({
    if (input$groupByEpisode) {
      seasonNumber = paste("Season", as.character(input$seasonNumber))
      data_table = dat |>
        filter(dat$Season == seasonNumber) |>
        select(Episode, Sentence) |>
        unnest_tokens(word, Sentence) |>
        group_by(Episode, word) |>
        summarize(count = n()) |>
        mutate(freq =  count / sum(count)) |>
        filter(word == input$wordFreq) |>
        mutate(Episode = as.integer(str_extract(Episode, "\\d+")))
    } else {
      data_table = dat |>
        select(Season, Sentence) |>
        unnest_tokens(word, Sentence) |>
        group_by(Season, word) |>
        summarize(count = n()) |>
        mutate(freq =  count / sum(count)) |>
        filter(word == input$wordFreq) |>
        mutate(Season = as.integer(str_extract(Season, "\\d+")))
    }
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    
    if (input$groupByEpisode) {
      plottitle = paste("Episode vs Sentiment Score for", input$charName)
      sentiment_analysis() |>
      ggplot(aes(x = Episode, y = sentiment_score)) + 
        geom_bar(stat = "identity") +
        xlab("Episode Number") + 
        ylab("Sentiment Score (afinn)") + 
        ggtitle(plottitle)
    } else {
      plottitle = paste("Season vs Sentiment Score for", input$charName)
      sentiment_analysis() |>
      ggplot(aes(x = Season, y = sentiment_score)) + 
        geom_bar(stat = "identity") +
        xlab("Season Number") + 
        ylab("Sentiment Score (afinn)") + 
        ggtitle(plottitle)
    }
  })
  
  # code for table1
  output$table1 <- renderDataTable({
    sentiment_analysis() |>
      datatable()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
    if (input$groupByEpisode) {
      plottitle = paste("Episode vs Frequency for", input$wordFreq)
      word_trend_analysis() |>
        ggplot(aes(x = Episode, y = freq)) + 
        geom_bar(stat = "identity") +
        xlab("Episode Number") + 
        ylab("Frequency") + 
        ggtitle(plottitle)
    } else {
      plottitle = paste("Season vs Frequency for", input$wordFreq)
      word_trend_analysis() |>
        ggplot(aes(x = Season, y = freq)) + 
        geom_bar(stat = "identity") + 
        xlab("Season Number") + 
        ylab("Frequency") + 
        ggtitle(plottitle)
    }
  })
  
  # code for table2
  output$table2 <- renderDataTable({
    word_trend_analysis() |>
      datatable()
  })
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

