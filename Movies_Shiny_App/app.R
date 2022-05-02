library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyalert)
library(ggridges)
library(scales)
library(RColorBrewer)
library(tidyr)

movies_data <- read_csv("data/movies.csv")
movies_data_jh <- read_csv("data/movies.csv")
genre_choices <- c('All Genres', sort(unique(movies_data$genre)))
rating_choices <- c('All Ratings', sort(unique(movies_data$rating)))
unique_years <- c(sort(unique(movies_data_jh$year)))
measures <- c('score', 'gross')
col_vals <- c(1,2,3,4,6,13)
genre_options <- c(sort(unique(movies_data_jh$genre)))

ui <- fluidPage(
    
    theme = shinytheme("cosmo"),
    
    fluidRow(
        
        tabsetPanel(
            
            id = "tab_being_displayed",
            
            tabPanel("IMBd User Scores",
                     
                     useShinyalert(),
                     
                     titlePanel(h1(p(strong("Comparing IMDb User Scores by Genre")), align = "center")),
                     
                     sidebarLayout(
                         
                         sidebarPanel(
                             
                             h3(p(strong("Side-by-Side Plot Selections"))),
                             
                             h5("Get a detailed comparison of User Scores for two film genres"),
                             
                             h5("Use the Inputs below to make a selection"),
                             
                             br(),
                             
                             h3("First Plot Inputs"),
                             
                             selectInput(inputId = "genre_input_1", label = "Choose First Genre",
                                         choices = genre_choices,
                                         selected = "All Genres"),
                             br(),
                             
                             h3("Second Plot Inputs"),
                             
                             selectInput(inputId = "genre_input_2", label = "Choose Second Genre",
                                         choices = genre_choices,
                                         selected = "All Genres"),
                             hr(),
                             
                             h3(p(strong("High-Level View Across Genres"))),
                             
                             h5("Get an overview of User Scores across film genre at a glance"),
                             
                             br(),
                             
                             img(src = "https://m.media-amazon.com/images/G/01/IMDb/BG_rectangle._CB1509060989_SY230_SX307_AL_.png",
                                 width="45%", height="45%", alt = "IMDb Logo",
                                 style="display: block; margin-left: auto; margin-right: auto;")
                         ),
                         
                         mainPanel(
                             fluidRow(
                                 splitLayout(cellWidths = c("50%", "50%"),
                                             plotOutput(outputId = "density_plot_1"),
                                             plotOutput(outputId = "density_plot_2")),
                                 fluidRow(plotOutput(outputId = "ridge_plot_1")))
                         )
                     )),
            
            tabPanel("Gross and Budget Across the Years",
                     
                     titlePanel(h1(p(strong("Gross and Budget Across the Years")), align = "center")),
                     
                     sidebarLayout(
                         
                         sidebarPanel(
                             
                             h3(p(strong("Genre and Year Selections"))),
                             
                             h5("See how much revenue movies grossed compared to their budgets"),
                             
                             h5("Use the Inputs below to make a selection"),
                             
                             br(),
                             
                             h3("Genre Inputs"),
                             
                             selectInput(inputId = "var", label = "Choose a Genre",
                                         choices = genre_choices,
                                         selected = "All Genres"),
                             br(),
                             
                             h3("Rating Inputs"),
                             
                             selectInput(inputId = "var2", label = "Choose a Rating",
                                         choices = rating_choices,
                                         selected = "All Ratings"),
                             br(),
                             
                             sliderInput(inputId = "alpha_level", label = "Nested Bar Transparency",
                                         min = 0, max = 1, value = 0.8, step = 0.2),
                             br(),
                             
                             img(src = "https://m.media-amazon.com/images/G/01/IMDb/BG_rectangle._CB1509060989_SY230_SX307_AL_.png",
                                 width="45%", height="45%", alt = "IMDb Logo",
                                 style="display: block; margin-left: auto; margin-right: auto;")
                             
                         ),
                         
                         mainPanel(
                             plotOutput(outputId = "bar"),
                             
                             br(),
                             
                             h6(p(strong("Bars are nested with Gross behind Budget measures")), align = "center"),
                             
                             h6(p(strong("for better comparisons between proportions (Bars are not stacked)")), align = "center")
                             
                         )
                     )),
            
            tabPanel("Top Movies by Measure",
                     
                     titlePanel(h1(p(strong("Top Movies by Gross or Score")), align = "center")),
                     
                     sidebarLayout(
                         
                         sidebarPanel(
                             
                             h3(p(strong("Genre and Year Selections"))),
                             
                             h5("See how much revenue movies grossed or how high they scored"),
                             
                             h5("Use the Inputs below to make a selection"),
                             
                             br(),
                             
                             h3("Genre Inputs"),
                             
                             selectInput(inputId = "genre", label = "Chose a Genre",
                                         choices = genre_options,
                                         selected = "Action"),
                             br(),
                             
                             h3("Year Inputs"),
                             
                             selectInput(inputId = "year", label = "Choose a Year",
                                         choices = unique_years,
                                         selected = "2009"),
                             br(),
                             
                             h3("Measure Inputs"),
                             
                             selectInput(inputId = "measure", label = "Choose a Measure",
                                         choices = c('score', 'gross'),
                                         selected = 'gross'),
                             br(),
                             
                             img(src = "https://m.media-amazon.com/images/G/01/IMDb/BG_rectangle._CB1509060989_SY230_SX307_AL_.png",
                                 width="45%", height="45%", alt = "IMDb Logo",
                                 style="display: block; margin-left: auto; margin-right: auto;")
                             
                         ),
                         
                         mainPanel(
                             plotOutput(outputId = "movies_bar"),
                             
                             checkboxInput(inputId = "show_table", label = "Show table",
                                           value = FALSE),
                             
                             DT::dataTableOutput(outputId = "movies_table")
                         )
                     )),
            
            tabPanel("About",
                     
                     verticalLayout(
                         
                         titlePanel(h1(p(strong("About the App and the Authors")), align = "center")),
                         
                         mainPanel(h4("This app was developed by Adam Kicklighter, Jay Patel, and Joseph Hines to satisfy 
                            requirements for the final project of the DSBA 5122 Visual Analytics course (Spring 2022) in the 
                            UNC Charlotte Data Science and Business Analytics Program."),
                                   br(),
                                   
                                   h4("Data was sourced from", HTML('<a href="https://www.kaggle.com/datasets/danielgrijalvas/movies">
                                                the Move Industry Dataset on Kaggle</a>'),", which itself was ultimately
                                                scraped from ",
                                      HTML('<a href="https://www.imdb.com/">IMDb</a>'),
                                      " via Python by the original analyst, Daniel Grijalva."),
                                   br(),
                                   
                                   h4(HTML('<a href="https://www.linkedin.com/in/adam-kicklighter/">Adam Kicklighter LinkedIn</a>')),
                                   
                                   h4(HTML('<a href="https://github.com/akicklig">Adam Kicklighter Github Projects</a>')),
                                   
                                   h4(HTML('<a href="https://www.linkedin.com/in/uncc-jay-patel/">Jay Patel LinkedIn</a>')),
                                   
                                   h4(HTML('<a href="https://github.com/jpat99">Jay Patel Github Projects</a>')),
                                   
                                   h4(HTML('<a href="https://www.linkedin.com/in/joseph-hines-4362a3231/">Joe Hines LinkedIn</a>')),
                                   
                                   h4(HTML('<a href="https://github.com/jhines117">Joe Hines Github Projects</a>')),
                                   
                                   br(),
                                   
                                   h4("Now, for a little about the Authors' inspiration on this project: their favorite films...")),
                         
                         br(),
                         
                         wellPanel(
                             
                             fluidRow(
                                 
                                 splitLayout(cellwidths = c("33%", "33%", "33%"), align = "center",
                                             
                                             fluidRow(
                                                 htmlOutput(outputId = "Adam_title"),
                                                 img(src = "https://upload.wikimedia.org/wikipedia/en/8/8b/No_Country_for_Old_Men_poster.jpg",
                                                     width="35%", height="35%", alt = "No Country for Old Men",
                                                     style="display: block; margin-left: auto; margin-right: auto;")),
                                             fluidRow(
                                                 htmlOutput(outputId = "Jay_title"),
                                                 img(src = "https://upload.wikimedia.org/wikipedia/en/b/bc/Interstellar_film_poster.jpg",
                                                     width="35%", height="35%", alt = "Interstellar",
                                                     style="display: block; margin-left: auto; margin-right: auto;")),
                                             fluidRow(
                                                 htmlOutput(outputId = "Joe_title"),
                                                 img(src = "https://upload.wikimedia.org/wikipedia/en/4/49/Dead_poets_society.jpg",
                                                     width="35%", height="35%", alt = "Dead Poets Society",
                                                     style="display: block; margin-left: auto; margin-right: auto;"))))
                         ))
            ))
    ))

server <- function(input, output) {
    
    #---------------------------------- DENSITY ----------------------------------#  
    
    observeEvent(req(input$genre_input_1 == c('History', 'Music', 'Sport')), {
        shinyalert("Insufficient Data for Density Plot", type = "error")
    })
    
    observeEvent(req(input$genre_input_2 == c('History', 'Music', 'Sport')), {
        shinyalert("Insufficient Data for Density Plot", type = "error")
    })
    
    output$density_plot_1 <- renderPlot({
        
        movies_data %>%
            filter(if(input$genre_input_1 != 'All Genres') (genre == input$genre_input_1) else TRUE) %>%
            ggplot(aes(score)) +
            geom_density(stat = "density",
                         color = "blue4",
                         lwd = 1,
                         fill = "dodgerblue1",
                         alpha = 0.8,
                         kernel = "gaussian") +
            labs(x = "IMDb User Scores (Out of 10)",
                 y = "Density of Scores",
                 title = paste0("Density of IMDb User Scores for ",input$genre_input_1," Films")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(face = "bold")) +
            theme(axis.title = element_text(face = "bold"))
    })
    
    output$density_plot_2 <- renderPlot({
        
        movies_data %>%
            filter(if(input$genre_input_2 != 'All Genres') (genre == input$genre_input_2) else TRUE) %>%
            ggplot(aes(score)) +
            geom_density(stat = "density",
                         color = "darkorange4",
                         lwd = 1,
                         fill = "orange1",
                         alpha = 0.8,
                         kernel = "gaussian") +
            labs(x = "IMDb User Scores (Out of 10)",
                 y = "Density of Scores",
                 title = paste0("Density of IMDb User Scores for ",input$genre_input_2," Films")) +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(face = "bold")) +
            theme(axis.title = element_text(face = "bold"))
    })
    
    output$ridge_plot_1 <- renderPlot({
        
        movies_data %>%
            ggplot(aes(score, genre, fill = genre)) +
            geom_density_ridges(scale = 4, size = 1, alpha = 0.7) +
            scale_y_discrete(limits = rev) +
            scale_fill_cyclical(
                name = "Color scheme",
                values = c("dodgerblue1", "orange1")
            ) +
            labs(x = "IMDb User Scores (Out of 10)",
                 y = "Film Genre",
                 title = "Density of IMDb User Scores Compared Across All Genres") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(face = "bold")) +
            theme(axis.title = element_text(face = "bold"))
    })
    
    #---------------------------------- NESTED BARS ----------------------------------# 
    
    output$bar <- renderPlot({
        movies_data %>%
            filter(if(input$var != 'All Genres' ) (genre == input$var) else TRUE) %>%
            filter(if(input$var2 != 'All Ratings' ) (rating == input$var2) else TRUE) %>%
            ggplot(aes(x=year)) + 
            geom_bar(aes(y = gross, fill="Budget"), alpha = 0.8, size = 1.1, stat='identity') +
            geom_bar(aes(y = budget, fill = "Gross"), alpha = input$alpha_level, size = 1.1, stat='identity') +
            scale_y_continuous(labels = comma) +
            scale_fill_manual(values = c("dodgerblue1","orange1"),
                              labels = c("Gross", "Budget")) +
            labs(x = "Year",
                 y = "Movie Budget and Gross Income in Dollars",
                 title=paste0("Performance for ",input$var," Films Rated ",input$var2),
                 fill = "Legend") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(face = "bold")) +
            theme(axis.title = element_text(face = "bold")) +
            theme(legend.position = 'right')
    })
    
    #---------------------------------- ORDERED BARS AND TABLES ----------------------------------# 
    
    top_10 <- reactive({as.data.frame(
        movies_data_jh %>%
            replace_na(list(gross=0, score=0)) %>%
            filter(year == input$year) %>%
            filter(genre == input$genre) %>%
            arrange(input$measure) %>%
            top_n(10))
        
    })
    
    top_100 <- reactive({as.data.frame(
        movies_data_jh %>%
            replace_na(list(gross=0, score=0)) %>%
            filter(year == input$year) %>%
            filter(genre == input$genre) %>%
            arrange(input$measure) %>%
            top_n(100))
        
    })
    
    output$movies_bar <- renderPlot({
        temp <- as.data.frame(top_10())
        m_bar <-temp %>% ggplot(
            aes(x = reorder(name,get(input$measure)), y = get(input$measure))) +
            geom_bar(stat = 'identity', fill = "dodgerblue1", alpha = 0.8) + 
            coord_flip() +
            scale_y_continuous(labels = comma) +
            labs(x = element_blank(),
                 y = str_to_title(input$measure),
                 title=paste0("Top 10 Movies by ",str_to_title(input$measure)," in the ",input$genre," Genre for ",input$year)) + 
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(plot.title = element_text(face = "bold")) +
            theme(axis.title = element_text(face = "bold"))
        
        m_bar
    })
    
    output$movies_table <- DT::renderDataTable({
        if(input$show_table){
            DT::datatable(top_100()[, col_vals], rownames = FALSE)
        }})
    
    #---------------------------------- ABOUT ----------------------------------#
    
    output$Adam_title <- renderText({
        
        paste("<h3><b>Adam<b><h3>
            <br><h4><b><i>No Country for Old Men<i><b><h4>")
        
    })
    
    output$Jay_title <- renderText({
        
        paste("<h3><b>Jay<b><h3>
            <br><h4><b><i>Interstellar<i><b><h4>")
        
    })
    
    output$Joe_title <- renderText({
        
        paste("<h3><b>Joe<b><h3>
            <br><h4><b><i>Dead Poets Society<i><b><h4>")
        
    })
    
}

shinyApp(ui = ui, server = server)