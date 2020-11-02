library(shiny)
library(tidyverse)
library(WikidataQueryServiceR)


sparql_query <- 'SELECT (year(?date) as ?year) ?songLabel
                         WHERE
                         {
                           ?song wdt:P31 wd:Q7366 .   # instance of a song
                           ?song wdt:P175 wd:Q1299 .  # performer is The Beatles
                           ?song wdt:P577 ?date .     # has publication date
                           SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
                         }'

beatles_songs <- query_wikidata(sparql_query) 


ui <- fluidPage(
    
    titlePanel("Songs by The Beatles"),
    
    fluidRow(column(4,
                    sliderInput("years",
                                "Years:",
                                sep = "",
                                min = 1962,
                                max = 1980,
                                value = c(1962, 1980))),
             column(8,
                    mainPanel(
                        plotOutput("beatles_plot")
                        )
                    )
             )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$beatles_plot <- renderPlot({
        
        beatles_songs %>% 
            filter(year > min(input$years), 
                   year < max(input$years)) %>% 
            ggplot(aes(x = year)) +
            geom_histogram(col = "white", fill = "#438aca", bins = max(input$years) - min(input$years)) +
            theme_classic() +
            labs(
                x = "Year",
                y = "Count",
                title = "Number of The Beatles songs released each year"
            ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
