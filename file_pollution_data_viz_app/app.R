library(shiny)
library(tidyverse)
library(ggtext)

df <- read.csv(here::here('pollution_419.csv'))

ui <- fluidPage(
  titlePanel('Dynamic Histogram Generator'),
    selectInput('column', 'Select Column to Analyze:', 
                  c('Precipitation', 
                    'Education Level',
                    'Nonwhite %', 
                    'Nitrogen Oxides', 
                    'Sulfur Dioxide')), 
    plotOutput('main_hist'),
    fluidRow(
      column(4, plotOutput("plot1")),
      column(4, plotOutput("plot2")),
      column(4, plotOutput("plot3"))
    )
)

server <- function(input, output) {
  
  column_name <- reactive({
    case_when(input$column == 'Precipitation' ~ 'PRECIP',
              input$column == 'Education Level' ~ 'EDUC', 
              input$column == 'Nonwhite %' ~ 'NONWHITE',
              input$column == 'Nitrogen Oxides' ~ 'NOX', 
              input$column == 'Sulfur Dioxide' ~ 'SO2')
  })
  
  dat <- reactive(df[c('MORTRANK', column_name())])

  output$main_hist <- renderPlot({

  dat() %>% 
    ggplot(aes(x = .data[[column_name()]], fill = as.factor(MORTRANK))) +
      geom_histogram(bins = 15) +
      geom_vline(aes(xintercept = mean(.data[[column_name()]])), col = 'purple') + 
      geom_vline(aes(xintercept = median(.data[[column_name()]])), col = '#B8860B', linetype = 'dashed') +
      labs(x = input$column, fill = 'MORTRANK', y = '', title = paste('Distribution of', input$column),
           subtitle = paste0(
             "<span style='color:purple;'><b>Mean: ", round(mean(dat()[[column_name()]]), 1), "</b></span> | ",
             "<span style='color:#B8860B;'><b>Median: ", round(median(dat()[[column_name()]]), 1), "</b></span> |",
             " SD: ", round(sd(dat()[[column_name()]]), 1)
           )) +
      theme_bw() +
      theme(plot.subtitle = element_markdown(size = 14))
      
  })
  
  output$plot1 <- renderPlot({
    
    subdat <- dat() %>% filter(MORTRANK == 1) 
    
    subdat %>%
      ggplot(aes(x = .data[[column_name()]])) +
      geom_histogram(bins = 10, fill = '#F8766D') +
      geom_vline(aes(xintercept = mean(.data[[column_name()]])), col = 'purple') + 
      geom_vline(aes(xintercept = median(.data[[column_name()]])), col = '#B8860B', linetype = 'dashed') +
      labs(x = input$column, fill = 'MORTRANK', y = '', title = paste('Distribution of', input$column, '| Group 1'),
           subtitle = paste0(
             "<span style='color:purple;'><b>Mean: ", round(mean(subdat[[column_name()]]), 1), "</b></span> | ",
             "<span style='color:#B8860B;'><b>Median: ", round(median(subdat[[column_name()]]), 1), "</b></span> |",
             " SD: ", round(sd(subdat[[column_name()]]), 1)
           )) +
      theme_bw() +
      theme(plot.subtitle = element_markdown(size = 14))
    
  })
  output$plot2 <- renderPlot({
    
    subdat <- dat() %>% filter(MORTRANK == 2) 
    
    subdat %>%
      ggplot(aes(x = .data[[column_name()]])) +
      geom_histogram(bins = 10, fill = '#00BA38') +
      geom_vline(aes(xintercept = mean(.data[[column_name()]])), col = 'purple') + 
      geom_vline(aes(xintercept = median(.data[[column_name()]])), col = '#B8860B', linetype = 'dashed') +
      labs(x = input$column, fill = 'MORTRANK', y = '', title = paste('Distribution of', input$column, '| Group 2'),
           subtitle = paste0(
             "<span style='color:purple;'><b>Mean: ", round(mean(subdat[[column_name()]]), 1), "</b></span> | ",
             "<span style='color:#B8860B;'><b>Median: ", round(median(subdat[[column_name()]]), 1), "</b></span> |",
             " SD: ", round(sd(subdat[[column_name()]]), 1)
           )) +
      theme_bw() +
      theme(plot.subtitle = element_markdown(size = 14))
    
  })
  output$plot3 <- renderPlot({
    
    subdat <- dat() %>% filter(MORTRANK == 3) 
    
    subdat %>%
      ggplot(aes(x = .data[[column_name()]])) +
      geom_histogram(bins = 10, fill = '#619CFF') +
      geom_vline(aes(xintercept = mean(.data[[column_name()]])), col = 'purple') + 
      geom_vline(aes(xintercept = median(.data[[column_name()]])), col = '#B8860B', linetype = 'dashed') +
      labs(x = input$column, fill = 'MORTRANK', y = '', title = paste('Distribution of', input$column, '| Group 3'),
           subtitle = paste0(
             "<span style='color:purple;'><b>Mean: ", round(mean(subdat[[column_name()]]), 1), "</b></span> | ",
             "<span style='color:#B8860B;'><b>Median: ", round(median(subdat[[column_name()]]), 1), "</b></span> |",
             " SD: ", round(sd(subdat[[column_name()]]), 1)
           )) +
      theme_bw() +
      theme(plot.subtitle = element_markdown(size = 14))
    
  })
}
 
shinyApp(ui = ui, server = server)
