
  
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(gtrendsR)
library(forcats)
library(shinythemes)

# Load state map data
state_map <- map_data("state")

# Define UI
ui <- fluidPage(
  titlePanel("Google Trends with Israel and Palestine Key Words"),
 
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Heatmap 1", plotOutput("plot1")),
        tabPanel("Heatmap 2", plotOutput("plot2")),
        tabPanel("Heatmap 3", plotOutput("plot3")),
        tabPanel("Interest Over Time", plotOutput("plot4")),
        tabPanel("State ANOVA", plotOutput("plot5")),
        tabPanel("Year ANOVA", plotOutput("plot6")),
        tabPanel("State ANOVA Boxplot", plotOutput("plot8")),
        tabPanel("Year ANOVA Boxplot", plotOutput("plot9")),
        tabPanel("Keyword t-test", plotOutput("plot10")),
        tabPanel("State ANOVA Summary", verbatimTextOutput("state_anova_summary")),
        tabPanel("Year ANOVA Summary", verbatimTextOutput("year_anova_summary")),
        tabPanel("t-test Summary", verbatimTextOutput("t_test_result"))
        
      ),
      theme = shinytheme("lumen")  # Apply the 'flatly' theme to the main panel
  ),
  theme = shinytheme("lumen")  # Apply the 'flatly' theme to the entire page
)
  


# Define server logic
server <- function(input, output) {
  # Load res1 data
  res <- gtrends(c("Israel genocide", "Israel terrorism"),
                 geo = c("US"), 
                 time = "today 12-m")
  res1 <- res$interest_by_region
  
  # Load state map data
  state_map <- map_data("state")
  
  # Prepare data for plotting
  my_df <- res1 %>%
    mutate(region = tolower(location)) %>%
    filter(region %in% state_map$region) %>%
    select(region, keyword, hits) %>%
    pivot_wider(names_from = keyword, values_from = hits) %>%
    mutate(`Israel genocide` = as.numeric(`Israel genocide`),
           `Israel terrorism` = as.numeric(`Israel terrorism`))
  
  # Calculate percentage difference
  final_data <- my_df %>%
    mutate(percent_difference = `Israel genocide` / (`Israel genocide` + `Israel terrorism`) * 100) %>%
    left_join(state_map, by = c("region" = "region"))
  
  # Plot heatmap 1
  output$plot1 <- renderPlot({
    ggplot(final_data, aes(x = long, y = lat, group = group, fill = percent_difference)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "blue", high = "red", na.value = "white", name = "Percent Difference") +
      labs(title = "Percentage Difference in Hits between 'Israel genocide' and 'Israel terrorism' in Each State") +
      theme_minimal()
  })
  
  # Plot heatmap 2
  output$plot2 <- renderPlot({
    ggplot(final_data, aes(x = long, y = lat, group = group, fill = `Israel genocide`)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "blue", high = "red", na.value = "white", name = "Total Hits") +
      labs(title = "Total Hits of 'Israel genocide' in Each State") +
      theme_minimal()
  })
  
  # Plot heatmap 3
  output$plot3 <- renderPlot({
    ggplot(final_data, aes(x = long, y = lat, group = group, fill = `Israel terrorism`)) +
      geom_polygon(color = "black") +
      scale_fill_gradient(low = "blue", high = "red", na.value = "white", name = "Total Hits") +
      labs(title = "Total Hits of 'Israel terrorism' in Each State") +
      theme_minimal()
  })
  
  # Extract interest over time data
  interest_over_time <- res$interest_over_time
  
  # Convert 'date' column to Date format
  interest_over_time$date <- as.Date(interest_over_time$date)
  interest_over_time <- interest_over_time %>%
    filter(date >= as.Date("2023-10-17"))
  
  # Prepare data for statistical analysis
  state_interest <- res$interest_by_region
  state_interest <- state_interest %>%
    mutate(hits = ifelse(hits == "" | hits == "<1", NA, as.numeric(hits))) %>%
    na.omit() # Remove rows with NA
  
  state_interest2 <- state_interest %>%
    mutate(state_group = ifelse(hits < 1, "Other States", as.character(location)))
  
  state_interest2$state_group <- as.factor(state_interest2$state_group)
  state_interest2$state_group <- fct_relevel(state_interest2$state_group, "Other States")
  
  # Plot interest over time
  output$plot4 <- renderPlot({
    ggplot(interest_over_time, aes(x = date, y = hits, color = keyword)) +
      geom_line() +
      xlab("Date") +
      ylab("Search hits") +
      ggtitle("Interest over time") +
      theme_bw() +
      theme(legend.title = element_blank())
  })
  
  #t-test on hits by keyword
  keyword_t<- t.test(hits~keyword, data = interest_over_time)

  #Anova on hits by states
  states_aov<- aov(hits~keyword+state_group, data = state_interest2)

  #Anova on hits by year
  year_aov<- aov(hits~keyword+date, data = interest_over_time)
  
  output$t_test_result <- renderPrint({
    keyword_t
  })
  
  output$state_anova_summary <- renderPrint({
    summary(states_aov)
  })
  
  output$year_anova_summary <- renderPrint({
    summary(year_aov)
  })

  # Render ANOVA plots
  # State ANOVA plot
  output$plot5 <- renderPlot({ ggplot(state_interest2, aes(x = state_group, y = hits)) +
      geom_boxplot() +
      xlab("State Group") +
      ylab("Hits") +
      ggtitle("Mean Hits by State Group")
  })
  # Year ANOVA plot
  output$plot6 <- renderPlot({
    interest_over_time$date <- as.factor(interest_over_time$date)
    ggplot(interest_over_time, aes(x = date, y = hits)) +
      geom_boxplot() +
      xlab("Date") +
      ylab("Hits") +
      ggtitle("Mean Hits by Date")
  })
  
  # Plot keyword t-test
  output$plot7 <- renderPlot({
    ggplot(state_interest, aes(x = keyword, y = hits)) +
      geom_boxplot() +
      xlab("Keyword") +
      ylab("Hits") +
      ggtitle("Mean Hits by Keyword")
  })
  
  # Plot State ANOVA plot
  output$plot8 <- renderPlot({
    ggplot(state_interest2, aes(x = state_group, y = hits)) +
      geom_boxplot() +
      xlab("State Group") +
      ylab("Hits") +
      ggtitle("Mean Hits by State Group")
  })
  
  # Plot Year ANOVA plot
  output$plot9 <- renderPlot({
    interest_over_time$date <- as.factor(interest_over_time$date)
    ggplot(interest_over_time, aes(x = date, y = hits)) +
      geom_boxplot() +
      xlab("Date") +
      ylab("Hits") +
      ggtitle("Mean Hits by Date")
  })
  output$plot10 <- renderPlot({
    ggplot(state_interest, aes(x = keyword, y = hits)) +
      geom_boxplot() +
      xlab("Keyword") +
      ylab("Hits") +
      ggtitle("Mean Hits by Keyword")
  })
}

# Run the application
shinyApp(ui = ui, server = server)