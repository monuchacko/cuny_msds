#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)
library(tibble)

df_clean <-read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA608/master/lecture3/data/cleaned-cdc-mortality-1999-2010-2.csv')
df_clean_2010 <- df_clean %>%
    filter(Year == 2010) %>%
    group_by(State, ICD.Chapter) %>%
    mutate(Count = sum(Deaths), Crude.Rate = 10^5 * (Count / Population))

#Question 1
#As a researcher, you frequently compare mortality rates from particular causes across
#different States. You need a visualization that will let you see (for 2010 only) the crude
#mortality rate, across all States, from one cause (for example, Neoplasms, which are
#effectively cancers). Create a visualization that allows you to rank States by crude mortality
#for each cause of death.

#Question 2
#Often you are asked whether particular States are improving their mortality rates (per cause)
#faster than, or slower than, the national average. Create a visualization that lets your clients
#see this for themselves for one cause of death at the time. Keep in mind that the national
#average should be weighted by the national population.


# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "Mortality",
    fluidRow(
        column(12,
               h4("Mortality Rates by State (Year 2010)")
        ),
        column(6, 
               sidebarPanel(
                   selectInput('icdchapter', label = h3("Causes of Death"), unique(unique(df_clean$ICD.Chapter)), selected=1)
               )),
        column(6,
               mainPanel(
                   plotOutput('plot1')
               ))
    ),
    hr(),
    fluidRow(
        column(12,
               h4("Mortality Rate - All States Vs National Average")
        ),
        column(6,
               sidebarPanel(
                   selectInput('state2', label = h3("State"), unique(unique(df_clean$State)), selected=1)
               ),
               sidebarPanel(
                   selectInput('icdchapter2', label = h3("Causes of Death"), unique(unique(df_clean$ICD.Chapter)), selected=1)
               )
        ),
        column(6,
               mainPanel(
                   plotOutput('plot2')
               ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot({
        
        dfSlice <- df_clean_2010 %>%
            filter(ICD.Chapter == input$icdchapter)
        
        ggplot(dfSlice, aes(x = reorder(State, Crude.Rate), y = Crude.Rate, color = Crude.Rate)) +
            labs(x = "State", y = "Crude Mortality Rate") +  
            geom_bar(stat = "identity") +
            coord_flip() +
            theme_minimal()
    })
    
    output$plot2 <- renderPlot({
        
        dfSlice2 <- df_clean %>%
            group_by(Year, ICD.Chapter) %>%
            mutate(N_Population = sum(Population),
                   N_Count = sum(Deaths), 
                   N_Crude_Rate = 10^5*(N_Count/N_Population)) %>% 
            group_by(Year, ICD.Chapter, State) %>%
            mutate(S_Count=sum(Deaths),
                   S_Crude_Rate=10^5*(S_Count/Population)) %>%
            select(ICD.Chapter, State, Year, N_Crude_Rate, S_Crude_Rate) %>% 
            filter(ICD.Chapter == input$icdchapter2, State == input$state2) 
        
        ggplot(dfSlice2) +
            geom_bar(aes(x = Year, weight = S_Crude_Rate)) +
            labs(x = "Year", y = "Crude Mortality Rate") + 
            geom_line(aes(x = Year, y = N_Crude_Rate, linetype = "National Average"), col = "red", lwd = 1) +
            scale_linetype(name = NULL) +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
