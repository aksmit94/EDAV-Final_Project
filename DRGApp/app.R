#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(lubridate)
library(tidyverse)
library(plotly)
library(scales)
library(shinythemes)


data_na_removed <- as.tibble(read_csv("Inpatient_Cost.csv"))

data_na_removed <- data_na_removed[,-1]

data_na_removed$`DRG Definition` <- gsub('(.*?)- ', '',data_na_removed$`DRG Definition`)

drg_grouped_by_year <-  data_na_removed %>% group_by(Diagnostic_category, `DRG Definition`, `Year`,`Provider State`) %>%
  summarise(Count_Discharge = sum(`Total Discharges`),Sum_Amount = sum(`Total_Payments`))

drg_grouped_by_year  <- drg_grouped_by_year  %>% group_by(Year) %>% mutate(Percent_Discharges = 100 * Count_Discharge/sum(Count_Discharge),
                                                                             Percent_Cost = 100 * Sum_Amount/sum(Sum_Amount))

# Define UI for application that draws a histogram
ui <- fluidPage(

    h1("How to use this app:"),
    div(tags$i("Best viewed in Chrome (version 70+) at 100% zoom")),
    h3("1. Visualizing Medicare Costs in the US"),
    div("The default choropleth shows the average Medicare costs from 2011 to 2016 over all states in the US.
        Hovering on any state provides the total discharges over these years in that state.
        Clicking on a state shows the trend of healthcare costs over the years.
        The choropleth can be filtered based on (multiple) Disease Category and the Diseases in that category.", style = "width: 1000px; font-size:18px"),
    
    h3("2. Find my Provider"),
    div("This app shows the Top 10 (or less) most and least expensive healthcare providers for a selected state and disease type,
        which can be selected from the panel on the left", style = "width: 1000px; font-size:18px"),
    
    br(),
    
  theme = shinytheme("flatly"),
  navbarPage("US Heathcare in Numbers",
             tabPanel(
               # Application title
               "Visualizing Medicare Costs in the US",
               
               sidebarPanel(
                 h3("Filters"),
                 
                 selectizeInput("CatInput", 
                                label = "Select a Disease Category",
                                choices = unique(drg_grouped_by_year$Diagnostic_category), selected = NULL, multiple = TRUE,
                                options = NULL),
                 
                 selectizeInput("DRGInput", 
                                label = "Select a Disease",
                                choices = unique(drg_grouped_by_year$`DRG Definition`), selected = NULL, multiple = TRUE,
                                options = NULL),
                 
                 plotlyOutput("scatter")
               ),

               mainPanel(plotlyOutput("deaths")
                         ) 
               
             ),
            tabPanel(
              # Application title
              "Find my Provider",
              
              sidebarPanel(
                h3("Filters"),
                
                selectInput("StateInput", label = "Select State", 
                                   choices = unique(data_na_removed$`Provider State`)
                                  ),
                
                selectInput("CatInput2", 
                            label = "Select Disease Category",
                            choices = unique(data_na_removed$Diagnostic_category)
                              )
                
              ),
              
              mainPanel(plotlyOutput("highhospitals"),
                        br(),
                        br(),
                        br(),
                        br(),
                        plotlyOutput("lowhospitals")) 
            ))
  
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({
    # Can also set the label and select items
    subset <- unique(data_na_removed %>% filter(Diagnostic_category %in% input$CatInput) %>% select('DRG Definition'))
    
    updateSelectizeInput(session, "DRGInput",
                         label = "Select a Disease",
                         choices = subset,
                         selected = NULL)  
  })
  
  
  output$deaths <- renderPlotly({
    
    if(length(input$CatInput) != 0)
    {
      if(length(input$DRGInput) != 0)
      {
        filtered <- drg_grouped_by_year %>% filter(Diagnostic_category %in% input$CatInput) %>% filter(`DRG Definition` %in% input$DRGInput)
      }
      else
      {
        filtered <- drg_grouped_by_year %>% filter(Diagnostic_category %in% input$CatInput)
      }
      
      filtered$Average_Amount <- filtered$Sum_Amount/filtered$Count_Discharge
      
      total_cost_by_state <- filtered %>% dplyr::group_by(`Provider State`) %>% 
        dplyr::summarise(Total_Discharges = sum(Count_Discharge),
                         Total_Cost = sum(Sum_Amount))
      
      total_cost_by_state$Average_Cost <- total_cost_by_state$Total_Cost/total_cost_by_state$Total_Discharges
      
      colnames(total_cost_by_state)[1] <- 'state'
      
      total_cost_by_state$hover <- with(total_cost_by_state, paste(state, '<br>', "Total Discharges: ", Total_Discharges))
      
      # give state boundaries a white border
      l <- list(color = toRGB("white"), width = 2)
      
      # specify some map projection/options
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      
      # Create choropleth
      p <- plot_geo(total_cost_by_state, locationmode = 'USA-states') %>%
        add_trace(
          z = ~Average_Cost, text = ~hover, locations = ~state,
          color = ~Average_Cost, colors = 'Purples'
        ) %>%
        colorbar(title = "Average USD") %>%
        layout(
          geo = g
        ) %>% layout(autosize = F, width = 900, height = 800)
      
      plotly_build(p)
    }
    else
    {
     
      drg_grouped_by_year$Average_Amount <- drg_grouped_by_year$Sum_Amount/drg_grouped_by_year$Count_Discharge
      
      total_cost_by_state <- drg_grouped_by_year %>% dplyr::group_by(`Provider State`) %>% 
        dplyr::summarise(Total_Discharges = sum(Count_Discharge),
                         Total_Cost = sum(Sum_Amount))
      
      total_cost_by_state$Average_Cost <- total_cost_by_state$Total_Cost/total_cost_by_state$Total_Discharges
      
      colnames(total_cost_by_state)[1] <- 'state'
      
      total_cost_by_state$hover <- with(total_cost_by_state, paste(state, '<br>', "Total Discharges: ", Total_Discharges))
      
      # give state boundaries a white border
      l <- list(color = toRGB("white"), width = 2)
      
      # specify some map projection/options
      g <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showlakes = TRUE,
        lakecolor = toRGB('white')
      )
      
      #Create Choropleth
      p <- plot_geo(total_cost_by_state, locationmode = 'USA-states') %>%
        add_trace(
          z = ~Average_Cost, text = ~hover, locations = ~state,
          color = ~Average_Cost, colors = 'Purples'
        ) %>%
        colorbar(title = "Average USD") %>%
        layout(
          geo = g
        ) %>% layout(autosize = F, width = 900, height = 800)
      
      plotly_build(p)
 
    }
    
  })
  
  # On Click graph
  output$scatter <- renderPlotly({
    
    # Read in hover data
    eventdata <- event_data("plotly_click")
    
    # If NULL dont do anything
    if(is.null(eventdata) == T) return(NULL)
    
    # Get point number
    datapoint <- as.numeric(eventdata$pointNumber)[1]
    
    total_cost_by_state <- drg_grouped_by_year %>% dplyr::group_by(`Provider State`) %>% 
      dplyr::summarise(Total_Discharges = sum(Count_Discharge),
                       Total_Cost = sum(Sum_Amount))
    
    # Get State
    state <- as.character(total_cost_by_state[datapoint + 1, 1])
    
    # Get data for state
    if(is.null(state))
    {
      data <- data_na_removed %>% group_by(Year) %>% summarize(total_cost = sum(`Total_Payments`), total_discharges = sum(`Total Discharges`)) %>% mutate(avg = total_cost/total_discharges)  
      
    }
    else
    {
      data <- data_na_removed %>% filter(`Provider State` == state) %>% group_by(Year) %>% summarize(total_cost = sum(`Total_Payments`), total_discharges = sum(`Total Discharges`)) %>% mutate(avg = total_cost/total_discharges) 
    }
    
    # Show correlation heatmap
    # ggplot(data) + geom_point(aes(x = Year, y = avg), col="blue") + geom_path(aes(x = Year, y = avg), col = 'black') + scale_y_continuous(label = comma) + ylab(paste0("Average Cost for ", state))
    
    p <- plot_ly(data, x = ~Year, y = ~avg, name = "Year", type = 'scatter',
                 mode = "lines+markers", marker = list(color = "blue")) %>% 
      
      layout(
        title = paste0("Trend of total healthcare cost in ", state),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Cost in $"),
        margin = list(l = 100)
      )
    
    plotly_build(p)
    
  })

  
  
  output$highhospitals <- renderPlotly({
    
    if(length(input$StateInput) != 0)
    {
      state_data <- data_na_removed %>% filter(`Provider State` %in% input$StateInput)
      
      if(length(input$CatInput2) != 0)
      {
        state_data <- state_data %>% filter(Diagnostic_category %in% input$CatInput2)
      }
    }
    else
    {
      state_data <- data_na_removed
    }
     
    min_num_of_discharges <- 50
    
    provider_information <- state_data %>% group_by(`Provider Name`) %>% filter(sum(`Total Discharges`) > min_num_of_discharges) %>%
      summarise(Average_Cost = sum(Total_Payments)/sum(`Total Discharges`)) 
    
    number <- 10  
      
     
      costly_providers <- provider_information[order(-provider_information$Average_Cost),]
      
      costly_providers$`Provider Name` <- as.character(costly_providers$`Provider Name`) 
      
      costly_providers <- costly_providers[1:10,]
      
      
      p <- plot_ly(costly_providers, x = ~Average_Cost, y = ~`Provider Name`, name = "", type = 'bar',
                   mode = "markers", marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                                   line = list(color = 'rgba(58, 71, 80, 1.0)',
                                                               width = 3))) %>% 
      
        layout(
          title = "Average Cost of most expensive Providers",
          xaxis = list(title = "Average Cost in $"),
          yaxis = list(title = ""),
          margin = list(l = 100)
        )
      
      plotly_build(p)

  })
  
  output$lowhospitals <- renderPlotly({
    
    
    if(length(input$StateInput) != 0)
    {
      state_data <- data_na_removed %>% filter(`Provider State` %in% input$StateInput)
      
      if(length(input$CatInput2) != 0)
      {
        state_data <- state_data %>% filter(Diagnostic_category %in% input$CatInput2)
      }
    }
    else
    {
      state_data <- data_na_removed
    }
    
    min_num_of_discharges <- 50
    
    provider_information <- state_data %>% group_by(`Provider Name`) %>% filter(sum(`Total Discharges`) > min_num_of_discharges) %>%
      summarise(Average_Cost = sum(Total_Payments)/sum(`Total Discharges`), Discharges = sum(`Total Discharges`))
    
    number <- 10
      
    cheapest_providers <- head(provider_information[order(provider_information$Average_Cost),], number)
    
    
    p <- plot_ly(cheapest_providers, x = ~Average_Cost, y = ~`Provider Name`, name = "", type = 'bar',
                 mode = "markers", marker = list(color = 'rgba(58, 71, 80, 0.6)',
                                                 line = list(color = 'rgba(58, 71, 80, 1.0)',
                                                             width = 3))) %>% 
      
      layout(
        title = "Average Cost of least expensive Providers",
        xaxis = list(title = "Average Cost in $"),
        yaxis = list(title = ""),
        margin = list(l = 100)
      )
    
    plotly_build(p)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

