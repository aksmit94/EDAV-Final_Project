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
cod <- read.csv('USAcod.csv', header = TRUE)

cod$Age <- as.integer(cod$Age)
cod$Year <- ymd(cod$Year, truncated = 2L) 
cod$ICDnumber <- as.integer(cod$ICDnumber)
cod$ICDcode <-  as.factor(cod$ICDcode)
cod$Deaths <- as.integer(cod$Deaths)

cod$AgeGroup <- ifelse(cod$Age < 2, "Infant", ifelse(cod$Age < 20, "Teen", ifelse(cod$Age < 60, "Adult", "Old")))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Visualizing diseases related deaths over years"),
   
   sidebarLayout(
     sidebarPanel(
     # Sidebar with a slider input for number of bins 
     checkboxGroupInput(inputId = "age",
                   label = "Age Group",
                   selected = "Teen",
                   choiceNames = list("Infants", "Teens", "Adults", "Old"),
                   choiceValues = list("Infant", "Teen", "Adult", "Old"))
       ),
     
    # Show a plot of the generated distribution
    mainPanel(plotOutput("deaths"), width="500px", height="500px")
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
   output$deaths <- renderPlot({
     
    filtered <- cod %>% filter(AgeGroup %in% input$age) %>% group_by(AgeGroup, Year) %>% summarize(total = sum(Deaths))  
    filtered %>% ggplot() + geom_line(aes(Year,total, col=AgeGroup)) + geom_point(aes(Year,total,  col=AgeGroup)) + xlab("Year") + ylab("Deaths") + scale_y_continuous(label = comma)
   }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

