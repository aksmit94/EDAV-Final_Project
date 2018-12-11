#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(plyr)
library(shiny)


#################################
##### BEGIN STATES PORTION ######
#################################

## USA COD files have mortality data by gender, age and cause from 1960 to 2015.
## Consolidating individual state datasets into a single dataset
states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
filenames <- vector("character", 0)
for(s in states) {
  for(g in c("m", "f", "b")) {
    filenames[length(filenames)+1] <- paste("lifetables/", s, "/", s, "_", g, "ltper_5x5.csv", sep="")
  }
}
files <- list()
for(n in filenames){
  files[[length(files)+1]] <- read.csv(n)
}

combinedData <- ldply(files, data.frame)
combinedQ <- combinedData %>% select(PopName, Sex, Year, Age, qx) %>% as.tibble()
combinedQ <- combinedQ %>% filter(Age != "100-104", Age != "105-109", Age != "110+")
combinedQ$Age <- factor(combinedQ$Age, c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"))
# qx's seem to be 1 minus prob of surviving the age group. Converting to 1 minus prob of surviving 1 year.
combinedQ$qx[combinedQ$Age != "0" & combinedQ$Age != "1-4"] <- 1 - (1 - combinedQ$qx[combinedQ$Age != "0" & combinedQ$Age != "1-4"])^(1/5)
combinedQ$qx[combinedQ$Age == "1-4"] <- 1 - (1 - combinedQ$qx[combinedQ$Age == "1-4"])^(1/4)

# Convert the mortality dataset into a mortality improvement dataset
combinedImprov <- combinedQ %>% spread(Year, qx)
combinedImprov.orig <- combinedImprov
combinedImprov[, 5:ncol(combinedImprov)] <- 
  combinedImprov[, 5:ncol(combinedImprov)] / combinedImprov[, 4:(ncol(combinedImprov)-1)] - 1
combinedImprov <- combinedImprov[,-4]
combinedImprov %>% gather(Year, Improvement, -PopName, -Sex, -Age) -> combinedImprov
combinedImprov$Year <- as.factor(combinedImprov$Year)

#################################
##### END STATES PORTION ######
#################################

#################################
##### BEGIN US AGG PORTION ######
#################################

us <- read.csv("HMD-COD-USA/HMD-COD_5x1.csv") %>% as.tibble()

# Set disease codes
infections <- c("Tuberculosis", "Syphilis", "Infectious Gastro-enteritis", "Dysentery", "Meningococcal infection", "Septicemia", "Viral hepatitis", "HIV-AIDS", "Other infectious diseases")
infections_g <- rep("Certain infectious diseases", length(infections))
neoplasms <- c("Lip/oral cavity/pharynx", "Esophagus", "Stomach", "Colon", "Rectum", "Liver, gallbladder and bile ducts", "Pancreas", "Larynx", "Trachea, bronchus, and lung", "Skin", "Breast", "Cervix uteri", "Other uterus", "Ovary", "Prostate", "Kidney", "Bladder", "Hodgkin lymphoma", "Non-Hodgkin lymphoma", "Myeloma", "Leukemia", "Other malignant neoplasms", "Other neoplasms")
neoplasms_g <- rep("Malignant neoplasms", length(neoplasms))
blood <- c("Diseases of the blood and blood-forming organs")
blood_g <- rep("Blood", length(blood))
endocrine <- c("Diabetes Mellitus", "Overweight, obesity, and other hyperalimentation", "Other endocrine, nutritional and metabolic diseases")
endocrine_g <- rep("Endocrine, nutritional and metabolic diseases", length(endocrine))
mental <- c("Alcohol abuse", "Drug dependence, toxicomania", "Other mental and behavioural disorders")
mental_g <- rep("Mental and behavioural disorders", length(mental))
nervous <- c("Meningitis", "Parkinson's disease", "Alzheimer's disease", "Multiple sclerosis", "Other diseases of the nervous system and the sense organs")
nervous_g <- rep("Diseases of the nervous system and the sense organs", length(nervous))
heart <- c("Chronic rheumatic heart diseases", "Acute rheumatic heart diseases", "Arteriosclerotic/ischaemic and degenerative", "Hypertensive heart disease", "Pulmonary heart disease and diseases of pulmonary circulation", "Other forms of heart disease")
heart_g <- rep("Heart disease", length(heart))
cerebrovascular <- c("Cerebrovascular disease")
cerebrovascular_g <- rep("Cerebrovascular", length(cerebrovascular))
circulatory <- c("Artherosclerosis", "Aortic aneurysm", "Other diseases of arteries, arterioles and capillaries", "Other disorder of the circulatory system")
circulatory_g <- rep("Circulatory disorders", length(circulatory))
respiratory <- c("Influenza", "Other acute respiratory infections", "Pneumonia", "Chronic bronchitis", "Asthma", "Other obstructive pulmonary diseases", "Other respiratory diseases")
respiratory_g <- rep("Respiratory diseases", length(respiratory))
digestive <- c("Gastric and duodenal ulcer", "Gastro-enteritis (non-infectious)", "Chronic liver diseases and cirrhosis", "Other diseases of the digestive system")
digestive_g <- rep("Digestive system diseases", length(digestive))
skin <- c("Infections of skin and subcutaneous tissue", "Non-infectious diseases of the skin and subcutaneous tissue")
skin_g <- rep("Skin diseases", length(skin))
musculoskeletal <- c("Rheumatoid arthritis and osteoarthrosis", "Other diseases of the musculoskeletal system/connective tissue")
musculoskeletal_g <- rep("Musculoskeletal diseases", length(musculoskeletal))
genitourinary <- c("Nephritis, nephrosis and renal failure", "Infections of kidney", "Other diseases of kidney and ureter", "Other diseases of the genitourinary system")
genitourinary_g <- rep("Genitourinary diseases", length(genitourinary))
pregnancy <- c("Complications of pregnancy, childbirth and puerperium")
pregnancy_g <- rep("Pregnancy", length(pregnancy))
perinatal <- c("Certain conditions originating in the perinatal period")
perinatal_g <- rep("Perinatal", length(perinatal))
congenital <- c("Congenital malformations of the nervous system", "Congenital malformations of the circulatory system", "Other congenital malformations/anomalies")
congenital_g <- rep("Congenital malformations/anomalies", length(congenital))
unknown <- c("Senility without psychosis", "Sudden death", "Unknown and unspecified causes", "Other ill-defined or unknown")
unknown_g <- rep("Ill-defined or unknown", length(unknown))
external <- c("Motor vehicle accidents", "Accidental falls", "Accidental poisoning by alcohol", "Other accidental poisoning", "Other accidents", "Suicide", "Homicide", "Events of undetermined intent", "Other external causes")
external_g <- rep("External causes", length(external))

diseases <- c(infections, neoplasms, blood, endocrine, mental, nervous, heart, cerebrovascular, circulatory, respiratory, digestive, skin, musculoskeletal, genitourinary, pregnancy, perinatal, congenital, unknown, external, "All")
diseases_g <- c(infections_g, neoplasms_g, blood_g, endocrine_g, mental_g, nervous_g, heart_g, cerebrovascular_g, circulatory_g, respiratory_g, digestive_g, skin_g, musculoskeletal_g, genitourinary_g, pregnancy_g, perinatal_g, congenital_g, unknown_g, external_g, "All")
diseases_g_full <- paste(diseases_g, diseases, sep=" - ")

codes <- data.frame(
  COD.cat = levels(us$COD.cat),
  Disease = diseases,
  Group = diseases_g,
  Group.Disease = diseases_g_full
)


us <- us %>% full_join(codes, by=c("COD.cat"))


us %>% mutate(
  Pop.F = Deaths.F / (Rates.F / 100000),
  Pop.M = Deaths.M / (Rates.M / 100000), 
  Pop.T = Deaths.T / (Rates.T / 100000)
) -> us

us <- us %>% filter(Age != "85+", Age != "90+", Age != "95+", Age != "100+", Age != "105+")

us$Age <- factor(us$Age, c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99"))

us_5x5 <- us
us_5x5 <- us_5x5 %>% filter(Year >= 1960, Year <= 2014)
us_5x5 <- us_5x5 %>% mutate(
  YearBand = ifelse(
    Year<1965, "1960-1964", ifelse(
      Year<1970, "1965-1969", ifelse(
        Year<1975, "1970-1974", ifelse(
          Year<1980, "1975-1979", ifelse(
            Year<1985, "1980-1984", ifelse(
              Year<1990, "1985-1989", ifelse(
                Year<1995, "1990-1994", ifelse(
                  Year<2000, "1995-1999", ifelse(
                    Year<2005, "2000-2004", ifelse(
                      Year<2010, "2005-2009", ifelse(
                        Year<2015, "2010-2014", NA)))))))))))
)
us_5x5$YearBand <- as.factor(us_5x5$YearBand)


# Calculate aggregate death counts and populations for year bands
us_5x5_t <- us_5x5 %>% group_by(YearBand, Age, COD.cat, Disease, Group, Group.Disease) %>% dplyr::summarize(
  Deaths.F.t = sum(Deaths.F, na.rm=T), 
  Deaths.M.t = sum(Deaths.M, na.rm=T), 
  Deaths.T.t = sum(Deaths.T, na.rm=T), 
  Pop.F.t = sum(Pop.F, na.rm=T), 
  Pop.M.t = sum(Pop.M, na.rm=T), 
  Pop.T.t = sum(Pop.T, na.rm=T)
)

# Calculate qx's
us_5x5_t <- us_5x5_t %>% mutate(
  qx_F = Deaths.F.t / Pop.F.t, 
  qx_M = Deaths.M.t / Pop.M.t, 
  qx_T = Deaths.T.t / Pop.T.t
)

# Gather qx's into m/f/b
us_5x5_t_g <- 
  us_5x5_t %>% 
  select(YearBand, Age, COD.cat, Disease, Group, Group.Disease, qx_F, qx_M, qx_T) %>% 
  gather(Sex, qx, -YearBand, -Age, -COD.cat, -Disease, -Group, -Group.Disease)

# Rename "YearBand to Year"
us_5x5_t_g <- us_5x5_t_g %>% dplyr::rename(Year = YearBand)

# Rename qx's to "m"/"f"/"b"
us_5x5_t_g$Sex[us_5x5_t_g$Sex=="qx_F"] <- "f"
us_5x5_t_g$Sex[us_5x5_t_g$Sex=="qx_M"] <- "m"
us_5x5_t_g$Sex[us_5x5_t_g$Sex=="qx_T"] <- "b"
us_5x5_t_g$Sex <- as.factor(us_5x5_t_g$Sex)


# Convert the mortality dataset into a mortality improvement dataset
us_5x5_t_g_Improv <- us_5x5_t_g %>% spread(Year, qx)
us_5x5_t_g_Improv[, 8:ncol(us_5x5_t_g_Improv)] <- 
  us_5x5_t_g_Improv[, 8:ncol(us_5x5_t_g_Improv)] / us_5x5_t_g_Improv[, 7:(ncol(us_5x5_t_g_Improv)-1)] - 1
us_5x5_t_g_Improv <- us_5x5_t_g_Improv[,-7]
us_5x5_t_g_Improv <- us_5x5_t_g_Improv %>% gather(Year, Improvement, -COD.cat, -Disease, -Group, -Group.Disease, -Sex, -Age)




# Calculate death counts and populations for years
us_5x1_t <- us %>% group_by(Year, Age, COD.cat, Disease, Group, Group.Disease) %>% dplyr::summarize(
  Deaths.F.t = sum(Deaths.F, na.rm=T), 
  Deaths.M.t = sum(Deaths.M, na.rm=T), 
  Deaths.T.t = sum(Deaths.T, na.rm=T), 
  Pop.F.t = sum(Pop.F, na.rm=T), 
  Pop.M.t = sum(Pop.M, na.rm=T), 
  Pop.T.t = sum(Pop.T, na.rm=T)
) %>% as.tibble()

# Calculate qx's
us_5x1_t <- us_5x1_t %>% mutate(
  qx_F = Deaths.F.t / Pop.F.t, 
  qx_M = Deaths.M.t / Pop.M.t, 
  qx_T = Deaths.T.t / Pop.T.t
)

# Gather qx's into m/f/b
us_5x1_t <- 
  us_5x1_t %>% 
  select(Year, Age, COD.cat, Disease, Group, Group.Disease, qx_F, qx_M, qx_T) %>% 
  gather(Sex, qx, -Year, -Age, -COD.cat, -Disease, -Group, -Group.Disease)

# Rename qx's to "m"/"f"/"b"
us_5x1_t$Sex[us_5x1_t$Sex=="qx_F"] <- "f"
us_5x1_t$Sex[us_5x1_t$Sex=="qx_M"] <- "m"
us_5x1_t$Sex[us_5x1_t$Sex=="qx_T"] <- "b"
us_5x1_t$Sex <- as.factor(us_5x1_t$Sex)

# Filter to only 1965-2014
us_5x1_t <- us_5x1_t %>% filter(Year>=1965, Year<=2014)


#################################
##### END US AGG PORTION ######
#################################




#################################
##### BEGIN DYNAMIC PORTION ######
#################################

##### BEGIN UI #####

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  h1("How to use this app:"),
  div(tags$i("Best viewed in Chrome (version 70+) at 100% zoom")),
  
  div(tags$i("Please note that this app takes about 25-30 seconds to refresh on input")),
  
  br(),
  
  
  # Application title
  titlePanel("Mortality Improvement per State"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="state", 
        label="Select a State", 
        choices=states, 
        selected = "NY", 
        multiple = FALSE
      ),
      selectInput(
        inputId="sex", 
        label="Select Male/Female/Both", 
        choices=c("m", "f", "b"), 
        selected = "b", 
        multiple = FALSE
      ),
      selectInput(
        inputId="age", 
        label="Select an Age Group (for Line Plot only)", 
        choices=levels(us_5x5_t_g$Age), 
        selected = "65-69", 
        multiple = TRUE
      ),
      selectInput(
        inputId="disease", 
        label="Select a Cause of Death (for Line Plot only)", 
        choices=diseases_g_full, 
        selected = "All - All", 
        multiple = FALSE
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("improvement_heatmap"),
      plotOutput("improvement_comparison_heatmap"),
      plotOutput("mort_comparison_heatmap"),
      plotOutput("mortality_trend_bygroup")
    )
  )
)

##### END UI #####


##### BEGIN SERVER #####

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  # Mortality improvement
  
  output$improvement_heatmap <- renderPlot({
    
    ggplot(combinedImprov %>% filter(PopName==input$state, Sex==input$sex) %>% dplyr::rename(Improve = Improvement), aes(Year, Age)) +
      geom_raster(aes(fill = Improve)) +
      scale_fill_gradientn(limits=c(-1, 1), colours = terrain.colors(10)) +
      ggtitle(paste(input$state, " Mortality Improvement by Age and Year (Green indicates improvement)", sep=""))
  })
  
  
  
  # Comparison of mortality improvement vs US aggregate
  
  output$improvement_comparison_heatmap <- renderPlot({
    
    us_Improv_compare <- us_5x5_t_g_Improv %>% filter(COD.cat=="All", Sex==input$sex)
    state_Improv_compare <- combinedImprov %>% filter(PopName==input$state, Sex==input$sex)
    us_Improv_compare <- us_Improv_compare %>% arrange(Age, Year)
    state_Improv_compare <- state_Improv_compare %>% arrange(Age, Year)
    diff_Improv_compare <- cbind(select(state_Improv_compare, Age, Year), state_Improv_compare$Improvement - us_Improv_compare$Improvement)
    names(diff_Improv_compare) <- c("Age", "Year", "Delta")
    diff_Improv_compare <- as.tibble(diff_Improv_compare)
    
    ggplot(diff_Improv_compare, aes(Year, Age)) +
      geom_raster(aes(fill = Delta)) +
      scale_fill_gradientn(limits=c(-1, 1), colours = terrain.colors(10)) +
      ggtitle(paste(input$state, " Improvement Minus US Aggregate Improvement (Green indicates better improvement)", sep=""))
    
  })
  
  
  output$mort_comparison_heatmap <- renderPlot({
    
    us_Mort_compare <- us_5x5_t_g %>% filter(COD.cat=="All", Sex==input$sex)
    state_Mort_compare <- combinedQ %>% filter(PopName==input$state, Sex==input$sex)
    diff_Mort_compare <- state_Mort_compare %>% full_join(us_Mort_compare, by=c("Age", "Year"))
    diff_Mort_compare <- diff_Mort_compare %>% mutate(Ratio=qx.x/qx.y)
    diff_Mort_compare <- as.tibble(diff_Mort_compare)
    
    ggplot(diff_Mort_compare %>% filter(Year != levels(diff_Mort_compare$Year)[1]), aes(Year, Age)) +
      geom_raster(aes(fill = Ratio)) +
      scale_fill_gradientn(limits=c(0, 2), colours = terrain.colors(10)) +
      ggtitle(paste(input$state, " Mortality / US Aggregate Mortality (Green indicates lower mortality)", sep=""))
    
  })
  
  
  output$mortality_trend_bygroup <- renderPlot({
    
    ggplot(us_5x1_t %>% filter(Group.Disease==input$disease, Sex==input$sex, Age %in% input$age), aes(Year, qx, colour=Age)) +
      geom_line() +
      ggtitle(paste("Probability of death, US, 1965 to 2014, for ", input$disease, sep="")) +
      ylab("Probability of Death in Year") +
      guides(colour = guide_legend(reverse=TRUE))
    
    
  })
  
}

##### END SERVER #####

# Run the application 
shinyApp(ui = ui, server = server)


#################################
##### END DYNAMIC PORTION ######
#################################


