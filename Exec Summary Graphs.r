########Exec summary graph 1
```{r fig.height=10, fig.width=10, warning=FALSE, messages=FALSE}
library(plotly)

data_na_removed <- as.tibble(read_csv("Inpatient_Cost.csv"))

data_na_removed <- data_na_removed[,-1]

data_na_removed$`DRG Definition` <- gsub('(.*?)- ', '',data_na_removed$`DRG Definition`)

drg_grouped_by_year <-  data_na_removed %>% group_by(Diagnostic_category, `DRG Definition`, `Year`,`Provider State`) %>%
  summarise(Count_Discharge = sum(`Total Discharges`),Sum_Amount = sum(`Total_Payments`))

drg_grouped_by_year  <- drg_grouped_by_year  %>% group_by(Year) %>% mutate(Percent_Discharges = 100 * Count_Discharge/sum(Count_Discharge),
                                                                           Percent_Cost = 100 * Sum_Amount/sum(Sum_Amount))


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
```
######################


########Exec summary graph 2
```{r fig.height=10, fig.width=10, warning=FALSE, messages=FALSE}
library(scales)

data <- read_csv('Inpatient_Cost.csv')


filtered_data <- data %>% filter(sum(`Total Discharges`) > 50) %>% group_by(`Provider State`, `Provider Name`) %>% 
  summarise(Average_Cost = sum(Total_Payments)/sum(`Total Discharges`)) 

median_for_state <- filtered_data %>% group_by(`Provider State`) %>% summarise(median_val = median(Average_Cost))

costly_states<-head(median_for_state[order(-median_for_state$median_val),], 5)

filtered_data %>% inner_join(costly_states) %>% ggplot(aes(x = reorder(`Provider State`, Average_Cost, median),y = Average_Cost)) + geom_boxplot() + coord_flip() + 
  theme_grey(20) + xlab("State") + ylab("Average Cost in USD") + ggtitle("") + scale_y_continuous(label=dollar_format())
````
