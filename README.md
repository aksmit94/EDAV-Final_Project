## EDAV Final Project

Introduction
============

Questions/Motivation for choosing the topic
-------------------------------------------

<font size="4"> According to [Commonwealth
Fund](https://www.commonwealthfund.org/publications/issue-briefs/2015/oct/us-health-care-global-perspective)
, spendings for the Healthcare Industry in the US as a percentage of the
GDP is 32% higher than France. The gap can be illustrated in the
following graph. </font> <!-- ![](PercentGDP.png) -->
![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-3-1.png)

<font size="4"> Medicare also contributes a lot to this cost. According
to this [Forbes
article](https://www.forbes.com/sites/danmunro/2012/12/30/2012-the-year-in-healthcare-charts/#1d34e95f6c8c),
Annual per capita cost increases exponentially in the US as the age
increases.

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-4-1.png)

The graphs above motivated us to try and understand what is driving the
major costs. Are there some disease categories which are contributing a
lot to those costs? We wanted to understand the efficiency of the
healthcare industry. Looking at the 2nd graph motivated us to search for
the Medicare dataset and understand what are some good efficiency
metrics.

We were also interested in the mortality rate over time. This would
allow us to see whether improvements in healthcare technology have
decreased the overall mortality rate. The buckets of age for which
mortality rate decreased over time as the years increased also intrigued
us.

Another motivation for using the mortality data set is that we wanted to
understand whether the medicare spending makes sense according to the
Disease Categories. For example, is there a correlation between the
amount spent on a disease category( or paid out on average) with the
disease category having the highest death rate?

List of team members and a description of how each contributed to the
project:

1.  Asaf Gal

-   Worked on the shiny interactive heatmaps for the mortality datasets.
-   Created the analysis and the executive summary for the mortality
    data

1.  Ashwin Jayaraman +Worked on the analysis for the Medicare Dataset.
    Explored and created the visualizations for the same

2.  Raphael Kintzer

-   Worked on the analysis for the Medicare Dataset. Explored and
    created the visualizations for the same

1.  Akshat Mittal

-   Created the shiny interactive app for the Mortality and the Medicare
    Datasets.

Each member contributed to the final documentation by explaining their
parts.

Description of data
===================

Sources of the Data
-------------------

The medicare dataset was collected from the website of Centers for
Medicare and Medicaid Services (CMS). The links to the dataset can be
found
[here](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Inpatient.html).
There is a file associated with each year.

1.  [2011](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2011_CSV.zip)
2.  [2012](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2012_CSV.zip)
3.  [2013](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2013_CSV.zip)
4.  [2014](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2014_CSV.zip)
5.  [2015](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2015_CSV.zip)
6.  [2016](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Downloads/Inpatient_Data_2016_CSV.zip)

The Mortality dataset was initially can be obtained from
[here](https://cod.mortality.org/COD/USA/CODinputs/USAcod.csv). The link
requires a credential to access the dataset. We had to apply to get the
access (Username: "hmdcod" and Password: "magali").

While working on the dataset, it was found that the data was lacking in
some key areas. We emailed the creator of the dataset and she emailed us
2 new datasets that she will were recently published on the website, one
that includes the aggregate data for the US and other which includes
mortality rates by states:

1.  [Mortality by State, split by Age, Year, and
    Sex](https://usa.mortality.org/uploads/lifetables/lifetables.zip%20on%2012/7/2018.)

2.  [Mortality for US aggregate, split by Cause of Death, 5-year age
    groups, Year, and
    Sex](http://www.nber.org/data/vital-statistics-mortality-data-mulitiple-cause-of-death.html)

Tabular data was received from Magali Barbieri on 12/7/2018, originally
sourced from the National Bureau of Economic Research website .

General Description of Data
---------------------------

To best describe the data we will differentiate between the datasets and
then explain the relationship between them. The Medicare dataset
contains the following columns:

1.  DRG Definition: The code and description identifying the Medicare
    Severity Diagnosis Related Groups (MS-DRG). They are a
    classification system that groups similar clinical conditions
    (diagnoses) and the procedures furnished by the hospital during an
    inpatient stay
2.  Provider Id: The provider identifier assigned to the Medicare
    certified hospital facility.
3.  Provider Name: The name of the provider
4.  Provider Street Address: The provider’s street address
5.  Provider City: The city where the provider is located
6.  Provider State: The state where the provider is located
7.  Provider Zip Code: The provider’s zip code
8.  Provider HRR: The Hospital Referral Region (HRR) where the provider
    is located
9.  Total Discharges: The number of discharges billed by the provider
    for inpatient hospital services
10. Average Covered Charges: The provider's average charge for services
    covered by Medicare for all discharges in the DRG. These will vary
    from hospital to hospital because of differences in hospital charge
    structures
11. Average Total Payments: The average total payments to all providers
    for the MS-DRG including the MS- DRG amount, teaching,
    disproportionate share, capital, and outlier payments for all cases.
    Also included in average total payments are co-payment and
    deductible amounts that the patient is responsible for and any
    additional payments by third parties for coordination of benefits
12. Average Medicare Payments: The average amount that Medicare pays to
    the provider for Medicare's share of the MS-DRG. Average Medicare
    payment amounts include the MS-DRG amount, disproportionate share,
    capital, and outlier payments for all cases. Medicare payments do
    not include beneficiary co-payments and deductible amounts nor any
    additional payments from third parties for coordination of benefits.

The Mortality dataset:

State mortality data is available as a separate file for each state,
bucketed by 1 year, 5 years, and 10 years, for both age and year. Each
state's mortality is presented as an actuarial life table. This analysis
is only focused on the qx's (probability of death within that age
group). The mortality data is segmented by sex as either "male",
"female", or "both". Population data is not available on the state
level, so it is not possible to aggregate state-level mortality data to
infer US aggregate mortality. Cause of death data is not provided on the
state level.

US aggregate mortality data is available in 5yr age buckets, and 1yr
"year" buckets, for males, females, and both. Death counts are provided
along with the mortality rates, allowing for the population to be
calculated (pop = deaths / rate) Cause of death is also provided. The
coding was provided by Magali Barbieri along with an index. This was
based on the NBER coding based on ICD codes. There are 92 cause-of-death
codes, which is significantly consolidated from the NBER codes, and for
each code a Disease and Grouping is provided in the index. There is also
an "All" code for the total for all causes of death. The dataset also
includes columns showing the mortality for that cause of death as a
fraction of the total mortality. Due to the availability of population
data (inferred from the rates and death counts) and disease groupings,
it is possible to also infer mortality rates for entire disease
groupings. There are a total of 20 disease groupings, which is possibly
few enough to be able to visualize simultaneously. No state data is
provided in the dataset.

<!-- Mortality dataset prep -->
<!-- Converting the mortality dataset into a mortality *improvement* dataset -->
<!-- Reading in the 5x1 data (ie 5yr "age" buckets, 1yr "year" buckets). -->
<!-- Using the index to create a table of disease codes, groupings, and full categorization ([Grouping] - [Disease]) to go along with the numeric coding in the dataset. -->
<!-- Processing the US aggregate data (adding descriptive names to the diseases, calculating populations, removing unnecessary age groups) -->
<!-- Converting the mortality dataset into a mortality improvement dataset -->
    # Converting the mortality into mortality improvement by spreading out the mortality rates over all years, and taking the difference between years (qx_t / qx_t-1 - 1). Dropping the first year group because there are no mortality rates to compare it to (since it is the first).
    us_5x5_t_g_Improv <- us_5x5_t_g %>% spread(Year, qx)
    us_5x5_t_g_Improv[, 8:ncol(us_5x5_t_g_Improv)] <- 
      us_5x5_t_g_Improv[, 8:ncol(us_5x5_t_g_Improv)] / us_5x5_t_g_Improv[, 7:(ncol(us_5x5_t_g_Improv)-1)] - 1
    us_5x5_t_g_Improv <- us_5x5_t_g_Improv[,-7]
    us_5x5_t_g_Improv <- us_5x5_t_g_Improv %>% gather(Year, Improvement, -COD.cat, -Disease, -Group, -Group.Disease, -Sex, -Age)

Analysis of data quality
========================

Description of Data Quality
---------------------------

There were some missing values for the cost of providers in the data.
This can be visualized using the visna package:

    visna(data)

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-11-1.png)

We could see that there were 2 types of missing patterns in the medicare
dataset.

1.  The first pattern where all the column values were missing except
    for year. There were 4,147 such instances from the 1,080,374 rows.
    This contributed to 0.3% of the total data and all occured in the
    year 2013.
2.  The second pattern was where only the column Total Discharges was
    missing. These occurred in 41 rows in the dataset.

We decided to ignore these rows since their contribution to the overall
data was approximately 0.3%. As a result, our data for analysis
consisted of 1,076,186 rows.

Certain Diagnosis Related Groups only started appearing from the year
2014. There were 637 total Diagnosis Related Groups most of which were
pretty similar. For example, there were Diagnosis Related Groups like

1.  Heart Transplant Or Implant Of Heart Assist System W MCC
2.  Heart Transplant Or Implant Of Heart Assist System W/O MCC

While researching online, we found out that the difference between the
two was that one consisted of Heart Transplant on a person with Multiple
Chronic Conditions and the other consisted of a Heart Transplant on a
person without Multiple Chronic Conditions. These 2 DRGs were pretty
similar to us and we decided to map them according to the diagnosis
categories.

We mapped the DRGs to the Diagnosis Categories using the following
[source](https://en.wikipedia.org/wiki/Major_Diagnostic_Category).

While plotting the trend over time, we saw that CMS did not include
certain DRGs before the year 2013. This can be visualized in the graph
below, where each color is a different DRG:

    filtered_data <- data  %>% group_by(Diagnostic_category, Year) %>% 
      dplyr::summarize(Average_Cost = sum(Total_Payments)/sum(`Total Discharges`)) %>% mutate(count = n())

    ggplot(filtered_data, aes(x=Year, y=Average_Cost, color =Diagnostic_category )) + geom_line() +
      xlab('Year') + ylab('Average Cost') + ggtitle('Average Cost Trend by Diagnostic Category')

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-13-1.png)

For the state mortality dataset, there were no NAs or missing values.
The US aggregate dataset also had no NAs. However, for many age groups
there were 0 deaths, and many causes of death had no records prior to
certain years, such as HIV-AIDS prior to the 1980s. Essentially, this
means that deaths occurring due to HIV-AIDS prior to the 1980s were
either nonexistent or would have been coded as something other than
HIV-AIDS (such as “Other infections” or “Other unexplained”). However,
it is unlikely that any significant number of deaths are actually
“missing” from the dataset, since deaths data are usually unambiguous
(dead or not dead) and carefully recorded by government agencies.
Therefore, the data can be considered usable and representative.

Main analysis
=============

Findings
--------

We first decided to see how the costs varied by State in the US.
Cleaning the cost variables by removing the ‘$’ and the ‘,’ in the
dataset, we used the choropleth package to plot the same:

    state_choropleth(total_cost_by_state,title = "Average Cost by State",
                     legend = "Average Cost")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-15-1.png)

As expected, we noticed that California and New York have a high average
cost of treatment. This can be attributed to the high cost of living in
those states. On the other hand, Florida having such a low average cost
seems to indicate that the treatments are generally cheaper in that
state.

We then investigated the total number of discharges by state:

    total_cost_by_state$value<- total_cost_by_state$Total_Discharges
    state_choropleth(total_cost_by_state,title = "Total Discharges by State",
                     legend = "Total Discharges")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-16-1.png)

As we can see, the overall number of discharges in Florida is actually
quite high and are in the similar bucket to California and New York.

The map showing discharges also indicates the population density of the
US. States with high populous have a high number of discharges.

When we compared this choropleth graph of Total Discharges by State to
the previous graph Average Cost by State there were a few notable
things. First, excluding New York and California the graphs are almost
inversely related. The states with the highest number of discharges were
actually the states with lower average cost.

This comparison led us to explore further breakdowns within each DRG for
each state which could shed a little light as to why there is an inverse
relationship. Based on these two graphs we also wondered whether a
further breakdown of the DRG’s amongst the states could expose disparity
between the states for a given DRG. In short, these first two
choropleths showed us that the data did not follow the trends we would
expect and required us to dive deeper in to the differences between
states. This realization was the main motivation towards building our
interactive map.

Our next question was on which Diagnoses Related Groups are the most
expensive on average?

To accomplish the above task, we bucketed the DRGs by their general
categories based on codes provided by CMS. The graph can be seen below:

    ggplot(drg_cost, aes(x = reorder(Diagnostic_category,Average_Cost) ,y = Average_Cost)) + 
             geom_col() + scale_y_continuous(labels = function(l) { paste0(l/1000, "K")}) +
      coord_flip() + ylab('Average Cost') + xlab('DRG') + ggtitle('Average Cost by DRG')

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-18-1.png)

Some key takeaways from the above graph:

Heart and Transplant are significantly more expensive than all other
DRGs. There are many possibilities as to why this is occuring. One
possibility is that the procedure is simply more expensive because it
requires more resources (doctors, nurses, hours in hospital, medication,
surgery etc.) Another possibility could be that there are fewer cases
for these treatments and those treatments are significantly more
expensive and have an outsized effect on the category average.

This motivated us to see the similar graph for the number of discharges

    drg_discharge <- drg_grouped_by_year %>% group_by(`Diagnostic_category`) %>% 
      dplyr::summarise(Discharge = sum(Count_Discharge))

    ggplot(drg_discharge, aes(x = reorder(Diagnostic_category,Discharge) ,y = Discharge)) + 
             geom_col()  + scale_y_continuous(labels = function(l) { paste0(l/1000000, "M")}, breaks = pretty(drg_discharge$Discharge, n = 20)) +
      coord_flip() + ylab('Count') + xlab('DRG') + ggtitle('Number of Discharges by DRG')

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-19-1.png)

This graph does shed light on the our question from the previous graph
as to whether the DRG costs are biased by outliers if they have to few
datapoints in their category. We see that in general this is not the
case. The transplant data has 14,794 which is certainly enough to
dispell that theory.

The most basic derivation from this graph is that it shows which DRGs
medicare treats most. It makes sense for example that Pregnancy and
Childbirth are rarely paid for by medicare because in general only
people over 65 are eligible for medicare. Note that the 6,983 cases here
likely come from the small group of people who received social security
disability insurance checks for over 24 months and become eligible for
medicare.

We now want to see the relationship between the diagnoses related
group's percent discharges and percent cost. We thought that the
percentage cost should correlate with the percentage discharged.

    ggplot(tidy_form_of_data, aes(x = Percentage,
                                    y = fct_reorder2(Diagnostic_category, fct_reorder(Category,Percentage), -Percentage), color = Category)) +
       geom_point() + ylab("DRGs") + theme_dotplot + 
       ggtitle("DRGs by Percent Avg Cost & Percent of Discharges") + scale_color_manual(name="Category", 
                            labels = c("Percent Cost",
                                       "Percent Discharges"), 
                            values = c("Percent_Cost"="Blue", 
                                       "Percent_Discharges"="red"))

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-22-1.png)

If we look at the DRG Musculoskeletal System & Connective Tissue we see
that it accounts for approximately 12.59% of discharges while it also
accounts for over 17% of cost. This discrepancy could be due to a few
different factors. One possibility is that each treatment is expensive
compared to other treatments. This high cost could also be due to an
array of other factors.

From the above 3 graphs we wanted to dig deeper into the variation
within the states. We believed that there were some hospitals which were
charging more for the same diagnostic category within a particular state

We first filtered out the hospitals which did not have 50 discharges in
the 5 years. We looked at the state of California which had a higher
average cost and looked at the Diagnostic Category of Heart.

We then plotted the Hospitals which have the highest average cost

    ggplot(costly_providers, aes(x = Average_Cost, y = fct_reorder(`Provider Name`, Average_Cost))) +
      geom_point(color = "blue") + xlab("Average Cost") + ylab("") + 
      scale_x_continuous(labels = function(l) { paste0("$",l/1000, "K")}) +
      theme_dotplot + 
      ggtitle("Average Cost of Most expensive Providers")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-25-1.png)

We then plotted the Hospitals which have the lowest average cost

    ggplot(cheapest_providers, aes(x = Average_Cost, y = fct_reorder(`Provider Name`, Average_Cost))) +
      geom_point(color = "blue") + xlab("Average Cost") + ylab("") + 
      scale_x_continuous(labels = function(l) { paste0("$",l/1000, "K")}) +
      theme_dotplot + 
      ggtitle("Average Cost of Least expensive Providers")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-26-1.png)

There was a huge variation among the hospitals within the state of
California. The highest hospitals were charging upwards of 400K while
the lowest hospitals were charging less than 100K. Seeing such a high
variation motivated us to create the 2nd interactive component.

This would allow the user to play with the data and identify the most
expensive and least expensive hospitals by state for a diagnostic
category.

<!-- Asaf's mortality data findings -->
The mortality improvement can be presented as a heatmap.

    # Heatmap of US mortality (all deaths, all states)
    ggplot(us_5x5_t_g_Improv %>% filter(COD.cat=="All", Sex=="b"), aes(Year, Age)) +
      geom_raster(aes(fill = Improvement)) +
      scale_fill_gradientn(colours = terrain.colors(10)) +
      ggtitle("US Mortality Improvement")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-27-1.png)

The mortality heatmap comprises three effects: Time effect (mortality
rates improving at a given time, which can be seen as a vertical bar in
the heatmap), Age effect (mortality rates improving for a given age in
all years, which can be seen as a horizontal bar), and Cohort effect
(mortalty rates improving for a group born at a specific time,
continuing throughout its lifetime, which can be seen as an
upward-sloping diagonal line in the heatmap). A positive time effect can
be seen here in the late 70s and again in the late 90s. These may
possibly be due to spillover positive health effects due to a strong
economy. A positive age effect can be seen here for ages below 10, which
apparently had consistently good mortality improvement over time.
Finally, a small cohort effect can be seen for people born around 1990,
who had very strong mortality improvement for about two decades.

There are also specific incidents in the heatmap, which can be seen as
singular dots. The two negative incidents worth noting are 15-29 year
olds in 1965-1969, and 30-39 year olds in 1985-1994. The first negative
incident may be explained by activities tied to the "hippie movement" in
the 60s, primarily affecting teenagers and young adults at the time. The
second negative incident may be explained by the HIV-AIDS epidemic in
the late 80s and early 90s, mostly affecting young to middle-aged men,
but also other groups as well. New York seems to have experienced a
worse mortality impact at this time compared to the rest of the US
(particularly deep South states), as can be seen in the interactive
mortality plots of NY versus other states, and in the NY vs aggregate US
plot below.

Calculating the difference between a state's mortality improvement, and
the US aggregate mortality improvement, as an example.

    # Difference between State Improvement and US aggregate improvement
    us_Improv_compare <- us_5x5_t_g_Improv %>% filter(COD.cat=="All", Sex=="b")
    state_Improv_compare <- combinedImprov %>% filter(PopName=="NY", Sex=="b")
    us_Improv_compare <- us_Improv_compare %>% arrange(Age, Year)
    state_Improv_compare <- state_Improv_compare %>% arrange(Age, Year)
    diff_Improv_compare <- cbind(select(state_Improv_compare, Age, Year), state_Improv_compare$Improvement - us_Improv_compare$Improvement)
    names(diff_Improv_compare) <- c("Age", "Year", "Delta")
    diff_Improv_compare <- as.tibble(diff_Improv_compare)

    # Plotting the difference in mortality improvement as a heatmap (green indicates that the state had better improvement)
    ggplot(diff_Improv_compare, aes(Year, Age)) +
     geom_raster(aes(fill = Delta)) +
     scale_fill_gradientn(colours = terrain.colors(10)) +
     ggtitle(paste("NY Improvement Minus US Aggregate Improvement", sep=""))

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-28-1.png)
(Green indicates NY improved more than US avg)

Calculating the difference between a state's mortality rates, and the US
aggregate mortality rates, just as an example.

    # Difference between State mortality and US aggregate mortality
    us_Mort_compare <- us_5x5_t_g %>% filter(COD.cat=="All", Sex=="b")
    state_Mort_compare <- combinedQ %>% filter(PopName=="NY", Sex=="b")
    diff_Mort_compare <- state_Mort_compare %>% full_join(us_Mort_compare, by=c("Age", "Year"))
    diff_Mort_compare <- diff_Mort_compare %>% mutate(Ratio=qx.x/qx.y)
    diff_Mort_compare <- as.tibble(diff_Mort_compare)

    # Plotting the difference in mortality rates as a heatmap (green indicates that the state had better mortality)
    ggplot(diff_Mort_compare %>% filter(Year != levels(diff_Mort_compare$Year)[1]), aes(Year, Age)) +
       geom_raster(aes(fill = Ratio)) +
       scale_fill_gradientn(colours = terrain.colors(10)) +
       ggtitle(paste("NY Mortality / US Aggregate Mortality", sep=""))

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-29-1.png)
(Green indicates NY had lower mortality than US avg)

Calculating an average US mortality rate by year, regardless of age.
This will not necessarily show a decreasing trend, as demographics
change (particularly an aging population)

    # Calculate aggregate death counts and populations for years
    us_Allx1_t <- us %>% group_by(Year, COD.cat, Disease, Group, Group.Disease) %>% dplyr::summarize(
      Deaths.F.t = sum(Deaths.F, na.rm=T), 
      Deaths.M.t = sum(Deaths.M, na.rm=T), 
      Deaths.T.t = sum(Deaths.T, na.rm=T), 
      Pop.F.t = sum(Pop.F, na.rm=T), 
      Pop.M.t = sum(Pop.M, na.rm=T), 
      Pop.T.t = sum(Pop.T, na.rm=T)
    ) %>% as.tibble()

    # Calculate qx's
    us_Allx1_t <- us_Allx1_t %>% mutate(
      q_F = Deaths.F.t / Pop.F.t, 
      q_M = Deaths.M.t / Pop.M.t, 
      q_T = Deaths.T.t / Pop.T.t
      )

    # Gather qx's into m/f/b
    us_Allx1_t <- 
      us_Allx1_t %>% 
      select(Year, COD.cat, Disease, Group, Group.Disease, q_F, q_M, q_T) %>% 
      gather(Sex, q, -Year, -COD.cat, -Disease, -Group, -Group.Disease)

    # Rename qx's to "m"/"f"/"b"
    us_Allx1_t$Sex[us_Allx1_t$Sex=="q_F"] <- "f"
    us_Allx1_t$Sex[us_Allx1_t$Sex=="q_M"] <- "m"
    us_Allx1_t$Sex[us_Allx1_t$Sex=="q_T"] <- "b"
    us_Allx1_t$Sex <- as.factor(us_Allx1_t$Sex)

    # Plot of mortality improvement over time
    ggplot(us_Allx1_t %>% filter(COD.cat=="All"), aes(Year, q, colour=Sex)) +
      geom_line() +
      ggtitle("Probability of death (all ages), US, 1959 to 2016, Males and Females") +
      ylab("Probability of Death in Year")

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-30-1.png)

Repeating the same calculations, but this time splitting the mortality
rate by age groups. Now, the mortality rate should be decreasing over
time, since we are adjusting for age.

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

    # Plot of mortality improvement over time
    ggplot(us_5x1_t %>% filter(COD.cat=="All", Sex=="b", Age %in% c("55-59", "60-64", "65-69", "70-74", "75-79")), aes(Year, qx, colour=Age)) +
      geom_line() +
      ggtitle("Probability of death, US, 1959 to 2016, for Older Age Groups") +
      ylab("Probability of Death in Year") +
      guides(colour = guide_legend(reverse=TRUE))

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-31-1.png)

Executive summary
=================

There were three main findings that we derived from our analysis:

1.  When looking at the state map, we see that Alaska has a higher
    average cost compared to other states having a similar population.
    It’s median cost for most of the Diagnostic Categories are at the
    top.

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-32-1.png)

1.  California and New York have a high average cost and high number of
    discharges. They contribute to the majority of the cost. Reducing
    their costs by 5% will reduce the overall spend by

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-33-1.png)

1.  Going to treatment for Heart in Florida is more economical. [Flight
    tickets from anywhere](https://www.transtats.bts.gov/AIRFARES/) in
    the US to Miami Florida cost on average $317.71 and you would be
    saving on average an amount of about $20,000 (Average Cost of Heart
    Treatment Across US - Average Cost of Heart Treatment for Florida -
    Cost of Flight to Miami).

Bottom Line: Fly to Florida for Heart Treatment!

1.  Mortality Dataset Findings

What are the key drivers of mortality, and how have they changed over
time?

Repeating the same calculations as above, but this time calculating
mortality rates for each cause of death group. The mortality rates for
each cause of death will then be used to identify which causes of death
are most prominent.

Also, will be ignoring demographics again, since too many dimensions
will make it hard to visualize.

![](Health_of_US_Healthcare_files/figure-markdown_strict/unnamed-chunk-34-1.png)

Perinatal death decreased precipitously between 1960 and 2014. It is
also apparent that heart disease and cerebrovascular disease started
decreasing in importance beginning in the early 1990s. The 25-year trend
beginning around 1990 exactly reverses the increasing mortality from
those two causes of death in the prior 25-year period. The importance of
external causes as a cause of death did not change at all throughout the
years. Some diseases became slightly more important throughout the
years, such as Respiratory, Endocrine, and Digestive diseases, as well
as Malignant Neoplasms. Mental and behavioral disorders, and diseases of
the nervous system and sense organs, became more important.

Interactive component
=====================

DRGApp
------

[LINK](https://aksmit94.shinyapps.io/DRGApp/)

The aim of the interactive app is to make it easy to visualize, in
several dimensions (year, number of discharges, type of disease etc),
the scenario of healthcare costs in the US. Kindly note that this app is
best viewed in Chrome (version 70+) at 100% zoom. It consists of two
components:

1.  Visualizing Medicare Costs in the US:

-   This component was aimed at enabling the user to see how the
    healthcare costs have changed over the years in their state, overall
    as well as the average treatment costs for particular disease group.

-   The user is greeted with a choropleth of healthcare costs for the
    Medicare enrolled population averaged over 2011-2016 and all DRGs.
    Hovering over a state provides details of the value of average
    healthcare cost in that state as well as the total number of
    discharges in that state from 2011-2016.

-   Clicking on a particular state shows, in the side-panel, a line
    graph of trend of average healthcare costs over the years for that
    state. One remarkable fact evident from this graph is that the
    overall average healthcare cost shot up sharply in 2014 irrespective
    of the state

-   The app also provides user the feature to view the average costs for
    one or more disease categories (eg. Heart, Lungs etc.) over all the
    states. A second level of filter can be applied by choosing one or
    more diseases in the chosen disease category.

1.  Find my Provider:

-   This component enables the user to see the most expensive and least
    expensive treatment providers in their state. The user can make a
    selection among all US states and the type of disease they are
    interested to check the providers for.

-   This app would help make an informed decision in choosing a
    healthcare provider by the expected expenditure for treatment.

MortalityLightApp
-----------------

[LINK](https://aksmit94.shinyapps.io/MortalityLightApp/)

Given a state (any of the 50 US states plus DC) and sex (either “male”
or “female”, or both sexes combined), This app shows three heatmaps.
Each heatmap is dimensioned on year and age, and shows one of the
following: 1. The first heatmap shows the chosen state’s and sex’s
individual mortality improvement. Green indicates that the mortality for
that age group has decreased (improved) from the prior year, reflected
as a percentage of prior period’s mortality. 2. The second heatmap shows
the mortality improvement for the state/sex compared to the US total
average for that sex. Green indicates that the state/sex’s mortality
improvement for that age group and year is lower (better) than the
corresponding group under the US total average, reflected as a delta
(difference) versus the US average. 3. The third heatmap shows the
state/sex’s mortality rates over the corresponding US average for that
group. Green indicates that the mortality rate for that state/sex is
lower (better) than the corresponding US average for that group,
reflected as a ratio (1 meaning the mortality rate is the same as the US
average, and &lt;1 meaning the state’s mortality is lower).

This series of plots shows how a state is performing in terms of
mortality. It shows how it is improving compared to itself, how it is
improving compared to other states, and where it currently stands in
terms of its mortality vs other states. It also shows particular trends
in the data, particularly time effects, age effects, and cohort effects.
It also shows noteworthy mortality incidents in the state. Splitting by
sex allows further detail into what is driving those effects, for
example if a particular mortality deterioration is mostly driven by
worse mortality for men.

New York generally seems to be a good indication of what is happening to
national trends. In general, the mortality patterns in New York are the
same as the national averages, but are more exaggerated. (An exception
to this is the negative mortality incident in the 60s, which seems to be
no worse in New York than it was in the national average). However, the
positive time effects in the late 70s and late 90s are present, as is
the cohort effect for people born in the 80s.

It is interesting to compare coastal states to southern and midwestern
states. In particular, the states in the deep South such as Mississippi
and Alabama did not experience the mortality improvement that other
states did starting in the 80s for people being born in that decade.
Therefore, while the national average has shown substantial mortality
improvement for young people born in the last few decades compared to
prior generations, the southern states generally had unchanged mortality
for those same people, causing their mortality to be on the average
significantly worse than their non-Southern peers.

MortalityHeavyApp
-----------------

[LINK](https://aksmit94.shinyapps.io/MortalityHeavyApp/)

This app is identical to the Light App, but includes an additional
graphic showing the mortality trend over time for a particular set of
age groups, and cause of death. This helps show the underlying driver of
mortality improvement or deterioration in the above heatmaps. For
example, it is easy to see that the spike in HIV-AIDS deaths among
middle-aged men occurred at the same time and to the same sex and age
group as the heatmaps show a negative mortality event. This can be seen
by setting the State to “NY”, Sex to “m”, Age Groups to “35-39”, and
Cause of Death to “Certain Infectious Diseases - HIV-AIDS”.

The additional user inputs in the Heavy App are the desired Age Groups
and the Cause of Death (which feed only into the fourth visualization;
the Cause of Death is not an input into the state-level heatmaps because
Cause of Death data was not provided with the state datasets). The Cause
of Death is aggregated to a disease grouping, rather than exact cause of
death even though this is how the data was provided, because this allows
more data points to be weighed together, relieving some of the
statistical noise and avoiding issues where there is insufficient data.
This also makes the app easier to use, as there are only 20 disease
groupings but 92 specific causes of death. We believe the decision to
aggregate up to disease grouping is theoretically justified because we
assume there is some correlation across specific causes of death within
each disease grouping.

The output is simply a line chart showing mortality rate over time for
the desired age groups, sex, and cause of death grouping.

Conclusion
==========

Limitations
-----------

1.  The medicare dataset only looks at the Medicare population which is
    defined for populated aged 65 and above. This does not include the
    costs for the other age groups.

2.  We do not have the historical trends for the cost. The dataset
    starts from the year 2011. Having a historical perspective would
    allow us to see in which year did the percentage of increase in the
    costs increase the most.

3.  The Medicare dataset is also limited to the cost for the Inpatient
    Hospital Admissions. We do not have the data for the other types of
    services, mainly - The outpatient costs which includes the office
    visits and other annual wellness visits We no not have the
    Laboratory Costs and the Pharmaceutical Costs.

4.  We have an overall summary of the costs for each provider. This does
    not include the Patient Information, which is confidential. However,
    having a unique ID for the patients could allow us to see the
    quality of the providers. Having a high readmission rate after
    surgery could indicate that the surgeries were not effective and
    contributes a lot to the overall cost.

5.  The mortality dataset includes the overall numbers and does not
    provide the provider information. If we had that, we can combine the
    2 datasets to also judge the effectiveness of a provider. </font>

Appendix
========

[GITHUB](https://github.com/aksmit94/EDAV-Final_Project)
