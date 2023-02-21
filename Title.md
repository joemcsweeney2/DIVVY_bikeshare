---
 title: "Divvy bikeshare 2022 trip data"
 output: 
   html_document:
    df_print: paged
 
 ---

%% [markdown]
# The purpose of this notebook is to analyze trip data collected from Chicago's DIVVY bikeshare program for the calendar year 2022
  
# This analysis is tasked with the problem of answering the following questions:
1. how do DIVVY Annual Members and Casual Members differ in their use of the service
2. How can we use social media to convince Casual Members to become Annual Members

# Data source description
##### The data sources used in this analysis consist of a series of 12 spreadsheet files downloaded from Motivate Inc. Each files contains one month of data from the calendar year 2022. This raw data is securely stored in Google Cloud Services and on my local hard drive.
 
# Data verification
## Reliable:
Data comes from the city of Chicago’s database via Bikeshare. The data was collected by Motivate Inc. This is reliable data from a government source.
Data provided is sufficient to answer the question of this analysis. “How do members and casual users differ in their use of the program
## Original:
Data is 1st hand data collected by the city through Motivate Inc.,  as mentioned above
## Comprehensive:
The data appears complete and accurate for use in this project
The tables contain many null and empty  values, but these are not in critical areas of use for this analysis and will be removed to ensure the integrity of the analysis.
## Current:
Data used in this analysis ranges from January 2022 through December 2022 and is considered current and useful. This data will be used to guide marketing objectives for 2023 and forward.
## Cited:
Data comes from the city of Chicago’s DIVVY system.
It is made available by Motivate International Inc. for public use.
The license agreement can be viewed [here](https://ride.divvybikes.com/data-license-agreement).

# Data Cleaning
### Loading libraries to be used in data processing and cleaning
```{r}
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(forcats)
library(hms)
```

### Creating a list of the paths for the individual monthly files /kaggle/input/divvy-bikeshare-2022-trip-data/202201-divvy-tripdata.csv
```{r}
options(max.print = 150)
batch = data.frame(filename = list.files(path = "/kaggle/input/divvy-bikeshare-2022-trip-data", pattern = '*.csv', full.names = TRUE))
batch
```
```{r}
raw.file.path = batch %>%
  mutate(filepath = paste0("/kaggle/input/divvy-bikeshare-2022-trip-data"), filename)
```
```{r}
raw.file.path
```
```{r}
head(raw.file.path, n=1)
```

### Importing the data
```{r}
ldf = lapply(raw.file.path$filename, read.csv)
```
### Shortening the names of the files
```{r}
names(ldf) = substr(batch, 49, 54)
```
#### looking at the January 2022 data
```{r}
head(ldf$'202201', n = 10)
```
### Creating summaries for the the dataframes
#### For display purposes I'm only showing 1 summary. All of them have some NA's in end_lat and end_lng.
```{r}
data_sumrz = lapply(ldf, summary)
head(data_sumrz, n = 1)
```
###  We'll batch process these out.
### We'll show all summaries her to check that all NA's have been removed
```{r}
ldf_no_null <- lapply(ldf, function(x) x[complete.cases(x), ])

for (i in ldf_no_null) {
  data.frame(summary(i))
             
  print(summary(head(i)))
}
```
### Merging the dataframestrips = bind_rows(ldf_no_null). The length info confirms the successful merging of the data frames.
```{r}
trips = bind_rows(ldf_no_null)
```
#### The lengths confirm the merging was succesfull
```{r}
d_sum = summary(trips)
d_sum
```
# ### Checking for any duplicated rows. All data frames are 0X13 indicating no duplicates are present.
```{r}
chk = sum(duplicated(trips))
```
#### Confirming there are no duplicated rows
```{r}
chk
```
### Started_at & ended_at are in the wrong class. We'll fix that, changing them both to datetime.
```{r}
trips$started_at = as.POSIXct(trips$started_at)
trips$ended_at = as.POSIXct(trips$ended_at)
```
#### Confirming the change
```{r}
print(c(class(trips$started_at), typeof(trips$started_at), class(trips$ended_at), typeof(trips$ended_at)))
```
### Some of the start_station_name entries are blank.
```{r}
head(filter(trips, start_station_name == ""), n = 10)
```
```{r}                      
count(filter(trips, start_station_name == ""))
```
### There are quite a few, but I will drop these incomplete data points. They will make certain calculations difficult
```{r}
trips = subset(filter(trips, !start_station_name == ""))
```
#### Confirming no blank values exist.
```{r}
count(filter(trips, start_station_name == ""))
```
### We will not need the end station columns for the analysys, so they will be dropped.
```{r}
trips = subset(trips, select = -c(end_station_name, end_station_id))
```
### Examining the rideable types
```{r}
trips %>%   
  group_by(rideable_type) %>%   
  summarise(total = n_distinct(ride_id))
```
#### No information could be found on the DIVVY website regarding docked bikes & they are onla a small percentage of the data so the type will be dropped from the analysis
```{r}
trips = subset(filter(trips, !rideable_type  == "docked_bike"))
```
####  FALSE indicates no more docked bikes appear in the data
```{r}
count(trips, rideable_type  == "docked_bike")
```
# Data Manipilation
### Arranging data in chronilogical order
```{r}
trips = trips %>% 
  arrange(started_at)
head(trips, n=10)
```
### Adding a month, day of month and day of week column
```{r}
trips$month = month(trips$started_at, label = TRUE)
trips$day_of_month = day(trips$started_at)
trips$day_of_week = wday(trips$started_at, label = TRUE)
head(trips, n=10)
```
### Stripping date from datetime and adding it as time_of_day column
```{r}
trips= trips %>% 
  mutate(trips, time_of_day = update(ymd_hms(trips$started_at), year = 0, month = 1, mday = 1))
head(trips, n=10)
```
### Adding a column for ride duration
```{r}
trips$ride_dur =  as_hms(difftime(trips$ended_at, trips$started_at, 'mins'))
```
```{r}                     
head(trips)
```
### Creating a table for station popularity
```{r}
station_popularity_cl = trips

station_popularity_cl = station_popularity_cl %>% 
  group_by(start_station_name, member_casual) %>%
  summarise(popularity = n_distinct(ride_id))
```
```{r}
head(station_popularity_cl, n=25)
```
### Filtering out stations with < 50 rides
```{r}
station_popularity_cl = filter(station_popularity_cl, popularity > 50)%>%
  arrange(popularity)

head(station_popularity_cl, n=25)
```
### Creating a dataframe for station location information.
```{r}
locations = trips
locations = subset(locations, select = c(start_station_name, start_station_id, start_lat, start_lng))
```
```{r}         
locations
```
#### There are many different small variations in the latitude and longituge data. I'll grab the first distinct set of values for each station and assign them to all occurences of the stations. This will make it possible to create a map of the stations.
```{r}
locations = distinct(locations, start_station_name, .keep_all = TRUE)
```
```{r}         
locations
```
### Creating at table for average ride duration by member type.
```{r}
ARL = trips  
ARL = ARL %>% 
  group_by(member_casual) %>% 
  summarise(avg_dur = mean(ride_dur))

ARL
```
#### Converting the output to H:M:S
```{r}
ARL$avg_dur = as_hms(ARL$avg_dur)

ARL
```
# Exporting the data
```{r}
write.csv(trips, "/kaggle/working/trips_cl.csv", row.names = FALSE)
write.csv(locations, "/kaggle/working/locations.csv", row.names = FALSE)
write.csv(station_popularity_cl, "/kaggle/working/station_popularity_cl.csv", row.names = FALSE)
write.csv(ARL, "/kaggle/working/ARL.csv", row.names = FALSE)
```
# Visualizations rendered in Tableau
## [Link to Tableau Workbook](https://public.tableau.com/app/profile/joseph.aloysius.mcsweeney/viz/CAPSTONEPROJECTDIVVYBikeshareChicago/Story1)
```{r}
magick::image_read('/kaggle/input/visuals/pie.PNG')
```
### We can see here that casual members make up roughly 1/3 of all rides.
```{r}
magick::image_read('/kaggle/input/visuals/month.PNG')
```
### Ride volume appears to be seasonal due to Chicago's weather. The orange portion of the bars represents Annual Members and the blue, Casual Members. 
#### It is interesting to note that in Fall and Winter months the Annual Members account for a much higher percentage of rides than Casual Members. In the Winter months alone Annual Members make up over 80% of rides.
```{r}
magick::image_read('/kaggle/input/visuals/day.PNG')
```
### If we look at the data for ride volume by day of the week we can see that Annual Members represent a higher percentage of the rides, whereas the weekends are about even.
### It appears as though Annual Members may be using DIVVY to commute to and from work.
```{r}
magick::image_read('/kaggle/input/visuals/tod.PNG')
```
### The data here seems to support this idea of Annual Members as commuters. You can see that between 5 a.m. and 9 a.m. riders are predominantly Annual Members. We see this pattern again between 4 p.m. and 6 p.m.
```{r}
magick::image_read('/kaggle/input/visuals/arl.PNG')
```
### We see here that Casual Member's rides average around 8:30 minutes longer than that of Annual Members. This might indicate Annual Members riding with more of a sense of purpose. They simply need to get from point A to point B, i.e. work or running errands. Casual Members seem to ride more casually.
```{r}
magick::image_read('/kaggle/input/visuals/arl_month.PNG')
```
### Annual Member ride durations tend to be less affected by seasonality. They only vary about three minutes in length between any two months of the year. This is to be expected for commuters as work is a year-round endeavour. 
### It varies greatly for Casual Members though with roughly a ten minute differenct between the longest and shortest average ride time. Casual Members aren't commuters so if they don't have to ride in the cold, they don't.
```{r}
magick::image_read('/kaggle/input/visuals/arl_day.PNG')
```
### When we examine the data at a slightly more granular level, average ride duration by the day of the week, we see a distinct pattern emerge. For Annual Members riding Monday to Friday, there is almost no variation in the duration of their rides. This would lead you to believe they make the same ride every day. This goes to further support the idea of them as commuters.
### Casual Members also display a distinct pattern of behavior. Their numbers fan out increasingly the further they get away from mid-week. They start out on Sunday and reaching their lowest level on Wednesday, then after 'Hump-day' they ramp back up at around the same rate. It looks as if the work-week affects their riding. They ride a bit on Monday, but by the time Wednesday rools around they don't have the time. After Wednesday though, they seem to see the weekend coming up and begin to ride more frequently.
```{r}
magick::image_read('/kaggle/input/visuals/map.PNG')
```
### Here we're looking at the popularity of DIVVY stations. The more popular stations are represented by larger pie graph denoting the physical location of the station on the map. The individual pie charts represent the the percentage of rides by Annual Members in orange, while the Casual Members are represented by the blue slices.
### You can quickly see a fairly apparent pattern here. Stations that lie along the shoreline are the most popular stations. The vast majority of the rides originating from them are by Casual Members. Casual Members are using the DIVVY service for pleasure and sight-seeing.
```{r}
magick::image_read('/kaggle/input/visuals/map2.PNG')
```
### I've singled out a few of these stations here.
## Insights gained from the analysis
### Annual Members
* Are commuters going to and from work or running errands
* Their rides are shorter and more focused
* They ride mor during the work-week
#
### Casual Members
* Ride for leisure
* Ride more frequently around tourist destinations
* Ride more frequently on and around the weekend

## How do Casual Members use the DIVVY bikeshare service differently from Annual Members
### Casual members don't use DIVVY as a main form of transportation. They use it more leisurely. They take many trips along the shoreline, taking in the sights.

## Answering the the main question of the analysis: How can we convince Casual Members to convert to Annual Members.
## Three suggestions:
**1.  Start a campaign on social media touting the benefits of commuting via Divvy:**
* **Appeal to their wallet**. Save money at the pump
* **Appeal to their environmental conscientiousness**. Reduce your carbon footprint
* **Appeal to their desire for personal wellness**. Get in shape
 
**2. Incentivise conversion:**
* **Offer a small discount** for converting to Annual Membership
* **Utilize the partnership with Lyft** by offering discounts on Lyft rides to Annual Members
* **Capitalize on strategic alliances**. The DIVVY app shows maps for public transportation. Partner with the city to offer discounts for Annual Members who take advantage of these services

**3. Start a grassroots campaign:**
* **Take advantage of the weekend days**, which are more popular with Casual Members
* **Get boots on the ground**. Have DIVVY team members at popular stations along the shore on the weekend telling Casual Members about the benefits of going Annual
* **Have DIVVY team members offer to help** Casual Members convert to Annual Membership


/kaggle/input/visuals/arl.PNG



