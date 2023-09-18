###title: "Visualization-corn data with weather information"
### Loading required packages

library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
library(tinytex)


### Reading data and removing NA's 
#note: corn_R.csv is the data with weather infornmation
#reading data and see the summary of data before change
corn <- read.csv("corn_R.csv")
summary(corn)
#removing the rows with NA's- using str() and summary() to look at the data summary
cornc <- na.omit(corn)
summary(cornc)
str(cornc)


### Changing year to factor. working with cornc from now on
#changing year to factor. working with cornc from now on 
cornc$year <- as.factor(cornc$year)
str(cornc)


### Plotting for vom
#plots: years by vom
vom <-
  ggplot(cornc,
         aes(x = year, y = vom)) +
  geom_boxplot(fill="#e0cb5e")+ 
  labs(title="Corn Deoxynivalenol (DON) Mycotoxin in Ontario", 
       subtitle="years (2011-2022)",
       x="Year",
       y="DON concentration (ppm)")
vom

### Plotting temp periods /years 
# plots: years by percip for the chosen periods 
p1 <-
  ggplot(cornc,
         aes(x = year, y = total_precip_July_3)) +
  geom_boxplot(fill="plum")+ 
  labs(title="July week 3 percipitation", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Total Percipitaion (mm)")
p1 +scale_y_continuous(limits = c(0, 20))

p2 <-
  ggplot(cornc,
         aes(x = year, y = total_precip_July_4)) +
  geom_boxplot(fill="lightblue")+ 
  labs(title="July week 4 percipitation", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Total Percipitaion (mm)")
p2 +scale_y_continuous(limits = c(0, 20))

p3 <-
  ggplot(cornc,
         aes(x = year, y = total_precip_Aug_1)) +
  geom_boxplot(fill="#f98ea0")+ 
  labs(title="Aug week 1 percipitation", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Total Percipitaion (mm)")
p3 +scale_y_continuous(limits = c(0, 20))

p4 <-
  ggplot(cornc,
         aes(x = year, y = total_precip_Aug_2)) +
  geom_boxplot(fill="lightgreen")+ 
  labs(title="Aug week 2 percipitation", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Total Percipitaion (mm)")
p4 +scale_y_continuous(limits = c(0, 20))

### Plotting percip periods /years 
# plots: years by temp for the chosen periods
p5 <-
  ggplot(cornc,
         aes(x = year, y = mean_temp_July_3)) +
  geom_boxplot(fill="plum")+ 
  labs(title="July week 3 mean temperature", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Temperature (°C)")
p5 +scale_y_continuous(limits = c(15, 30))

p6 <-
  ggplot(cornc,
         aes(x = year, y = mean_temp_July_4)) +
  geom_boxplot(fill="lightblue")+ 
  labs(title="July week 4 mean temperature", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Temperature (°C)")
p6 +scale_y_continuous(limits = c(15, 30))

p7 <-
  ggplot(cornc,
         aes(x = year, y = mean_temp_Aug_1)) +
  geom_boxplot(fill="#f98ea0")+ 
  labs(title="Aug week 1 mean temperature", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Temperature (°C)")
p7 +scale_y_continuous(limits = c(15, 30))

p8 <-
  ggplot(cornc,
         aes(x = year, y = mean_temp_Aug_2)) +
  geom_boxplot(fill="lightgreen")+ 
  labs(title="Aug week 2 mean temperature", 
       subtitle="years (2011-2022)",
       x="Year",
       y="Temperature (°C)")
p8 +scale_y_continuous(limits = c(15, 30))


### Plotting data on dual maps (vom and temp/or percip)

#### Loading required packages for plotting and reading the data
library(tidyverse)
library(sf)
library(mapview)
library(RColorBrewer)
library(ggplot2)
library(tigris)
library(dplyr)
library(leaflet.extras2)
library(leaflet)
library(raster)
library(rasterVis)
library(lattice)


#### Subset a year for percip (change layer name for desired year or period) & you can use the breaks for the desired ranges
#### Example: 2018
#there will be warnings regarding the color spectrum which are OK :)
#year 2018
yr <- subset(cornc, year == "2018")
#breaks <- c(0, 5, 10, 15, 20) # manual breaks
Percip <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4269,
  grid = FALSE,
  zcol = "total_precip_Aug_1",#at=breaks,
  col.regions = brewer.pal(9, "Greens"),
  layer.name = "Total Percipitation(mm)-Aug 1_2018")


yr <- subset(cornc, year == "2018")
#breaks <- c(15, 17, 20, 23, 25, 27, 30) # manual breaks
Temp <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4269,
  grid = FALSE,
  zcol = "mean_temp_Aug_1",#at=breaks,
  col.regions = brewer.pal(9, "Oranges"),
  layer.name = "Temperature(°C)-Aug 1_2018")

breaks <- c(0, 0.5, 1.0, 2.0, 5, 44) # manual breaks
Vom <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4269,
  grid = FALSE,
  zcol = "vom",
  layer.name = "Mycotoxin 2018",
  at=breaks
)
Percip|Vom
Temp|Vom

#### Example: 2022
#subset a year for percip (change layer name for desired year or period) & you can use the breaks for the desired range
#there will be warnings regarding the color spectrum which are OK :)
# year 2022
yr <- subset(cornc, year == "2022")
#breaks <- c(0, 5, 10, 15, 20) # manual breaks
Percip <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4269, 
  grid = TRUE,
  zcol = "total_precip_July_3",#at=breaks,
  col.regions = brewer.pal(9, "Greens"),
  layer.name = "Total Percipitation(mm)-July 3_2022")

#breaks <- c(15, 17, 20, 23, 25, 27, 30) # manual breaks
Temp <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4326,
  grid = FALSE,
  zcol = "mean_temp_July_3",#at=breaks,
  col.regions = brewer.pal(9, "Oranges"),
  layer.name = "Temperature(°C)-July 3_2022")

breaks <- c(0, 0.5, 1.0, 2.0, 5, 44) # manual breaks
Vom <- mapview(
  yr,
  xcol = "longitude",
  ycol = "latitude",
  crs = 4269,
  grid = FALSE,
  zcol = "vom",
  layer.name = "Mycotoxin 2022",
  at=breaks
)
Percip|Vom
Temp|Vom