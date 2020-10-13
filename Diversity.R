## Data Visualization (GOV 16 - QSS17) Fall 2017
## Lab 2
##
## Name: James Wen
## Date: October 25, 2017 

# Inital Settings  --------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(ggthemes)
library(maps)
library(ggmap)

# Load Data ---------------------------------------------------------------
# used data for 2015-16 school year 
df.2015 <- read.csv("data/CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv", na.strings = c("NULL", "NA")) %>% 
  mutate(percent_white = UGDS_WHITE * 100) %>% 
  select(INSTNM, LATITUDE, LONGITUDE, percent_white)

df.2015.adjusted <- na.omit(df.2015) # remove observations completely if percent_white == NA

# organize data for Californian map
states <- map_data("state")   
california <- subset(states, region %in% c("california"))    

# organize data for putting a county map on the Californian map 
county <- map_data("county")   # obtain map data for counties 
california_counites <- subset(county, region == "california")   # obtain data for Ca counties 

# Make Figure -------------------------------------------------------------

ggplot() + 
  geom_polygon(data = california,                     # make a map of California first 
               mapping = aes(x = long, y = lat),
               color = "black", fill = "gray") +
  geom_polygon(data = california_counites,            # overlay a map of the counties onto a CA map 
               aes(x = long, y = lat, group = group), 
               fill = NA, color = "white") + 
  coord_fixed(xlim = c(-122.75, -121.75),             # adjust coordinates to focus on the Bay Area 
              ylim = c(37.25, 38.25),
              ratio = 1.2) +
  geom_point(data = df.2015.adjusted,                 # overlay college data ontop of map 
             aes(x = LONGITUDE, y = LATITUDE, color = percent_white), 
             size = 1, na.rm = TRUE) +
  theme_few() +
  scale_y_continuous(breaks = FALSE) + 
  scale_x_continuous(breaks = FALSE) +
  labs(
    title = "Diversity in Bay Area Colleges",
    subtitle = "Data Source: College Scorecard Data",
    x = " ",
    y = " ",
    color = "Percent White")
ggsave("figure/Lab2.pdf", scale = 0.8, width = 12, height = 8) 
