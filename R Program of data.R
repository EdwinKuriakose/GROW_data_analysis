library(leaflet)
library(tidyverse)
library(sf)
library(mapview)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(maps)
library(dbscan)
library(spc)
library(fpc)
library(factoextra)

# install.packages("devtools")
# devtools::install_github("kassambara/factoextra")


#==============================================================================================
#                                    Loading the data
#==============================================================================================

#for smooth run of the code, set the working directory to the location where the GROW data file
#is saved.

#Loading the dataset from the csv file, and sorting it as a proper table
dataset <- read.csv('GrowLocations2.csv')

#==============================================================================================
#                                    Data cleaning up
#==============================================================================================

#Get a structured information about the data
summary(dataset)

#removing all the duplicate sensor values 
data_clean <- distinct(dataset, Serial, .keep_all = TRUE)

#trimming serial of the sensor, and begin and end date as well, with just dates.
data_clean$Serial <- strtrim(data_clean$Serial, 19)
data_clean$BeginTime <- strtrim(data_clean$BeginTime, 10)
data_clean$EndTime <- strtrim(data_clean$EndTime, 10)

#trimming latitude and longitude data to 1 decimal point
data_clean$Latitude <- sprintf(data_clean$Latitude, fmt = '%#.1f')
data_clean$Longitude <- sprintf(data_clean$Longitude, fmt = '%#.1f')


#adding a new column, which shows for how long the sensir worked, by subtracting end date 
#from start date
data_clean$Functioning_days <- difftime(data_clean$EndTime, data_clean$BeginTime, units = "days")
data_clean$Functioning_days <- as.numeric(as.character(data_clean$Functioning_days))
summary(data_clean$Functioning_days)

#removing columns which are not needed
data_clean <- subset(data_clean, select= -c(Type, SensorType, Code))

#Due to the error in lat and long data, swapping the column names to the correct ones
data_clean <- data_clean %>% rename(Long = Latitude,
                                    Lat = Longitude)

#The latitude and Longitude values are saved as char type, so needs to be converted to 
#numeric data
data_clean$Lat <- as.numeric(as.character(data_clean$Lat))
data_clean$Long <- as.numeric(as.character(data_clean$Long))


#Creating 2 more data frames
ll_data <- subset(data_clean, select = -c(Serial ,BeginTime, EndTime, Functioning_days ))
str(ll_data)

data_ext <- subset(data_clean, select = c(Serial, Long, Lat))
#write.csv(data_ext, "Ext Data.csv")
str(data_ext)

scaled_data <- scale(ll_data) 

#==============================================================================================
#                                    Plotting the data
#==============================================================================================

#Plotting the functioning days data
plot(x = data_clean$Functioning_days, 
     main = "Sensor functioning days",
     ylab = "Number of Days",
     type = "p",
     col="black"
     )


#Plotting the latitude and longitude data
locations_sf <- sf::st_as_sf(ll_data, coords = c("Long", "Lat"), crs = 4326)
mapview::mapview(locations_sf, col.regions = 'cyan' )


#==============================================================================================
#                                   Data Clustering
#==============================================================================================


#==================================================================
#             Using K-Means
#==================================================================

#First, determine the optimal number of clusters using the function fviz_nbclust and using the 
#Elbow method
fviz_nbclust(scaled_data, kmeans, method = "wss") + labs(subtitle = "Using the Elbow Method")

#On to calculating the K-Means
set.seed(123)
km <- kmeans(scaled_data, 3, nstart = 10000)

km$cluster
km$centers
km$size

#Combining the cluster data with the actual data 
dd <- cbind(scaled_data, cluster = km$cluster)
head(dd)

#Cluster plot
autoplot(km, scaled_data, frame = TRUE)


#==================================================================
#                                   Using DBSCAN
#==================================================================

#First, finding the eps value 
e <- dbscan::kNNdistplot(ll_data, k = 10)

#after checking the plot and determining the value, use dbscan function for clustering
d <- dbscan::dbscan(ll_data, eps = 0.55, MinPts = 375)

#Plotting the clusters
factoextra::fviz_cluster(d, ll_data, geom = "point", ellipse = TRUE, ellipse.type = "euclid")

#==============================================================================================
#     To be run in the end, to clear all packages, plots, console and workspace
#==============================================================================================

# To Clear workspace
rm(list = ls())

# To Clear packages
p_unload(psych, GPArotation)

# Clear plots
dev.off()

# Clear console
cat("\014")  # ctrl+L