#conduct K-means cluster analysis

#install packages
install.packages('tidyverse')
install.packages('cluster')
install.packages('fpc')
install.packages('factoextra')
install.packages('janitor')

#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(factoextra)
library(janitor)

#set working directory
setwd("c/Users/kspen/OneDrive/Documents")

#read in datafile
empdf <- read.csv("Employees.csv")
view(empdf)

#create a dataframe using the assignment variables 
quantdf <- empdf[c(1,16,20,25)]

#create a dataframe using the assignment variables by removing the unneeded columns
#quantdf <- empdf[c(-2, -3, -4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-17,-18,-19,-21,-22,-23,-24,-26,-27,-28)]

#normalize each variable
quantdfn <-scale(quantdf)
view(quantdfn)

#set random number seed
set.seed(42)

#create a function to calculate total wss deviation to use for elbow plot
wss <- function(k){kmeans(quantdfn, k, nstart = 10)}$tot.withinss

#range of k values for elbow plot
k_values <- 1:10

#run function to create range of values for elbow plot
wss_values <- map_dbl(k_values, wss)

#create a new dataframe containing both k_values and wss values
elbowdf <- data.frame(k_values, wss_values)

#graph the elbow plot
ggplot(elbowdf, mapping=aes(x=k_values, y=wss_values))+
  geom_line()+geom_point()

#run k_means clustering with 4 clusters (k=4) and 1000 random restarts
k4 <- kmeans(quantdfn, 4, nstart = 1000)

#display the structure of the 4 means clustering object
str(k4)

#display information on 4 means clustering
k4

#display cluster statistics
cluster.stats(dist(quantdfn, method="euclidean"), k4$cluster)

#combining each observation's cluster assignment with unscaled dataframe
quantdfk4 <- cbind(quantdf, clusterID=k4$cluster)
view(quantdfk4)

#calculate variable averages for all non-normalized observations
summarize_all(quantdf, mean)

#calculate variable averages for each cluster
quantdfk4%>%
  group_by(clusterID)%>%
  summarize_all(mean)

