#install packages
install.packages("tidyverse")
install.packages("cluster")
install.packages("fpc")
install.packages("factoextra")
install.packages("janitor")

#load libraries
library(tidyverse)
library(cluster)
library(fpc)
library(factoextra)
library(janitor)

#set working directory
setwd("C:/Users/kspen/OneDrive/Documents")

#read in datafile
empdf<-read.csv("employees.csv")
View(empdf)

#Create a data frame with only quant variables - Age, MonthlyIncome, PercentSalaryHike,
#YearsAtCompany
quantdf<-empdf[c(1, 16, 20, 25)]
View(quantdf)

#calculate distance between each pair of observations using euclidean distance
euc_dist<-dist(quantdf, method="euclidean")

#run hierarchical clustering with the hclust function and Ward's method
euc_ward<-hclust(euc_dist, method="ward.D2")

#plot the dendrogram
plot(euc_ward)

#Create 4 clusters using the cutree function
euc_ward_4<-cutree(euc_ward, k=4)

#display vector of cluster assignments for each observation
euc_ward_4

#visualize clusters on the dendrogram
rect.hclust(euc_ward, k=4, border=2:4)

#link cluster assignments to original quant data frame
hcl4df<-cbind(quantdf, clusterID=euc_ward_4)

#write data frame to CSV file to analyze in Excel
write.csv(hcl4df, "employees_hcl.csv")

#display number of observations in each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize(n())

#Calculate variable averages for each cluster
hcl4df %>%
  group_by(clusterID) %>%
  summarize_all(mean)

#calculate variable averages for all non-normalized observations
summarize_all(hcl4df, mean)


