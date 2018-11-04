# Remove all objects from R
rm(list = ls())

# Set working directory
setwd("D:/Edwisor assignments")

#create Data Frame consisit of Student list and the COurse they are taking
student_data <- data.frame(
  id = c (1:5), 
  name = c("Rick","Dan","Michelle","Ryan","Gary"),
  COurse = c("Data Science","Full Stack","Mean Stack","ML Expert","DataSCience")
)

student_data

# Install Mongilite package and Library


install.packages("mongolite")
library(mongolite)

# Connect to the DB= Edwisor and Collection= Ediwor_assign
c= mongo(collection = "edwisor_assign",db="edwisor")

#Insert datframe student_data into Mongo DB
c$insert(student_data)
