data = read.csv("data.csv")  # read csv file 
data 

names(data)[1] <- "Gender"
names(data)[2] <- "Age"


dataHommes <- subset(data, Gender=="Un homme") 
