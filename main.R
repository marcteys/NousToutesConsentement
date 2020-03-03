data = read.csv("datasimple.csv")  # Lire le fichier csv 
data 


dataHommes <- subset(data, Genre=="Un homme") 
dataFemmes <- subset(data, Genre=="Une femme") 



