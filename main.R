
#  Dependances 
library(tigerstats)

library(stringr)
library(ggplot2)
library(gridExtra)
install.packages("egg") 
library("egg")
install.packages("devtools")
library(devtools)


# Charger le document principal

data = read.csv("datasimple.csv", header = TRUE, encoding  = "UTF-8")  # Lire le fichier csv 
names(data)[1] <- "Genre"# Fix erreur de la première column



## Split par genre

dataFemmes <- subset(data, Genre=="Femme") 
dataHommes <- subset(data, Genre=="Homme"  ) 

dataFemmesHommes <- subset(data, Genre=="Homme" | Genre=="Femme" ) 
# dataFemmesHommes <- rbind(dataFemmes,dataHommes) # ça fonctionne aussi
dataFemmesHommes <- droplevels(dataFemmesHommes)

# Charger le header 
header = read.csv("header.csv", header = TRUE, encoding  = "utf-8")  # Lire le fichier csv 
header

#### Stats 











## FONCTIONS A CHARGER

wrap_strings <- function(vector_of_strings,width){sapply(vector_of_strings,FUN=function(x){paste(strwrap(x,width=width), collapse="\n")})} 


AfficherGraphGenre <- function(data,columnName = "", type = "percent", reverse = TRUE, removeEmpty = FALSE) {
 
  # Clear
  if(removeEmpty)
    data = data[-which(data[,c(columnName)] == ""), ]
  
  tmp = data.frame()
   # Regarder si c'est vide 
  tmp = xtabs(~ Genre + data[, c(columnName)], data)
  tmp = as.data.frame(tmp) 
  names(tmp)[2] <- columnName# Fix erreur de la première column
  # Remove cleared rows
  if(removeEmpty)
  tmp = tmp[-which(tmp$Freq == "0"), ] #clear empty things
  
  tmpPercent = data.frame()
  
  tmpPercent = rowPerc(xtabs(~ Genre + data[, c(columnName)], data)  )
  tmpPercent = as.data.frame(tmpPercent) 
  names(tmpPercent)[2] <- columnName# Fix erreur de la première column
  print(tmpPercent)
  if(removeEmpty)
  tmpPercent = tmpPercent[-which(tmpPercent$Freq == "0"), ] #clear empty things
  
  tmpPercent = head(tmpPercent,-2) # on enleve les total
  tmp = head(tmp,-2)
  
  if(reverse) {
  
  tmp = arrange(tmp, -row_number()) # reverse row orders to display yes and now first
  tmpPercent = arrange(tmpPercent, -row_number()) # reverse row orders to display yes and now first
  
  
  tmp[, c(columnName)] <- factor(tmp[, c(columnName)], levels = rev(levels(tmp[, c(columnName)])) )
  tmpPercent[, c(columnName)] <- factor(tmpPercent[, c(columnName)], levels = rev(levels(tmpPercent[, c(columnName)] ))) # Reverse factors
  #df_transpose = t(tmp)
  }
  
  #tmpPercent = head(tmpPercent,-4) # On eneleve les je ne sais pas 
  #tmp = head(tmp,-4)
  
  
  mycols <- c("#a132b8FF","#d9aee2ff", "#f3e4f6FF","#f4a5c1ff" , "#f3c2b4ff", "#f0f1f2ff","#a132b8FF","#d9aee2ff", "#f3e4f6FF","#f4a5c1ff" , "#f3c2b4ff", "#f0f1f2ff")
  textCol <- c("black", "black", "white","black","black", "black","black", "black", "white","black","black", "black")
  plottitle = factor(header[1,columnName])
  plottitle = str_replace(plottitle, "\n", "\n")
  plottitle
  
  
  plot <- ggplot(data = tmpPercent, aes(x = 2, y = Freq, fill = tmpPercent[,c(columnName)] )) + 
    geom_bar(width = 1, stat = "identity", color ="white", position = position_fill()) +
    geom_text(aes(label = round(Freq)), position = position_fill(vjust = 0.5), color ="black",size=4) +
    coord_polar(theta = "y") +
    facet_wrap(~Genre,strip.position="bottom")  +
    scale_fill_manual(values = mycols) +
    theme_void() +  
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    theme(legend.position='right') + 
    xlim(0.5, 2.5) +
    labs(fill  = "", title = wrap_strings(plottitle, 50)) +
    # theme(plot.title = element_text(hjust=0.5)) + 
    theme(legend.key.height=unit(1, "cm")) +
    theme(legend.key.size = unit(0.5, "cm")) +
    theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
    theme(strip.text = element_text(size=15) ) +  
    theme( legend.margin = margin(5, 50, 5, 50) ) +
    #theme(plot.title = element_text(size=30/nchar(plottitle), vjust=-200))+
    theme(plot.title = element_text(size=13, hjust = 0.5))
  
  
  return(plot)
}

p1 = AfficherGraphGenre(dataFemmesHommes, "SexeHorsPenetrationSommeil", reverse = TRUE)
p2 = AfficherGraphGenre(dataFemmesHommes, "SexeHorsPenetrationSommeilPartenaire", reverse = FALSE, removeEmpty = TRUE)

ggarrange(p1, p2, ncol = 1) %>%
ggexport(filename = "test.png") 

