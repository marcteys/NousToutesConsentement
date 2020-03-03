

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

rowPerc(xtabs(~ Genre + PressionSexe, data))

str(factor(header[1,"PressionSexe"]), max.levels=0)




library(ggplot2)
tmp = data.frame()
tmp = xtabs(~ Genre + PressionSexe, dataFemmesHommes)
tmp = as.data.frame(tmp) 

tmpPercent = data.frame()

tmpPercent = rowPerc(xtabs(~ Genre + PressionSexe, dataFemmesHommes))
tmpPercent = as.data.frame(tmpPercent) 

tmpPercent = head(tmpPercent,-2)


tmp = arrange(tmp, -row_number()) # reverse row orders to display yes and now first
tmpPercent = arrange(tmpPercent, -row_number()) # reverse row orders to display yes and now first
tmpPercent
tmp$PressionSexe <- factor(tmp$PressionSexe, levels = rev(levels(tmp$PressionSexe)))
tmpPercent$PressionSexe <- factor(tmpPercent$PressionSexe, levels = rev(levels(tmpPercent$PressionSexe))) # Reverse factors
#df_transpose = t(tmp)




str(factor(header[1,"PressionSexe"]), max.levels=0)



mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

plottitle = c(as.character(header[1,"PressionSexe"]))

plot <- ggplot(data = tmpPercent, aes(x = 2, y = Freq, fill = PressionSexe)) + 
  geom_bar(width = 1, stat = "identity", color ="white", position = position_fill()) +
  geom_text(aes(label = Freq), position = position_fill(vjust = 0.5), color ="white") +
  coord_polar(theta = "y") +
  facet_wrap(~Genre )  +
  scale_fill_manual(values = mycols) +
  theme_void() +  
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  theme(legend.position='right') + 
 xlim(0.5, 2.5) +
  labs(fill  = "", title = plottitle) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme(legend.key.height=unit(1, "cm")) 
 #+  theme(plot.title = element_text(size=1000/nchar(plottitle), vjust=10))
plot 






GetInfoParGenre(data, "PressionSexe")



GetInfoParGenre(data, "SexePenetrationEveille")


library(tigerstats)



GetInfoParGenre <- function(data,column = "") {
 
  
  result <- rowPerc(xtabs(~ Genre + eval(parse(text = column)), data))
  
  print(factor(header[1,column]), max.levels=0)
  
  result
  
  return(result)
}





