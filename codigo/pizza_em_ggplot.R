# Um exemplo gráfico pizza no ggplot2!
# Fonte: https://stackoverflow.com/questions/20443467/place-labels-on-pie-chart
#Reproducible example
party = c("None","Republicans","Republicans","Independents","Democrats",
          "Democrats","Republicans","Republicans","Independents","Independents",
          "Democrats","Democrats","Republicans","Democrats","Democrats",
          "Independents","Democrats","Democrats","Republicans","None")

data_party = data.frame(party=party)

#Pie chart without labels
pie = ggplot(data_party, aes(x="", fill=factor(party))) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_line(color="grey60"),
        panel.border=element_blank())
pie

#Percentage of each category
Democrats = (sum(data_party$party=="Democrats")/length(data_party$party))*100
Democrats = paste("(",Democrats,"%)",sep="")

Republicans = (sum(data_party$party=="Republicans")/length(data_party$party))*100
Republicans = paste("(",Republicans,"%)",sep="")

Independents = (sum(data_party$party=="Independents")/length(data_party$party))*100
Independents = paste("(",Independents,"%)",sep="")

None = (sum(data_party$party=="None")/length(data_party$party))*100
None = paste("(",None,"%)",sep="")

#Add labels with % in pie chart
if (!require(gridExtra)) {
  install.packages(("gridExtra"))
  library(gridExtra)
}
library(grid)

grid.text(paste("Democrats"   ,Democrats   ,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=14, col="black"), rot = 00)
grid.text(paste("Republicans" ,Republicans ,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=14, col="black"), rot = 00)
grid.text(paste("Independents",Independents,sep=" "), x=unit(0.52, "npc"), y=unit(0.27, "npc"), gp=gpar(fontsize=14, col="black"), rot = 00)
grid.text(paste("None"        ,None        ,sep=" "), x=unit(0.35, "npc"), y=unit(0.38, "npc"), gp=gpar(fontsize=14, col="black"), rot = 36)
