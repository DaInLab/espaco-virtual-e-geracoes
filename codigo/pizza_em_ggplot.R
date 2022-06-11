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


# Versão 2
library(ggplot2)
library(dplyr)

piramide = structure(list(`Faixa etária` = c("60 a 69", "70 a 79", "80 a 89","90 ou mais", "60 a 69", "70 a 79", "80 a 89", "90 ou mais"),
                          sexo = c("Feminino", "Feminino", "Feminino", "Feminino", "Masculino", "Masculino", "Masculino", "Masculino"), 
                          pop = c(401425,242451, 118671, 36191, 288951, 150313, 57404, 11262)), 
                     row.names = c(NA, -8L),
                     class = c("tbl_df", "tbl", "data.frame"))

abs_virgula <- function (x) {
  format(abs(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

piramide %>%
  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) %>%
  ggplot(mapping = aes(x = `Faixa etária`,
                       y = ifelse(sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs_virgula, limits = (max(piramide$pop))* c(-1,1)) +
  labs(y = "População", x = "Faixa etária (em anos)") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  theme(legend.position = "bottom") +
  coord_flip()

# Só faixa etária?
piramide %>%
#  mutate(sexo = factor(sexo, levels = c("Masculino", "Feminino"))) %>%
#  ggplot(mapping = aes(x = `Faixa etária`,
 #                      y = ifelse(sexo == "Feminino",  yes = pop, no = -pop), fill = sexo)) +
  ggplot(mapping = aes(x = `Faixa etária`, y=pop)) +
   geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs_virgula, limits = (max(piramide$pop))* c(-1,1)) +
  labs(y = "População", x = "Faixa etária (em anos)") +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  theme(legend.position = "bottom") +
  coord_flip()





