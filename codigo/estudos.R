soma_pais_grafico <- table(euevgera$pais)
soma_pais_grafico[5] <- sum(soma_pais_grafico)
names(soma_pais_grafico) <- c("Brasil", "Espanha", "Mexico", "Portugal", "Total Geral")
soma_pais_grafico
#     Brasil     Espanha      Mexico    Portugal Total Geral 
#        982         268         138          23        1411 

pct_pais <- paste(round(unname(soma_pais_grafico) / (unname(soma_pais_grafico[5])) * 100), "%")
pct_pais
#[1] "70 %" "19 %" "10 %" "2 %"

ggplot(data = euevgera, aes(pais)) +
  geom_bar(fill = "orange") +
  labs(x = "Origem dos dados", y = "Quantidade") +
  annotate(geom = "text", x = c(1:5), y = c(1010, 295, 175, 55, 1410), label = as.vector(soma_pais_grafico)) 

graph.pais <- barplot(as.vector(soma_pais_grafico), 
                     xlab = "Origem dos dados", 
                     ylab = "CEUEV Questionários Aplicados",
                     col = "orange",
                     ylim = c(0,max(soma_pais_grafico) + 200),
                     names.arg = names(soma_pais_grafico))
text(x = graph.pais, y = soma_pais_grafico, label = unname(soma_pais_grafico), cex=1,pos=3)
axis(1, at=graph.pais, labels=pct_pais, tick=F, las=1, line=-2.0, cex=0.5)

# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow"),
    labels = paste(names(casos_genero), "-", pct_genero))

# Gráfico do tipo barra
graph.genero <- barplot(casos_genero, 
                        xlab = "Gênero dos respondentes", 
                        ylab = "Quantidade",
                        col = "orange",
                        ylim = c(0,max(casos_genero) + 100))
text(x = graph.genero, y = casos_genero, label = unname(casos_genero), cex=1, pos=3)
axis(1, at=graph.genero, labels=pct_genero, tick=F, las=1, line=-6.0, cex= 3.0)

# Gráfico do tipo pizza no ggplot2
#Gráfico de pizza sem rótulos
data_genero = data.frame(genero=euevgera$genero)

pie = ggplot(data_genero, aes(x="", fill=genero)) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(legend.position = "none",
        panel.grid.major = element_line(color="grey60"),
        panel.border=element_blank())
pie

#Porcentagem de cada categoria
feminino = round((sum(data_genero$genero=="feminino")/length(data_genero$genero))*100)
feminino = paste("(",feminino,"%)",sep="")
masculino = round((sum(data_genero$genero=="masculino")/length(data_genero$genero))*100)
masculino = paste("(",masculino,"%)",sep="")
nao_repondeu = round((sum(data_genero$genero=="")/length(data_genero$genero))*100)
nao_repondeu =paste("(",nao_repondeu,"%)",sep="")  
 
#
if (!require(grid)) {
  install.packages(("grid"))
  library(grid)
}

#Adicionar rótulos com % no gráfico de pizza
grid.text(paste("Masculino",masculino,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("Feminino",feminino,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("NR/NS" ,nao_repondeu,sep=" "), x=unit(0.52, "npc"), y=unit(0.32, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)

casos_sexo <-
  for (i in 1:nrow(euevgera)) {
    euevgera$grupo[i] <- if(euevgera$genero[i] == "feminino") "Feminino" else "Masculino"
  }

ggplot(data = euevgera, aes(x = "", y=genero, fill = grupo)) +  
  geom_col() +
  #  geom_col(color = "black") +
  coord_polar(theta = "y")

