# Análise Exploratória dos Dados
# Author: João Pedro Albino
# 
# Importando os dados para o ambiente R
# Criando o dataframe de trabalho denominado "euevgera"
# ----
# Problema na importação do formato da variável "data.ano" em arquivos ".csv" por meio de read.table()!
# 05/11/2018 => 005-11-20??
# Solução: escrever uma função que receba uma string e a converte em uma data usando o formato desejado utilizando 
# “setAs” para defini-la como um método depois utilizá-la como função como parte das colClasses em read.table(). 
# Fonte: https://stackoverflow.com/questions/13022299/specify-custom-date-format-for-colclasses-argument-in-read-table-read-csv
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )

euevgera <- read.table("./dados/dados_minerados.csv", 
                       header = TRUE, sep = ";", as.is = T,
                       colClasses = c("character", "myDate", "character", "factor", "factor", "character"))
# Totais de casos/observações
total_casos <- nrow(euevgera)
total_casos
#[1] 1411

# Variáveis importadas
nome_variaeis <- names(euevgera)
nome_variaeis
#[1] "pesquisador" "data.ano"    "pais"        "genero"      "idade"       "estilo"

# Os dados nesta análise foram compilados dos trabalhos desenvolvidos por pesquisadores 
# que utilizaram o questionário CEUEV (ANEXO 4) desenvolvidos em Barros (2008) e em Alonso e Gallego (2008),
# referentes a quatro países, Brasil, Espanha, México e Portugal.
# Os casos selecionados e analisados totalizam 1411 casos ou observções e foram consideradas as seguintes vaviáveis:
# "pesquisador", "data.ano", "pais", "genero", "idade" e "estilo".
# Fontes dos dados utilizadas:
# Pesquisadores                       | País      | Trabalho de origem
# María Carmen López Berlanga         | Espanha   | El estilo de uso del espacio virtual con estudiantes de Educación Secundaria - Berlanga, M. C. L., et. al, artigo, 2018.
# Alejandro Villegas Barrera          | México    | Herramienta para la creación de un Entorno Personal de Aprendizaje - Barrera, A. V., et. al., reporte, 2017.  
# Marcos Andrei Ota                   | Brasil    | Adaptatividade em Ambientes Virtuais: uma proposta para personalizar a aprendizagem em cursos híbridos de ensino superior - Ota, M. A., tese, 2018. 
# Adriana Aparecida de Lima Terçariol | Brasil    | Os Estilos de Uso dos Espaços Virtuais e as Redes Sociais na Pedagogia: Um Estudo Exploratório - Terçariol, A.A.L., e Barros, D, M., 2017.
# Indefinido 1                        | Brasil    | ???
# Indefinido 2                        | Portugal  | ???
# Cristina Sánchez Romero             | Espanha   | Estudio de Identificación de Los Estilos Del Uso Del Virtual de Los Seniors: perspectivas iniciales - Romero, C. S., et al., 2020.

# ------------------------------------------------
# Gráfico 1: Quantidade de casos por país
# concatenando os dados
casos_pais <- table(euevgera$pais)
casos_pais
#Brasil  Espanha   Mexico Portugal 
#   982      268      138       23 
# Acertando os campos para o Gráfico 1: adicionando coluna "Total Geral"
# Utilizando o barplot
soma_pais_grafico <- table(euevgera$pais)
soma_pais_grafico[5] <- sum(soma_pais_grafico)
names(soma_pais_grafico) <- c("Brasil", "Espanha", "Mexico", "Portugal", "Total Geral")
soma_pais_grafico
#     Brasil     Espanha      Mexico    Portugal Total Geral 
#        982         268         138          23        1411 
pct_pais <- paste(round(unname(soma_pais_grafico) / (unname(soma_pais_grafico[5])) * 100), "%")
pct_pais
#[1] "70 %"  "19 %"  "10 %"  "2 %"   "100 %"

# Utilizando barplot
graph.pais <- barplot(as.vector(soma_pais_grafico), 
                      xlab = "Origem dos dados", 
                      ylab = "CEUEV Questionários Aplicados",
                      col = "orange",
                      ylim = c(0,max(soma_pais_grafico) + 139),
                      names.arg = names(soma_pais_grafico))
text(x = graph.pais, y = soma_pais_grafico, label = unname(soma_pais_grafico), cex=1,pos=3)
axis(1, at=graph.pais, labels=pct_pais, tick=F, las=1, line=-2.0, cex=0.5)

# utilizando o ggplot2
library(ggplot2)
ggplot(data = euevgera, aes(pais)) +
  geom_bar(fill = "orange") +
  labs(x = "Origem dos dados", y = "Quantidade") +
  annotate(geom = "text", x = c(1:4), y = c(1010, 295, 175, 55), label = unname(casos_pais)) +
  facet_wrap(vars(sum(casos_pais)))

# ------------------------------------------------
#Gráfico 2: Quantidade de respondentes por sexo
casos_genero <- table(euevgera$genero)
casos_genero
#     feminino masculino 
#511       553       347

# Acertando os casos "em branco" ou "não respondeu" como "NS/NR"
names(casos_genero) <- c("NS/NR", "Feminino", "Masculino")
casos_genero
#    NS/NR  Feminino Masculino 
#      511       553       347
pct_genero <- paste(round(unname(casos_genero) / sum(unname(casos_genero)) * 100), "%")
pct_genero
#[1] "36 %" "39 %" "25 %"

# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow"),
    labels = paste(names(casos_genero), "-", pct_genero))

# Gráfico do tipo pizza no ggplot2
#Gráfico de pizza sem rótulos
data_genero = data.frame(genero=euevgera$genero)

pie = ggplot(data_genero, aes(x="", fill=genero)) + 
  geom_bar(width=1) +
  coord_polar(theta="y",start=0) +
  geom_text(aes(y="",label="")) +
  xlab("") + ylab("Respondentes por gênero") +
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

#Adicione rótulos com % no gráfico de pizza
if (!require(grid)) {
  install.packages(("grid"))
  library(grid)
}

grid.text(paste("Masculino",masculino,sep=" "), x=unit(0.62, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("Feminino",feminino,sep=" "), x=unit(0.40, "npc"), y=unit(0.66, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)
grid.text(paste("NR/NS" ,nao_repondeu,sep=" "), x=unit(0.52, "npc"), y=unit(0.32, "npc"), gp=gpar(fontsize=12, col="black"), rot = 00)

# Gráfico do tipo barra
graph.genero <- barplot(casos_genero, 
                      xlab = "Gênero dos respondentes", 
                      ylab = "Quantidade",
                      col = "orange",
                      ylim = c(0,max(casos_genero) + 100))
text(x = graph.genero, y = casos_genero, label = unname(casos_genero), cex=1, pos=3)
axis(1, at=graph.genero, labels=pct_genero, tick=F, las=1, line=-5.0, cex.axis= 1.1)

# ------------------------------------------------
# Gráfico 3: Quantidade de respondentes por Estilo de Uso do espaço virtual
casos_genero <- table(euevgera$genero)
casos_genero
#     feminino masculino 
#511       553       347

# Acertando os casos "em branco" ou "não respondeu" como "NS/NR"
names(casos_genero) <- c("NS/NR", "Feminino", "Masculino")
casos_genero
#    NS/NR  Feminino Masculino 
#      511       553       347
pct_genero <- paste(round(unname(casos_genero) / sum(unname(casos_genero)) * 100), "%")
pct_genero
#[1] "36 %" "39 %" "25 %"

# Gráfico tipo "pizza"
pie(casos_genero,
    edges = 200, radius = 0.8,
    clockwise = F,
    density = NULL, angle = 90, col = c("red", "orange", "yellow"),
    labels = paste(names(casos_genero), "-", pct_genero))



