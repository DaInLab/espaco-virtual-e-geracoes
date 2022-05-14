df_alex$conta_A <- NULL
df_alex$conta_B <- NULL
df_alex$conta_C <- NULL 
df_alex$conta_D <- NULL
df_alex$estilo_uso <- NULL


if (qual_maior_contagem == 1) df_alex$estilo_uso[i] = "Estilo de Uso A - Uso Participativo no Espaço Virtual"
if (qual_maior_contagem == 2) df_alex$estilo_uso[i] = "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"
if (qual_maior_contagem == 3) df_alex$estilo_uso[i] = "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"
if (qual_maior_contagem == 4) df_alex$estilo_uso[i] = "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"
if (qual_maior_contagem > 4 | qual_maior_contagem = 0) df_alex$estilo_uso[i] = "Não definido!"

if (k == 2) df_alex[i, 50] = paste(df_alex[i, 50], "e Estilo de Uso B - Busca e Pesquisa no Espaço Virtual")
if (k == 3) df_alex[i, 50] = paste(df_alex[i, 50], "e Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual") 
if (k == 4) df_alex[i, 50] = paste(df_alex[i, 50], "e Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual") 

qual_maior_contagem <- which.max(c(dfesp$conta_A[i], dfesp$conta_B[i], dfesp$conta_C[i], dfesp$conta_D[i]))
if (qual_maior_contagem == 1) dfesp$estilo_uso[i] = "Estilo de Uso A - Uso Participativo no Espaço Virtual"
if (qual_maior_contagem == 2) dfesp$estilo_uso[i] = "Estilo de Uso B - Busca e Pesquisa no Espaço Virtual"
if (qual_maior_contagem == 3) dfesp$estilo_uso[i] = "Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual"
if (qual_maior_contagem == 4) dfesp$estilo_uso[i] = "Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual"
if (qual_maior_contagem > 4 | qual_maior_contagem < 1) dfesp$estilo_uso[i] = "Não definido!"

if (qual_maior_contagem < 4) {
  # Acertando os indices
  outra = c(46, 47, 48, 49)
  # procurando a segunda ocorrência    
  for (k in (qual_maior_contagem + 1):4){
    if (dfesp[i, k + 46] == dfesp[i, outra[qual_maior_contagem]]) {
      if (k == 2) dfesp[i, 50] = paste(dfesp[i, 50], "e Estilo de Uso B - Busca e Pesquisa no Espaço Virtual")
      if (k == 3) dfesp[i, 50] = paste(dfesp[i, 50], "e Estilo de Uso C - Estruturação e Planejamento no Espaço Virtual") 
      if (k == 4) dfesp[i, 50] = paste(dfesp[i, 50], "e Estilo de Uso D - Ação Concreta e Produção no Espaço Virtual") 
    }
  }
}

library(readxl)
df_seniors <- read_excel("dados/seniors_2020.xlsx", 
                           +     sheet = "uned")
View(df_seniors) 


set.seed(1234)

respostas = sample(1:nrow(df_seniors), nrow(df_seniors), 
                   prob = c(
                     rep(ind_prob[1], ind_resp[1]), rep(ind_prob[2], ind_resp[2]), rep(ind_prob[3], ind_resp[3]), rep(ind_prob[4], ind_resp[4]), rep(ind_prob[5], ind_resp[5]),
                     rep(ind_prob[6], ind_resp[6]), rep(ind_prob[7], ind_resp[7]), rep(ind_prob[8], ind_resp[8]), rep(ind_prob[9], ind_resp[9]), rep(ind_prob[10], ind_resp[10]),
                     
                     rep(ind_prob[11], ind_resp[11]), rep(ind_prob[12], ind_resp[12]), rep(ind_prob[13], ind_resp[13]), rep(ind_prob[14], ind_resp[14]), rep(ind_prob[15], ind_resp[15]),
                     rep(ind_prob[16], ind_resp[16]), rep(ind_prob[17], ind_resp[17]), rep(ind_prob[18], ind_resp[18]), rep(ind_prob[19], ind_resp[19]), rep(ind_prob[20], ind_resp[20]),
                     
                     rep(ind_prob[21], ind_resp[21]), rep(ind_prob[22], ind_resp[22]), rep(ind_prob[23], ind_resp[23]), rep(ind_prob[24], ind_resp[24]), rep(ind_prob[25], ind_resp[25]),
                     rep(ind_prob[26], ind_resp[26]), rep(ind_prob[27], ind_resp[27]), rep(ind_prob[28], ind_resp[28]), rep(ind_prob[29], ind_resp[29]), rep(ind_prob[30], ind_resp[30]),
                     
                     rep(ind_prob[31], ind_resp[31]), rep(ind_prob[32], ind_resp[32]), rep(ind_prob[33], ind_resp[33]), rep(ind_prob[34], ind_resp[34]), rep(ind_prob[35], ind_resp[35]),
                     rep(ind_prob[36], ind_resp[36]), rep(ind_prob[37], ind_resp[37]), rep(ind_prob[38], ind_resp[38]), rep(ind_prob[39], ind_resp[39]), rep(ind_prob[40], ind_resp[40])), 
                   replace=F)
respostas

