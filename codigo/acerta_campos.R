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

}