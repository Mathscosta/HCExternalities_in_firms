library(readxl)
library(data.table)
library(dplyr)
library(stringr)
library(stringi)
library(haven)
library(readxl)
library(ggplot2)
library(tidyr)



rais_pes.file <- list.files("//storage6/bases/DADOS/RESTRITO/RAIS/csv", pattern = 'brasil20', full.names = T) %>% 
  grep(pattern = '\\.csv', value = T)
rais_pes.file <- rais_pes.file[8:17]

rais_pes <- data.table()

n = -1

selec_vinc <- data.table('tipo_vinculo' = c(10,15,30,31), 'I' = 1)

for(i in rais_pes.file){
  
  ano_rais <- substr(i,48,51) %>% as.numeric
  rais_ano <- fread(i, nrows = n)
  

  
  # Seleciona somente os vínculos padrão de trabalhadores privado e público: CLT e Estatutário. Exclui temporários.  
  rais_ano <- selec_vinc[rais_ano, on = 'tipo_vinculo'][I == 1]
  rais_ano <- rais_ano[, .(tipo_estab, id_estab, cnpj_raiz, clas_cnae20, tamestab, uf, codemun, cpf, idade, genero, raca_cor, grau_instr,
                           tipo_vinculo, data_adm, data_deslig, horas_contr, salario,ind_horas_extras)]
  
  rais_ano[, ano := ano_rais]
  
  rais_pes <- rbindlist(list(rais_pes, rais_ano), fill = T)
  
}

dic_instr.file <- '//storage4/bases/RESTRITO/aval_MCMV/data/RAIS/conversao_anos_educacao.csv'
dic_instr <- fread(dic_instr.file)
dic_instr[, grau_instr_desc := grau_instr_desc %>% as.factor]

rais.m <- dic_instr[rais.m, on = 'grau_instr']



rais_pes[, .N, .(cpf, ano)]

histogram_data <- rais_pes[, .N, .(cpf, ano)][, .('num_anos' = .N), cpf]
histogram_data[, .N, num_anos][order(num_anos)]
histogram_data %>% ggplot(aes(x = num_anos)) + geom_bar()

instr_graph <- rais_pes[, .N, .(ano, grau_instr)][order(ano, grau_instr)][!is.na(ano)]
instr_graph[, total_ano := sum(N), ano]
instr_graph[, prop_instr := N/total_ano]
instr_graph[, grau_instr := grau_instr %>% as.factor()]
instr_graph %>% ggplot(aes(x = ano, y = prop_instr, color = grau_instr)) + geom_line(size = 1)

rais_pes[, mean(grau_instr), ano]

rais_pes[, .N, .(id_estab, cpf)]
rais_pes[, .N, .(id_estab, cpf)][, .N, cpf]
rais_pes[, .N, .(id_estab, cpf)][, .('num_empresas' = .N), cpf][, .N, num_empresas][order(num_empresas)]

rais_pes[, .N, .(tipo_vinculo)][order(tipo_vinculo)]





